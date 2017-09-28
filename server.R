library(shiny)
library(ggvis)
library(dplyr)
library(RSQLite)
library(dbplyr)

# Database tables
seasons.df <- read.csv("Seasons_Stats.csv", stringsAsFactors = FALSE)

seasons <- tbl_df(seasons.df) %>% rename(ID = X) #rename first column "ID"

# Join tables, filtering out those with <10 reviews, and select specified columns

seasons <- mutate(seasons, PPG = PTS/G)
seasons$Player <- sapply(seasons$Player, function(x) {gsub("\\*", "", x)}) # remove * at end of player's name

function(input, output, session) {
  
  # Filter the movies, returning a data frame
  seasons_r <- reactive({

    # recovering filter ranges
    minx_str <- paste0("input$", as.symbol(input$xvar), "[1]")
    minx <- eval(parse(text = minx_str)) # trick to force evaluation of input$Age[1] (where Age is the x variable)
    maxx_str <- paste0("input$", as.symbol(input$xvar), "[2]")
    maxx <- eval(parse(text = maxx_str))
    
    miny_str <- paste0("input$", as.symbol(input$yvar), "[1]")
    miny <- eval(parse(text = miny_str))
    maxy_str <- paste0("input$", as.symbol(input$yvar), "[2]")
    maxy <- eval(parse(text = maxy_str))
    
    seasons_1 <- seasons
    # Apply filters
    if (!is.null(minx) & !is.null(maxx) & !is.null(miny) & !is.null(maxy)) {
      cat("applying filters beginning...\n")
      print(seasons[seasons$ID==1706, c("Player")])
      cat("dim seasons", dim(seasons), "\n")
      cat(minx, maxx, miny, maxy)
      seasons_1 <- seasons %>% 
        filter(eval(parse(text = input$xvar)) >= minx, # need to convert "Year" to Year
               eval(parse(text = input$xvar)) <= maxx,
               eval(parse(text = input$yvar)) >= miny,
               eval(parse(text = input$yvar)) <= maxy) 
      print(seasons_1[seasons_1$ID==1706, c("Player")])
      cat("dim seasons", dim(seasons_1))
      cat("...applying filters finished\n")
    }
    cat(dim(seasons_1))
    #seasons <- as.data.frame(seasons)
    print(seasons[seasons$ID==1706, c("Player")])
    seasons_1
  })
  
  # Function for generating tooltip text
  player_tooltip <- function(x) {
    print(str(x))
    cat(names(x)[1], names(x)[2], names(x)[3])
    cat(x[[1]], x[[2]], x[[3]])
    cat("x= ", x$ID)
    if (is.null(x)) return(NULL)
    if (is.null(x$ID)) return(NULL)
    
    # Pick out the player with this ID
    seasons_2 <- isolate(seasons_r())
    player <- seasons_2[seasons_2$ID == x$ID, "Player"]
    
    # Format for tooltip text:
    paste0("<b>", player, "</b><br>",
           names(x)[2], ": ", format(round(x[[2]], 2), nsmall = 2), "<br>",
           names(x)[1], ": ", x[[1]])
  }
  
  # Dynamic filters
  output$xfilter <- renderUI({
    
    seasons_3 <- isolate(seasons_r())
    
    # preparing data for sliderInput
    xvar_str <- as.character(input$xvar)
    min_val <- round(min(seasons_3[,c(xvar_str)], na.rm = T)) # round avoids decimals
    max_val <- ceiling(max(seasons_3[,c(xvar_str)], na.rm = T)) # avoid removing outliers with ceiling
    
    sliderInput(inputId = xvar_str, 
                label = paste0(xvar_str, " Filter"), 
                min_val, 
                max_val, 
                value=c(min_val,max_val), 
                step=5, 
                round=-1, # this should limit to 1 decimal?
                sep = "")
  })
  
  output$yfilter <- renderUI({
    
    seasons_4 <- isolate(seasons_r())
    
    # preparing data for sliderInput
    yvar_str <- as.character(input$yvar)
    min_val <- round(min(seasons_4[,c(yvar_str)], na.rm = T)) # round avoids decimals
    max_val <- ceiling(max(seasons_4[,c(yvar_str)], na.rm = T))

    sliderInput(yvar_str, paste0(yvar_str, " Filter"), 
                min_val, 
                max_val, 
                value=c(min_val,max_val), 
                step=5, 
                round=-1, # this should limit to 1 decimal? 
                sep = "")
  })
  
  # A reactive expression with the ggvis plot
  vis <- reactive({
    # Lables for axes
    xvar_name <- input$xvar
    yvar_name <- input$yvar
    
    # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
    # but since the inputs are strings, we need to do a little more work.
    xvar <- prop("x", as.symbol(input$xvar))
    # cat(str(xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    
    seasons_r() %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_points(size := 50, size.hover := 200,
                   fillOpacity := 0.2, fillOpacity.hover := 0.5, key := ~ID) %>% # 'key' makes ID available for hover
      add_tooltip(player_tooltip, "hover") %>%
      add_axis("x", title = xvar_name, format="####", ticks = 5) %>% # format to keep round numbers
      add_axis("y", title = yvar_name) %>%
      add_legend("stroke", title = "NBA Champion", values = c("Yes", "No")) %>%
      scale_nominal("stroke", domain = c("Yes", "No"),
                    range = c("orange", "#aaa")) %>%
      set_options(width = 500, height = 500)
  })
  
  vis %>% bind_shiny("plot1")
}
