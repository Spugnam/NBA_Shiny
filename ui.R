library(shiny)
library(ggvis)

# For dropdown menu
actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}
fluidPage(
  titlePanel("NBA explorer"),
  fluidRow(
    column(3,
           wellPanel(
             h4("GOATs"),
             uiOutput("Goats1"),
             uiOutput("Goats2"),
             uiOutput("Goats3"),
             uiOutput("Goats4")
           ),
           wellPanel(
             h4("Graph"),
             uiOutput("xfilter"),
             selectInput("xvar", "X-axis variable", 
                         list(`Choose Metric` = c("Year", "Age", "G", "MP")), 
                         selected = "Year"),
             uiOutput("yfilter"),
             selectInput("yvar", "Y-axis variable", 
                         list(`Choose Metric`=c("PPG", "PER", "BPM", "VORP", "AST", "STL", "BLK", "ORB", "DRB")), 
                         selected = "PPG")
           )
    ),
    column(9,
           ggvisOutput("plot1")
    )
  )
)