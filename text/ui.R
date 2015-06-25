library(shiny)

shinyUI(pageWithSidebar(headerPanel("Shiny Text"), sidebarPanel(selectInput("dataset", 
    "Choose the dataset:", choices = c("rock", "pressure", "cars"
    )), numericInput("obs", "Number of observations to view:", 
    value = 10)), mainPanel(h4("tabulate the first rows of the data set"), 
    tableOutput("tabulate.the.first.rows.of.the.data.set"))))
