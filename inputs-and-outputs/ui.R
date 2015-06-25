library(shiny)

shinyUI(pageWithSidebar(headerPanel("mpg"), sidebarPanel(radioButtons("variable", 
    "Variable:", choices = c("cyl", "am", "gear")), checkboxInput("outliers", 
    "Show outliers?", value = FALSE)), mainPanel(h4("show Caption"), 
    verbatimTextOutput("show.Caption"), h4("plot boxplot of mpgData"), 
    plotOutput("plot.boxplot.of.mpgData"))))
