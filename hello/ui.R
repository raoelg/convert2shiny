library(shiny)

shinyUI(pageWithSidebar(headerPanel("Hello Shiny"), sidebarPanel(sliderInput("obs", 
    "Number of observations:", value = 500, min = 1L, max = 1000L, 
    step = 1L)), mainPanel(h4("plot histogram of dist"), plotOutput("plot.histogram.of.dist"))))
