library(shiny)

shinyUI(pageWithSidebar(headerPanel("Test"), sidebarPanel(sliderInput("mu", 
    "Value of μ", value = 210, min = 10, max = 2000, step = 10), 
    sliderInput("sigma", "Value of σ", value = 20, min = 0.1, 
        max = 198.1, step = 4.4), sliderInput("tau", "Value of τ (= 1/λ)", 
        value = 40, min = 1, max = 80, step = 2.72413793103448), 
    sliderInput("numbr", "Number of RTs", value = 6, min = 2L, 
        max = 12L, step = 1L), helpText("number of trials = 2^n \n4, 8, 16, 32, 64, 128, 256, 512, 1024  etc"), 
    sliderInput("nbins", "Number of histogram bins", value = 20, 
        min = 10, max = 100, step = 10), selectInput("color", 
        "Histogram color", choices = c("red", "green", "blue", 
        "orange", "yellow", "pink", "cyan", "magenta", "purple", 
        "gray", "transparent"))), mainPanel(h4("plot Histogram of Response Times"), 
    plotOutput("plot.Histogram.of.Response.Times"), h4("Note"), 
    verbatimTextOutput("Note"), h4("Parameter estimation"), verbatimTextOutput("Parameter.estimation"))))
