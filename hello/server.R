library(shiny)

shinyServer(function (input, output) 
{
    output$plot.histogram.of.dist <- renderPlot({
        dist <- rnorm(input$obs)
        hist(dist)
    })
})
