library(shiny)

shinyServer(function (input, output) 
{
    X..return.the.requested.dataset <- reactive({
        library(datasets)
        (data = switch(input$dataset, rock = rock, pressure = pressure, 
            cars = cars))
    })
    output$tabulate.the.first.rows.of.the.data.set <- renderTable({
        head(X..return.the.requested.dataset(), n = input$obs)
    })
})
