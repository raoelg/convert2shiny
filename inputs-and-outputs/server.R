library(shiny)

shinyServer(function (input, output) 
{
    X..get.the.data.frame..redefine.am.for.nicer.output <- {
        library(datasets)
        mpgData <- transform(mtcars, am = factor(am, labels = c("Automatic", 
            "Manual")))
    }
    X..compute.formula.text <- reactive({
        (formulaText <- paste("mpg ~", input$variable))
    })
    output$show.Caption <- renderText({
        X..compute.formula.text()
    })
    output$plot.boxplot.of.mpgData <- renderPlot({
        boxplot(as.formula(X..compute.formula.text()), data = mpgData, 
            outline = input$outliers)
    })
})
