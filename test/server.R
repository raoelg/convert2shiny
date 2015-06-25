library(shiny)

 source("../exgauss/exgauss_routines.R") 



shinyServer(function (input, output) 
{
    X..These.are.computations.that.need.to.be.done.whenever.input.values.change.but.don.t.produce.visible.output <- {
    }
    X..Each.computation.has.to.start.with.a.comment..followed.by.statements.which.include.assignments.to.new.variable.names <- {
    }
    X..generate.RTs <- reactive({
        (RTs = rexgauss(2^input$numbr, input$mu, input$sigma, 
            1/input$tau))
    })
    output$plot.Histogram.of.Response.Times <- renderPlot({
        hist(X..generate.RTs(), input$nbins, freq = FALSE, col = input$color, 
            main = "", xlab = "time")
        curve(dexgauss(x, input$mu, input$sigma, 1/input$tau), 
            par("usr")[1], par("usr")[2], lwd = 3, col = "darkgreen", 
            add = TRUE)
        fit = fitExgauss(X..generate.RTs())
        curve(dexgauss(x, fit$par[1], fit$par[2], fit$par[3]), 
            par("usr")[1], par("usr")[2], lwd = 3, col = 3, lty = 2, 
            add = TRUE)
    })
    output$Note <- renderPrint({
        cat("This plot is a histogram of the simulated response times, with the true and estimated exgauss pdf superimposed. The dark green continuous line is true pdf, light green dashed line is estimated pdf.")
    })
    output$Parameter.estimation <- renderPrint({
        fit = fitExgauss(X..generate.RTs())
        fit
    })
})
