library(shiny)

shinyUI(pageWithSidebar(headerPanel("Multiple Regression Analysis"), 
    sidebarPanel(fileInput("datafile", "Choose a CSV file to upload", 
        accept = c("text/csv", "text/plain")), uiOutput("Specify.the.dependent.variable"), 
        uiOutput("Select.independent.variables")), mainPanel(h4("tabulate Listing of the data read"), 
        tableOutput("tabulate.Listing.of.the.data.read"), h4("print Model"), 
        textOutput("print.Model"), h4("dump Summary of the linear (multiple) regression result"), 
        verbatimTextOutput("dump.Summary.of.the.linear..multiple..regression.result"), 
        h4("plot Diagnostics"), plotOutput("plot.Diagnostics"))))
