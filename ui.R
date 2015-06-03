library(shiny)
library(ggplot2)
Sys.setlocale("LC_ALL", "is_IS")

shinyUI(fluidPage(
    titlePanel('Linear Regression'),
    sidebarLayout(
        sidebarPanel(
            fileInput('file1', 'Choose file'),
            checkboxGroupInput("checkbox", label = "Output",choices=list("Raunskali"="raun","Lograskali"="log",
                                                                         "Leifarit a raunskala"="leifraun","Leifarit a logskala"="leiflog", "Tafla"="tafla") ,selected = NULL),
            actionButton("go", label="Submit"),
            br(),
            br(),
            textInput("nafn","Name of river"),
            radioButtons('format','Document format',c('PDF','HTML','Word'),inline=TRUE),
            downloadButton('downloadReport')
            
        ),
        #list of outputs
        mainPanel(
            uiOutput('plots'),
            #textOutput('text'),
            conditionalPanel(condition="'tafla' %in% input$checkbox",tableOutput('tafla'))
           
            
            
            
            
        )
    )
))
