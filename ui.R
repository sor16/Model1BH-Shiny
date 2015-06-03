library(shiny)

shinyUI(fluidPage(
    titlePanel('Rating curve'),
    sidebarLayout(
        sidebarPanel(
            fileInput('file1', 'Choose file'),
            br(),
            selectInput("select", label = "Select country", choices = list("Iceland" = 'Iceland'), 
                        selected = 'Iceland'),
            br(),
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
            textOutput('callreactive'),
            conditionalPanel(condition="'tafla' %in% input$checkbox",tableOutput('tafla'))
           
            
            
            
            
        )
    )
))
