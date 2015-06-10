library(shiny)

shinyUI(fluidPage(
    titlePanel('Rating curve'),
    sidebarLayout(
        sidebarPanel(
            selectInput("select", label = "Select country", choices = list("Iceland" = 'Iceland'), 
                        selected = 'Iceland'),
            br(),
            textInput("name","Name of river"),
            br(),
            fileInput('file1', 'Choose file'),
            checkboxGroupInput("checkbox", label = "Output",choices=list("Raunskali"="raun","Lograskali"="log",
                                                                         "Leifarit a raunskala"="leifraun","Leifarit a logskala"="leiflog") ,selected = NULL),
            actionButton("go", label="Submit"),
            br(),
            br(),
            downloadButton('downloadReport',label="Download as PDF")
            
        ),
        #list of outputs
        mainPanel(
        textOutput('callreactive'),
        
            tabsetPanel(
                id = 'dataset',
                
                tabPanel('Plot summary',  uiOutput('plots')),
                       
                tabPanel('Numeric summary', uiOutput('tafla'))
            )
        )
    )
))
