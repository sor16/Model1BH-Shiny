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
                                                                         "Leifarit a raunskala"="leifraun","Leifarit a logskala"="leiflog") ,selected = NULL),
            actionButton("go", label="Submit"),
            br(),
            br(),
            textInput("name","Name of river"),
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
