library(shiny)

shinyUI(fluidPage(
    titlePanel('Rating Curve Generator'),
    sidebarLayout(
        sidebarPanel(
            selectInput("select", label = "Choose country", choices = list("Iceland" = 'Iceland'), 
                        selected = 'Iceland'),
            br(),
            textInput("name","Name of river"),
            br(),
            fileInput('file1', 'Choose file'),
            checkboxGroupInput("checkbox", label = "Output",choices=list("Real scale"="raun","Logarithmic scale"="log",
                                                                         "Real scale residuals"="leifraun","Standardized residuals"="leiflog") ,selected = NULL),
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
                
                tabPanel('Plots',  uiOutput('plots')),
                       
                tabPanel('Numeric summary', uiOutput('tafla'))
            ),
        tagList(
            tags$head(
                tags$link(rel="stylesheet", type="text/css",href="style.css"),
                tags$script(type="text/javascript", src = "busy.js")
            )
        ),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        div(class = "busy",
            img(src="progress.GIF")
        )
        )
    )
))
