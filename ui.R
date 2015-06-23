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
           
            br(),
            checkboxGroupInput("checkbox2", label = "Models",choices=list("Model 1"= "mdl1", "Model 2"="mdl2"), inline=TRUE),
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
                
                #conditionalPanel(condition= "output.plots != NULL",
                                   
                tabPanel('Plots',  uiOutput('plots')),
                       
                tabPanel('Numeric summary', uiOutput('tafla'))
               # ),
                
#                 conditionalPanel(condition= "input.checkbox2 == mdl2",
#                                
#                                tabPanel('Plots2',  uiOutput('plots')),
#                                
#                                tabPanel('Numeric summary 2', uiOutput('tafla'))
#                 )
#                 
                
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
