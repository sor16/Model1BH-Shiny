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
            checkboxInput("checkboxA", label=strong("Advanced settings"), value=FALSE),
            br(), 
            conditionalPanel(condition="input.checkboxA == true",   sliderInput("slider", label = "Date Range", min = 1950, max = as.numeric(format(Sys.Date(), "%Y")), 
                                                                           value=c(1950,as.numeric(format(Sys.Date(), "%Y"))))),
            
            checkboxGroupInput("checkbox2", label = "Method",choices=list("Model 1"='mdl1', "Model 2"='mdl2'),inline=TRUE),
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
            
        tabPanel('Plots',uiOutput('plots'),verbatimTextOutput('hover_info')),
        tabPanel('Numeric summary', uiOutput('tafla')),
        tabPanel('Plots2',  uiOutput('plots2')),
        tabPanel('Numeric summary 2', uiOutput('tafla2'))
        
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
