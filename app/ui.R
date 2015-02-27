library(shiny)

## Legend with letters, color, name
## Choice of color scheme ala weblogo

fluidPage(
    
    titlePanel("GGLogo Prototype"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Input a list of sequences, separated by commas"),
            
            tags$textarea(id="sequence", rows=3, cols=40, "RWTHLASGRT,RWLSLBSGRT,RWTHLSSGRT"),
            
            hr(),
            
            actionButton("confirm", "Build Logo")
        ),
        
        mainPanel(
            #textOutput("sequences"),
            hr(),
            plotOutput("logoplot")
        )
    )
)
