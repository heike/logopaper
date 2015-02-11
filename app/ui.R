library(shiny)

fluidPage(
    
    titlePanel("GGLogo Prototype"),
    
    sidebarLayout(
        sidebarPanel(
            textInput("sequence", label = "Sequence", value = "RWTHLASGRT"),
            actionButton("addseq", "Add Sequence")
        ),
        
        mainPanel(
            #textOutput("sequences"),
            hr(),
            plotOutput("logoplot")
        )
    )
)
