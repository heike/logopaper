library(shiny)

fluidPage(
    
    titlePanel("GGLogo Prototype"),
    
    sidebarLayout(
        sidebarPanel(
        ),
        
        mainPanel(
            plotOutput("logoplot")
        )
    )
)
