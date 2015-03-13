library(shiny)
library(shinythemes)

## Legend with letters, color, name
## Choice of color scheme ala weblogo

fluidPage(theme = shinytheme("united"),
    
    titlePanel("GGLogo Prototype"),
    
    sidebarLayout(
        sidebarPanel(
            h4("Sequence"),
            
            helpText("Input a list of sequences, separated by commas"),
            
            tags$textarea(id="sequence", rows=3, cols=40, "RWTHLASGRT,RWLSLBSGRT,RWTHLSSGRT"),

            actionButton("confirm", "Build Logo Plot"),
            
            conditionalPanel(condition = "output.plotbuilt == true",
                downloadButton("download", "Download Logo Plot")
            ),
            
            hr(),
            
            checkboxInput("advanced", "Show Advanced Configuration"),
            
            conditionalPanel(condition = "input.advanced == true",
                h4("Advanced Configuration"),
                
                h5("Plot Colors"),
                textInput("colgrp1", "Group 1", "AILMFPWV"),
                textInput("name1", "Name 1", "Non-Polar"),
                textInput("col1", "Color 1", "grey80"),
                helpText("-----"),
                
                textInput("colgrp2", "Group 2", "RHK"),
                textInput("name2", "Name 2", "Basic"),
                textInput("col2", "Color 2", "#FC8D59"),
                helpText("-----"),
                
                textInput("colgrp3", "Group 3", "NCQGSTY"),
                textInput("name3", "Name 3", "Neutral"),
                textInput("col3", "Color 3", "#FFFFBF"),
                helpText("-----"),
                
                textInput("colgrp4", "Group 4", "DE"),
                textInput("name4", "Name 4", "Acidic"),
                textInput("col4", "Color 4", "#91BFDB"),
                helpText("-----"),
                                
                h5("Plot Labels"),
                textInput("title", "Plot Title", value = ""),
                textInput("xlab", "X Axis Label", value = "position"),
                textInput("ylab", "Y Axis Label", value = "bits"),
                
                hr(),
                
                h5("Download Options"),
                selectizeInput("image_format", "Image Format", choices = c("PNG" = "png", "JPG" = "jpg", "PDF" = "pdf")),
                numericInput("dpi", "DPI", value = 300, min = 1, max = 1000)
            )
        ),
        
        mainPanel(
            #textOutput("sequences"),
            hr(),
            plotOutput("logoplot")
        )
    )
)
