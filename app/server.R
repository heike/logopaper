library(shiny)
library(gglogo)
library(RColorBrewer)
library(ggplot2)
library(grid)

data(sequences)
data(aacids)

function(input, output) {
    
    values <- reactiveValues(seqs = list())
    
    observeEvent(input$confirm, {
        values$seqs <<- list(peptide = strsplit(input$sequence, split = ",")[[1]])
    })
    
    ## The initial uploaded dataset
    seqs.initial <- reactive({
        if (is.null(input$data)) return(NULL)
        else return(read.csv(input$data$datapath))
    })
    
    output$plotbuilt <- reactive({
        return(length(values$seqs) > 0)
    })
    outputOptions(output, 'plotbuilt', suspendWhenHidden=FALSE)
    
    mydf <- reactive({
        mydf1 <- data.frame(AA = strsplit(input$colgrp1, "")[[1]], Color = input$col1)
        mydf2 <- data.frame(AA = strsplit(input$colgrp2, "")[[1]], Color = input$col2)
        mydf3 <- data.frame(AA = strsplit(input$colgrp3, "")[[1]], Color = input$col3)
        mydf4 <- data.frame(AA = strsplit(input$colgrp4, "")[[1]], Color = input$col4)
        
        rbind(mydf1, mydf2, mydf3, mydf4)
    })
    
    myplot <- reactive({
        if (length(values$seqs) == 0) return(NULL)
        
        withProgress(message = "Building logo plot, please wait...", expr = {
            dm2 <- splitSequence(as.data.frame(values$seqs), "peptide")
            cols <- c(input$col1, input$col2, input$col3, input$col4)
            
            dm3 <- calcInformation(dm2, pos="position", elems="element", k=21)
            dm3b <- merge(dm3, aacids, by.x="element", by.y="AA", all.x=T)
            dm3bb <- merge(dm3b, mydf(), by.x = "element", by.y = "AA", all.x = T)
            
            ggplot(dm3bb, aes(x=position, y=bits, group=element, label=element, fill=Color), alpha=0.8) + 
                geom_hline(yintercept=-log(1/21, base=2), colour="grey30", size=0.5) + 
                geom_logo() + 
                scale_fill_manual("Polarity", values=cols, labels = c(input$name1, input$name2, input$name3, input$name4)) +  
                theme_bw() + theme(legend.position="bottom") + 
                xlab(input$xlab) +
                ylab(input$ylab) + 
                ggtitle(input$title) +
                theme(plot.margin=unit(c(0,0,0,0), "cm")) + 
                geom_hline(yintercept=0, colour="white", size=0.5) + 
                geom_hline(yintercept=0, colour="grey30", size=0.125) + 
                scale_y_continuous(breaks=c(-1,0,1,2,4), labels=c(1,0,1,2,4))
        })
    })
    
    output$logoplot <- renderPlot({
        print(myplot())
    })
    
    output$download <- downloadHandler(
        filename = function() { paste("logoplot", input$image_format, sep = ".") },
        content = function(file) {
            ggsave(file, plot = myplot(), dpi = input$dpi)
        }
    )
    
}
