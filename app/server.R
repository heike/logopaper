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
    
    output$plotbuilt <- reactive({
        return(length(values$seqs) > 0)
    })
    outputOptions(output, 'plotbuilt', suspendWhenHidden=FALSE)
    
    myplot <- reactive({
        if (length(values$seqs) == 0) return(NULL)
        
        withProgress(message = "Building logo plot, please wait...", expr = {
            dm2 <- splitSequence(as.data.frame(values$seqs), "peptide")
            cols <- c("grey80", brewer.pal(3,"RdYlBu"))
            
            dm3 <- calcInformation(dm2, pos="position", elems="element", k=21)
            dm3b <- merge(dm3, aacids, by.x="element", by.y="AA", all.x=T)
            
            ggplot(dm3b, aes(x=position, y=bits, group=element, label=element, fill=Polarity), alpha=0.8) + 
                geom_hline(yintercept=-log(1/21, base=2), colour="grey30", size=0.5) + 
                geom_logo() + 
                scale_fill_manual("Polarity", values=cols) +  
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
