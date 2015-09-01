library(shiny)
library(gglogo)
library(RColorBrewer)
library(ggplot2)
library(grid)
library(plyr)
library(dplyr)
library(seqinr)

data(sequences)
data(aacids)

# Hook into amino acid package to select option for polarity

function(input, output, session) {
    
    values <- reactiveValues(seqs = "")
    
    observeEvent(input$confirm, {
        values$seqs <<- input$sequence
    })
    
    ## The initial uploaded dataset
    seqs.initial <- reactive({
        if (is.null(input$data)) return(NULL)
        else return(read.fasta(input$data$datapath))
    })
    
    output$plotbuilt <- reactive({
        return(nrow(my.seqdata()) > 0)
    })
    outputOptions(output, 'plotbuilt', suspendWhenHidden=FALSE)
    
    mydf <- reactive({
        mydf1 <- data.frame(AA = strsplit(input$colgrp1, "")[[1]], Color = input$col1)
        mydf2 <- data.frame(AA = strsplit(input$colgrp2, "")[[1]], Color = input$col2)
        mydf3 <- data.frame(AA = strsplit(input$colgrp3, "")[[1]], Color = input$col3)
        mydf4 <- data.frame(AA = strsplit(input$colgrp4, "")[[1]], Color = input$col4)
        
        rbind(mydf1, mydf2, mydf3, mydf4)
    })
    
    my.df <- reactive({
        if (nchar(values$seqs) == 0 && is.null(seqs.initial())) return(NULL)
        
        test <- seqs.initial()
        mydf <- ldply(test, function(my.seq) {
            paste(toupper(as.character(my.seq)), collapse = "")
        })
        names(mydf) <- c("factor", "peptide")
        
        return(mydf)
    })
    
    observe({
        if (!is.null(my.df())) {
            my.df <- my.df()
            updateSliderInput(session, "zoom", max = nchar(as.character(my.df$peptide[1])), value = c(1, min(30, nchar(as.character(my.df$peptide[1])))))
        }
    })
    
    my.seqdata <- reactive({
        if (is.null(my.df())) return(NULL)
        my.df <- my.df()
        my.df$peptide <- sapply(strsplit(my.df$peptide, ""), function(x){paste(x[input$zoom[1]:input$zoom[2]], collapse = "")})
        
        return(my.df)
    })
    
    myplot <- reactive({
        if (is.null(my.seqdata())) return(NULL)
        
        withProgress(message = "Building logo plot, please wait...", expr = {
            test <- my.seqdata()
            
            dm2 <- splitSequence(test, "peptide")
            cols <- c(input$col1, input$col2, input$col3, input$col4)
            
            my.trt <- if (input$facetvar == "Factor") "factor" else NULL
            
            dm3 <- calcInformation(dm2, trt = my.trt, pos="position", elems="element", k=21)
            dm3b <- merge(dm3, aacids, by.x="element", by.y="AA", all.x=T)
            dm3bb <- merge(dm3b, mydf(), by.x = "element", by.y = "AA", all.x = T)
            
            #dm3bb$facet_group <- cut(as.numeric(dm3bb$position), seq(0, max(as.numeric(dm3bb$position)) + 29, by = 30), labels = FALSE)
            
            dm3bb$x_var <- if (input$facetvar == "Factor") dm3bb$factor else dm3bb$position
            my_text <- element_text(angle = ifelse(input$facetvar == "Factor", 90, 0), vjust = ifelse(input$facetvar == "Factor", 0.5, 1))
            
            ggplot(dm3bb, aes(x = x_var, y = bits, group = element, label = element, fill = Color), alpha = 0.8) + 
                geom_hline(yintercept=-log(1/21, base=2), colour="grey30", size=0.5) + 
                geom_logo() + 
                scale_fill_manual("Polarity", values=cols, labels = c(input$name1, input$name2, input$name3, input$name4)) +  
                geom_hline(yintercept=0, colour="white", size=0.5) + 
                geom_hline(yintercept=0, colour="grey30", size=0.125) + 
                theme_bw() + theme(legend.position="bottom", plot.margin=unit(c(0,0,0,0), "cm"), axis.text.x = my_text) + 
                xlab(input$xlab) +
                ylab(input$ylab) + 
                ggtitle(input$title) +
                scale_y_continuous(breaks=c(-1,0,1,2,4), labels=c(1,0,1,2,4)) +
                if (input$facetvar == "Factor") facet_wrap(~position)
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
