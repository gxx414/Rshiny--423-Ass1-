# DATA423 - Data Science in Industry
# Assignment1 - Xiaoxi Guo
# Global

library(shiny)
library(summarytools)
library(shinycssloaders)
library(vcd)
library(corrgram)
library(visdat)
library(ggplot2)
library(DT)
library(RColorBrewer)
library(car)

dat <- read.csv("Ass1Data.csv", header = TRUE)
attach(dat)
dat$Date = as.Date(dat$Date)
choices = colnames(as.data.frame(dat))
choicesA = choices[3:14][-2]
choicesB = choices[c(1,15:44)] 


# Server

shinyServer(function(input, output, session) {
    
    output$tableX <- DT::renderDataTable({
        DT::datatable(data = dat,
                      rownames = input$rownames,
                      selection = input$selection,
                      filter = list(position = input$filter),
                      options = list(searching = TRUE,
                                     pageLength = 10,
                                     lengthMenu = c(5, 10, 100),
                                     dom = paste(input$dom, collapse = ""),
                                     ordering = input$order
                      )
                      
        )  %>%
            formatStyle(columns = c("ID"), backgroundColor = "skyblue")  %>%
            formatRound(c("Y"), 3)
    })
    
    output$SelRows <- renderTable({
        req(input$tableX_rows_selected)
        print(dat[input$tableX_rows_selected,"ID"])
    })
    
    output$SummaryA1 <- renderPrint({
        summary(dat)
    })
    
    output$SummaryA2 <- renderPrint({
        print(dfSummary(dat), method = 'render')
    })
    
    output$Mosaic <- renderPlot({
        formula <- as.formula(paste("~",paste(input$VariablesA, collapse = " + ")))
        vcd::mosaic(formula, data = dat,
                    main = "Frequency Novelties", shade = TRUE, legend = TRUE)
    })
    
    output$Pairs <- renderPlot({
        variable = input$VariablesB
        GGally::ggpairs(data = dat[,variable], title = "Pairs of Ass1Data")
    })
    
    output$Corrgram <- renderPlot({
        variable = input$VariablesC
        corrgram::corrgram(x = dat[,variable],
                 order = input$Group,
                 abs = input$abs,
                 cor.method = input$CorrMeth,
                 text.panel = panel.txt,
                 main = "Correlation of Ass1Data")
    })
    
    output$Missing <- renderPlot({
        vis_miss(dat, cluster = input$cluster) +
            labs(main = "Missingness of Airquality data")
    })
    
    output$Boxplot <- renderPlot({
        variable = input$VariablesD
        data <- as.matrix(dat[,variable])
        data <- scale(data, center = input$standardise, scale = input$standardise)
        car::Boxplot(formula = ~., data = data, use.cols = TRUE, notch = FALSE, varwidth = FALSE,
                horizontal = FALSE, outline = input$outliers, ylab = NA, las = 2,
                col = brewer.pal(n = dim(dat), name = "RdBu"),
                range = input$range, main = "Boxplots of Ass1Data")
    })
    
    output$Risingvalue <- renderPlot({
        variable = input$VariablesE
        d <- dat[,variable]
        for (col in 1:ncol(d)) {
            d[,col] <- d[order(d[,col]),col] 
        }
        d <- scale(x = d, center = TRUE, scale = TRUE) 
        mypalette <- rainbow(ncol(d))
        matplot(x = seq(1, 100, length.out = nrow(d)), y = d, type = "l", xlab = "Percentile (%)", ylab = "Standardised Values", lty = 1, lwd = 1, col = mypalette, main = "Rising value chart")
        legend(legend = colnames(d), x = "topleft", y = "top", lty = 1, lwd = 1, col = mypalette, ncol = round(ncol(d)^0.4))
    })
})

