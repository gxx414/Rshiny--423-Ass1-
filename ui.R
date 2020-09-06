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


# DATA423 - Data Science in Industry
# Assignment1 - Xiaoxi Guo
# UI

domChoices <- c("l","f","r","t","i","p")
shinyUI(
    fluidPage(

        titlePanel(h2("Assignment 1 - Xiaoxi Guo")),
        h3("Dataset - Ass1Data",hr()),
        
        tabsetPanel(
            tabPanel(h4("DATA",hr()),
                     tabsetPanel(
                         tabPanel(
                                  h4("Datatable"),
                                  DT::dataTableOutput(outputId = "dat"),
                                  sidebarLayout(
                                      sidebarPanel(
                                          checkboxInput("rownames", "Show row names", value=T),
                                          checkboxInput("order", "Column ordering", value=T),
                                          selectInput("selection", "Selection type", choices=c("none","single","multiple"), selected = "none"),
                                          selectInput("filter", "Filter type", choices=c("none","top","bottom"), selected = "none"),
                                          selectInput("dom", "DOM", choices=domChoices, multiple = TRUE, selected=domChoices)
                                      ),
                                      
                                      mainPanel(
                                          DT::dataTableOutput("tableX"),
                                          tableOutput("SelRows")       
                                      )
                                  )
                         ),
                         
                         tabPanel(h4("Summary"),
                                  verbatimTextOutput(outputId = "SummaryA1")
                         ),
                         
                         tabPanel(h4("DfSummary"),
                                  withSpinner(tableOutput(outputId = "SummaryA2")
                         ))
                     )
        ),
        
            tabPanel(h4("VISUALISATION",hr()),
                   tabsetPanel(
                       tabPanel(h4("Mosaic"),
                                selectizeInput(inputId = "VariablesA", label = "Show variables:", choices = choicesA, multiple = T, selected = choicesA[1:2]),
                                withSpinner(plotOutput(outputId = "Mosaic"))
                       ),

                       tabPanel(h4("GGpairs"),
                            checkboxGroupInput(inputId = "VariablesB", label = "Variables:", choices = choicesB, selected = choicesB[1:2], inline = T),
                            shinycssloaders::withSpinner(
                                plotOutput(outputId = "Pairs")
                            )
                      ),
                      
                      tabPanel(h4("Correlations"),
                               checkboxGroupInput(inputId = "VariablesC", label = "Variables:", choices = choicesB, selected = choicesB[1:3], inline = T),
                               withSpinner(
                                   plotOutput(outputId = "Corrgram")
                               ),
                               checkboxInput(inputId = "abs", label = "Uses absolute correlation", value = TRUE),
                               selectInput(inputId = "CorrMeth", label = "Correlation method", choices = c("pearson","spearman","kendall"), selected = "pearson"),
                               selectInput(inputId = "Group", label = "Grouping method", choices = list("none"=FALSE,"OLO"="OLO","GW"="GW","HC"="HC"), selected = "OLO")
                      ),
                      
                      tabPanel(h4("Missing-value Chart"),
                               withSpinner(
                               plotOutput(outputId = "Missing")
                               ),
                               checkboxInput(inputId = "cluster", label = "Cluster missingness", value = FALSE)
                            
                      ),
                      
                      tabPanel(h4("Boxplot"),
                               checkboxGroupInput(inputId = "VariablesD", label = "Variables:", choices = choicesB, selected = choicesB[1:3], inline = T),
                               withSpinner(
                                   plotOutput(outputId = "Boxplot")
                               ),
                               checkboxInput(inputId = "standardise", label = "Show standardized", value = FALSE),
                               checkboxInput(inputId = "outliers", label = "Show outliers", value = TRUE),
                               sliderInput(inputId = "range", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5)

                      ),
                      
                      
                      tabPanel(h4("Rising-value Chart"),
                               checkboxGroupInput(inputId = "VariablesE", label = "Variables:", choices = choicesB, selected = choicesB[1:3], inline = T),
                               withSpinner(
                                   plotOutput(outputId = "Risingvalue")
                               )

                      )
                   ))
            )
        )
    )

