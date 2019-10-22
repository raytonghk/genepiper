library(shiny)
library(tidyverse)
library(phyloseq)
library(shinyjs)

VENN_HEIGHT <- 700
VENN_WIDTH <- 700
VENN_MARGIN <- .2

shinyServer(
  function(input, output, session) {
    vals <- reactiveValues()
    
    output$plotListReady <- reactive(isTruthy(vals$plotList))
    outputOptions(output, "plotListReady", suspendWhenHidden = FALSE)
    
    source("../function_phyloseq.R")
    
    ### Load Data Panel
    source("../server_panel_load_data.R", local = TRUE)
    
    ### Filter Panel
    source("../server_panel_filter.R", local = TRUE)
    
    ### Parameter Panel
    source("../server_panel_parameter.R", local = TRUE)
    
    observe({
      req(vals$filteredPhyloseq)
      vals$plotList <- NULL
    })
    
    serverTaxRank()
    
    # groupColumnVenn
    observe({
      req(vals$filteredPhyloseq)
      updateSelectInput(session, "groupColumnVenn", choices = c("Sample", characterVariableNames(vals$filteredPhyloseq)))
    })
    
    # groupLabel
    observe({
      req(vals$filteredPhyloseq, input$groupColumnVenn == "Sample")
      choices <- sample_names(vals$filteredPhyloseq)
      updateCheckboxGroupInput(session, "groupLabel", choices = choices, selected = choices[1 : min(5, length(choices))])
    })
    
    observe({
      req(vals$filteredPhyloseq, input$groupColumnVenn, input$groupColumnVenn != "Sample")
      choices <- unique(get_variable(vals$filteredPhyloseq, input$groupColumnVenn))
      updateCheckboxGroupInput(session, "groupLabel", choices = choices, selected = choices[1 : min(5, length(choices))])
    })
    
    # plotButton
    observe({
      req(input$groupLabel)
      if(length(input$groupLabel) %in% 2:5) {
        output$groupLabelMessage <- renderText(NULL)
        enable("plotButton")
      } else {
        output$groupLabelMessage <- renderText(HTML("Please select 2 - 5 labels."))
        disable("plotButton")
      }
    })
    
    observeEvent(input$plotButton,
                 {
                   vals$plotMessage <- NULL
                   vals$plotList <- NULL
                   tryCatch(
                     {
                       req(vals$filteredPhyloseq, input$taxRank)
                       vals$plotList <- agglomerateTaxa(vals$filteredPhyloseq, input$taxRank) %>%
                         otuPlotTable() %>%
                         plotList()
                     },
                     error = function(e) {
                       vals$plotMessage <- "Error: Create plot list error."
                     }
                   )
                 }
    )
    
    plotList <- function(plotTable) {
      if(input$groupColumnVenn == "Sample") {
        plotListOfSample(plotTable)
      } else {
        plotListOfGroup(plotTable)
      }
    }
    
    plotListOfSample <- function(plotTable) {
      req(length(input$groupLabel) %in% 2:5)
      plotTable %>%
        filter(Value != 0, Sample %in% input$groupLabel) %>%
        group_by(Sample) %>%
        summarise(OTU = list(OTU)) %>%
        {`names<-`(.$OTU, .$Sample)}
    }
    
    plotListOfGroup <- function(plotTable) {
      req(length(input$groupLabel) %in% 2:5)
      plotTable %>%
        addGroupToTable(vals$filteredPhyloseq, input$groupColumnVenn) %>%
        filter(Value != 0, !!as.name(input$groupColumnVenn) %in% input$groupLabel) %>%
        group_by(!!as.name(input$groupColumnVenn)) %>%
        summarise(OTU = list(OTU)) %>%
        {`names<-`(.$OTU, .[[input$groupColumnVenn]])}
    }
    
    output$plotMessage <- renderText(HTML(vals$plotMessage))
    
    ### Result
    source("../function_plot.R", local = TRUE)
    
    output$vennPlot <- renderPlot(
      {
        req(vals$venn)
        grid::grid.newpage()
        grid::grid.draw(vals$venn)
      }
    )
    
    observe({
      req(vals$plotList)
      futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
      vals$venn <- VennDiagram::venn.diagram(x = vals$plotList,
                                             filename = NULL,
                                             fill = 2 : (length(vals$plotList) + 1),
                                             height = VENN_HEIGHT,
                                             width = VENN_WIDTH,
                                             margin = VENN_MARGIN,
                                             main = input$title,
                                             main.cex = input$labelSize + 1,
                                             cat.cex = input$labelSize,
                                             cex = max(1, input$labelSize - 1))
    })
    
    # downloadDialogButton
    source("../dialog_download.R", local = TRUE)
    
    observeEvent(input$downloadDialogButton, showModal(downloadImageDialog()))
    
    output$imageDownloadButton <- downloadHandler(paste0(input$imageFileName, ".png"),
                                                  content = function(file) {
                                                    png(file, input$imageWidth * 300, input$imageHeight * 300)
                                                    grid::grid.draw(vals$venn)
                                                    dev.off()
                                                    removeModal()
                                                  }
    )
    
  }
)















































