library(shiny)
library(tidyverse)
library(phyloseq)
library(shinyjs)

shinyServer(
  function(input, output, session) {
    vals <- reactiveValues()
    
    output$modifiedPhyloseqReady <- reactive(isTruthy(vals$modifiedPhyloseq))
    outputOptions(output, "modifiedPhyloseqReady", suspendWhenHidden = FALSE)
    
    source("../function_phyloseq.R")
    
    ### Load Data Panel
    source("../server_panel_load_data.R", local = TRUE)
    
    ### Filter Panel
    source("../server_panel_filter.R", local = TRUE)
    
    ### Parameter Panel
    source("../server_panel_parameter.R", local = TRUE)
    
    observe({
      req(vals$filteredPhyloseq)
      vals$modifiedPhyloseq <- NULL
    })
    
    serverTaxRank()
    
    # plotButton
    observeEvent(input$plotButton,
                 {
                   vals$plotMessage <- NULL
                   vals$modifiedPhyloseq <- NULL
                   tryCatch(
                     {
                       req(vals$filteredPhyloseq, input$taxRank, input$abundanceType)
                       vals$modifiedPhyloseq <- agglomerateTaxa(vals$filteredPhyloseq, input$taxRank) %>%
                         transformCount(input$abundanceType)
                       vals$taxRank <- input$taxRank
                       vals$abundanceType <- input$abundanceType
                       vals$prevalence <- input$prevalence
                       vals$displayFilter <- input$displayFilter
                       vals$displayNumber <- input$displayNumber
                     },
                     error = function(e) {
                       vals$plotMessage <- "Error: Create plot table error."
                     }
                   )
                 }
    )
    
    output$plotMessage <- renderText(HTML(vals$plotMessage))
    
    ### Graphic Panel
    source("../server_panel_graphic.R", local = TRUE)
    
    ### Result
    source("../function_plot.R", local = TRUE)
    
    output$barPlot <- renderPlot(print(vals$formatedGg))
    
    observe({
      vals$formatedGg <- NULL
      req(vals$gg)
      vals$formatedGg <- formatGg(vals$gg)
    })
    
    observe({
      vals$gg <- NULL
      req(vals$modifiedPhyloseq)
    })
    
    observe({
      req(vals$modifiedPhyloseq, input$graphicGroupColumn == "None")
      vals$gg <- plotBarWithoutGroup(vals$modifiedPhyloseq, vals$taxRank, vals$prevalence, vals$displayFilter, vals$displayNumber)
    })
    
    plotBarWithoutGroup <- function(phyloseq, rank, prevalence, displayFilter, displayNumber) {
      isolate(
        {
          rankPlotTable(phyloseq, rank) %>%
            filterTaxaByPrevalence(prevalence, rank) %>%
            filterTaxaByAbundance(displayFilter, displayNumber, rank) %>%
            factorPlotTableRankBySum(rank) %>%
            ggplot(aes_string(x = "Sample", y = "Value", fill = rank)) +
            geom_col(color = "black") +
            labs(y = vals$abundanceType)
        }
      )
    }
    
    observe({
      req(vals$modifiedPhyloseq, input$graphicGroupColumn != "None")
      vals$gg <- plotBarWithGroup(vals$modifiedPhyloseq, input$graphicGroupColumn, vals$taxRank, 
                                  vals$prevalence, vals$displayFilter, vals$displayNumber)
    })
    
    plotBarWithGroup <- function(phyloseq, groupColumn, rank, prevalence, displayFilter, displayNumber) {
      isolate(
        {
          rankPlotTable(phyloseq, rank) %>%
            filterTaxaByPrevalence(prevalence, rank) %>%
            filterTaxaByAbundance(displayFilter, displayNumber, rank) %>%
            addGroupToTable(phyloseq, groupColumn) %>%
            factorPlotTableRankBySum(rank) %>%
            factorPlotTableSampleByGroup(groupColumn) %>%
            ggplot(aes_string(x = "Sample", y = "Value", fill = rank)) +
            geom_col(color = "black") +
            labs(y = vals$abundanceType) +
            theme(axis.text.x = element_text(color = getGroupFactorColor(phyloseq, groupColumn)))
        }
      )
    }
    
    # downloadDialogButton
    source("../dialog_download.R", local = TRUE)
    
    observeEvent(input$downloadDialogButton, showModal(downloadImageDialog()))
    
    output$imageDownloadButton <- downloadHandler(paste0(input$imageFileName, ".png"),
                                                  content = function(file) {
                                                    ggsave(file, vals$formatedGg, "png", height = input$imageHeight, width = input$imageWidth)
                                                    removeModal()
                                                  }
    )
  }
)







































