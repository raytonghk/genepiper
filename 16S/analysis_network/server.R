library(shiny)
library(tidyverse)
library(phyloseq)
library(shinyjs)

shinyServer(
  function(input, output, session) {
    vals <- reactiveValues()
    
    output$igListReady <- reactive(isTruthy(vals$igList))
    outputOptions(output, "igListReady", suspendWhenHidden = FALSE)
    
    output$nameTypeDisplay <- reactive(isTruthy(vals$corrTarget == "Taxa" && vals$taxRank != "OTU"))
    outputOptions(output, "nameTypeDisplay", suspendWhenHidden = FALSE)
    
    source("../function_phyloseq.R")
    source("./analysis_network_function.R", local = TRUE)
    
    ### Load Data Panel
    source("../server_panel_load_data.R", local = TRUE)
    
    ### Filter Panel
    source("../server_panel_filter.R", local = TRUE)
    
    ### Parameter Panel
    source("../server_panel_parameter.R", local = TRUE)
    
    observe({
      req(vals$filteredPhyloseq)
      vals$igList <- NULL
    })
    
    serverTaxRank()
    
    # groupColumn
    observe(
      {
        req(vals$filteredPhyloseq)
        updateSelectInput(session, "groupColumn", choices = c("None", characterVariableNames(vals$filteredPhyloseq)))
      }
    )
    
    output$analysisMessage <- renderText(HTML(vals$analysisMessage))
    
    observeEvent(
      input$analysisButton,
      {
        vals$dfList <- NULL
        vals$igList <- NULL
        vals$taxRank <- NULL
        vals$corrTarget <- NULL
        vals$analysisMessage <- NULL
        req(vals$filteredPhyloseq, input$taxRank, input$abundanceType, input$corrMethod, input$corrTarget)
        tryCatch(
          {
            dfList <- agglomerateTaxa(vals$filteredPhyloseq, input$taxRank) %>%
              transformCount(input$abundanceType) %>%
              getDFList(input$corrTarget)
            vals$dfList <- dfList
            vals$igList <- dfList %>%
              getCorrList(input$corrMethod) %>%
              getIgList(input$corrMethod)
            vals$taxRank <- input$taxRank
            vals$corrTarget <- input$corrTarget
          },
          error = function(e) {
            vals$analysisMessage <- "Error: Correlation error."
          }
        )
      }
    )
    
    ### Graphic Panel
    observe(
      {
        req(vals$corrTarget, vals$filteredPhyloseq, vals$taxRank)
        if(vals$corrTarget == "Taxa") {
          ranks <- c(rank_names(vals$filteredPhyloseq), "OTU")
          updateSelectInput(session, "nodeColor", choices = c("None", ranks[1 : which(ranks == vals$taxRank)]))
        } else if(vals$corrTarget == "Samples") {
          updateSelectInput(session, "nodeColor", choices = c("None", characterVariableNames(vals$filteredPhyloseq)))
        }
      }
    )
    
    observe(
      {
        req(vals$corrTarget, vals$filteredPhyloseq)
        if(vals$corrTarget == "Taxa") {
          updateSelectInput(session, "nodeSize", choices = c("None", "Abundance"))
        } else {
          updateSelectInput(session, "nodeSize", choices = c("None", "Abundance", numericVariableNames(vals$filteredPhyloseq)))
        }
      }
    )
    
    ### Result
    observe(
      {
        req(vals$igList)
        updateSelectInput(session, "plotSelect", choices = names(vals$igList))
      }
    )
    
    output$plotNetwork <- visNetwork::renderVisNetwork(vals$vis)
    
    observe(
      {
        vals$vis <- NULL
        req(vals$ig)
        vals$vis <- vals$ig %>%
          visNetwork::visIgraph() %>%
          visNetwork::visNodes(font = list(size = input$labelSize)) %>%
          visNetwork::visLegend()
      }
    )
    
    observe(
      {
        vals$ig <- NULL
        req(vals$igList, input$plotSelect)
        vals$ig <- vals$igList[[input$plotSelect]] %>%
          igEdgeColor() %>%
          igEdgeSize() %>%
          igNodeColor() %>%
          igNodeSize() %>%
          igNameType()
      }
    )
    
    output$downloadIgButton <- downloadHandler("ig.rds",
                                               function(file) {
                                                 req(vals$ig)
                                                 saveRDS(vals$ig, file)
                                               })
    
    # downloadDialogButton
    source("../dialog_download.R", local = TRUE)
    
    observeEvent(input$downloadDialogButton, showModal(downloadImageDialogWithoutDim()))
    
    output$imageDownloadButton <- downloadHandler(paste0(input$imageFileName, ".html"),
                                                  content = function(file) {
                                                    visNetwork::visSave(vals$vis, file)
                                                    removeModal()
                                                  }
    )
  }
)




























