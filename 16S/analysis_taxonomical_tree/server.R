library(shiny)
library(tidyverse)
library(phyloseq)
library(shinyjs)

TIP_LABEL_SIZE <- 4

shinyServer(
  function(input, output, session) {
    vals <- reactiveValues()
    
    output$treeReady <- reactive(isTruthy(vals$tree))
    outputOptions(output, "treeReady", suspendWhenHidden = FALSE)
    
    source("../function_phyloseq.R")
    
    ### Load Data Panel
    source("../server_panel_load_data.R", local = TRUE)
    
    ### Filter Panel
    source("../server_panel_filter.R", local = TRUE)
    
    ### Parameter Panel
    source("../server_panel_parameter.R", local = TRUE)
    
    observe({
      req(vals$filteredPhyloseq)
      vals$tree <- NULL
    })
    
    serverTaxRank()
    
    # columnForColor
    observe({
      req(vals$filteredPhyloseq)
      updateSelectInput(session, "columnForColor", choices = c("None", characterVariableNames(vals$filteredPhyloseq)))
    })
    
    # columnForShape
    observe({
      req(vals$filteredPhyloseq)
      updateSelectInput(session, "columnForShape", choices = c("None", characterVariableNames(vals$filteredPhyloseq)))
    })
    
    # columnForSize
    observe({
      req(vals$filteredPhyloseq)
      updateSelectInput(session, "columnForSize", choices = c("None", "Abundance" = "abundance", numericVariableNames(vals$filteredPhyloseq)))
    })
    
    # plotButton
    observeEvent(input$plotButton,
                 {
                   vals$plotMessage <- NULL
                   vals$tree <- NULL
                   tryCatch(
                     {
                       req(vals$filteredPhyloseq, input$taxRank)
                       phyloseq <- agglomerateTaxa(vals$filteredPhyloseq, input$taxRank) %>%
                         filterPhyloseqTaxaByPrevalence(input$prevalence) %>%
                         filterPhyloseqTaxaByAbundance(input$displayFilter, input$displayNumber)
                       str(phyloseq)
                       vals$tree <- plotTree(phyloseq)
                       vals$treeHeight <- treeHeight(phyloseq)
                     }
                   )
                 }
    )
    
    plotTree <- function(phyloseq) {
      plot_tree(phyloseq, color = colorColumn(), shape = shapeColumn(),
                size = sizeColumn(), label.tips = tipLabelColumn(), text.size = TIP_LABEL_SIZE, nodelabf = nodeLabel()
      )
    }
    
    colorColumn <- function() {
      switch((input$columnForColor != "None") + 1, NULL, input$columnForColor)
    }
    
    shapeColumn <- function() {
      switch((input$columnForShape != "None") + 1, NULL, input$columnForShape)
    }
    
    sizeColumn <- function() {
      switch((input$columnForSize != "None") + 1, NULL, input$columnForSize)
    }
    
    tipLabelColumn <- function() {
      switch((input$displayTipLabel) + 1, NULL, input$taxRank)
    }
    
    nodeLabel <- function() {
      switch((input$displayNodeLabel) + 1, nodeplotblank, NULL)
    }
    
    treeHeight <- function(phyloseq) {
      length(phy_tree(phyloseq)$tip.label) * input$plotHeight
    }
    
    output$plotMessage <- renderText(HTML(vals$plotMessage))
    
    ### Graphic Panel
    # Legend Tab
    hideTab("graphicTabset", "Legend")
    
    observeEvent(input$plotButton,
      {
        if(all(c(input$columnForColor, input$columnForShape, input$columnForSize) == "None")) {
          hideTab("graphicTabset", "Legend")
        } else {
          showTab("graphicTabset", "Legend")
        }
      }
    )
    
    ### Result
    # treePlot
    observe({
      req(vals$tree, vals$treeHeight)
  
      tree <- vals$tree +
        ggtitle(input$title)
      
      try(
        {
          if(input$displayLegend == FALSE) {
            tree <- tree +
              guides(color = FALSE, fill = FALSE, shape = FALSE)
          }
        }, silent = TRUE
      )
      
      output$treePlot <- renderPlot(print(tree), height = vals$treeHeight)
    })
    
    # downloadDialogButton
    source("../dialog_download.R", local = TRUE)
    
    observeEvent(input$downloadDialogButton, showModal(downloadImageDialog()))
    
    output$imageDownloadButton <- downloadHandler(filename = function() {
      paste0(input$imageFileName, ".png")
    },
                                                  content = function(file) {
                                                    ggsave(file, vals$tree, "png", height = input$imageHeight, width = input$imageWidth)
                                                    removeModal()
                                                  }
    )
  }
)
