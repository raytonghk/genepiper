library(shiny)
library(tidyverse)
library(phyloseq)
library(shinyjs)

shinyServer(
  function(input, output, session) {
    vals <- reactiveValues()
    
    output$friedmanReady <- reactive(isTruthy(vals$friedmanTable))
    outputOptions(output, "friedmanReady", suspendWhenHidden = FALSE)
    
    source("../function_phyloseq.R")
    
    ### Load Data Panel
    source("../server_panel_load_data.R", local = TRUE)
    
    ### Filter Panel
    source("../server_panel_filter.R", local = TRUE)
    
    ### Parameter Panel
    source("../server_panel_parameter.R", local = TRUE)
    
    observe(
      {
        req(vals$filteredPhyloseq)
        vals$friedman <- NULL
      }
    )
    
    serverTaxRank()
    serverGroupColumnWithLabel()
    
    observe({
      req(vals$filteredPhyloseq, input$groupColumn)
      choices <- characterVariableNames(vals$filteredPhyloseq)
      choices <- choices[!choices == input$groupColumn]
      updateSelectInput(session, "blockColumn", choices = choices)
    })
    
    # analysisButton
    observeEvent(input$analysisButton,
                 {
                   vals$analysisMessage <- NULL
                   vals$modifiedPhyloseq <- NULL
                   vals$tableWithGroup <- NULL
                   vals$friedmanTable <- NULL
                   tryCatch(
                     {
                       req(vals$filteredPhyloseq, input$taxRank, input$abundanceType, 
                           input$groupColumn, input$blockColumn, input$groupLabels, 
                           input$signif)
                       vals$modifiedPhyloseq <- agglomerateTaxa(vals$filteredPhyloseq, input$taxRank) %>%
                         transformCount(input$abundanceType)
                       vals$tableWithGroup <- otuDataFrameWithTaxaRowname(vals$modifiedPhyloseq, input$taxRank) %>%
                         rownames_to_column("OTU") %>%
                         gather("Sample", "Value", -OTU) %>%
                         addGroupToTable(vals$filteredPhyloseq, input$groupColumn) %>%
                         addGroupToTable(vals$filteredPhyloseq, input$blockColumn) %>%
                         filter(!!as.name(input$groupColumn) %in% input$groupLabels) %>%
                         arrange(!!as.name(input$groupColumn), !!as.name(input$blockColumn))
                       vals$friedmanTable <- select(vals$tableWithGroup, -Sample) %>%
                         group_by(OTU) %>%
                         nest() %>%
                         rowwise() %>%
                         mutate(friedman = list(friedman.test(data[["Value"]], data[[input$groupColumn]],
                                                              data[[input$blockColumn]]))) %>%
                         mutate(chi_squared = friedman$statistic, p_value = friedman$p.value) %>%
                         filter(p_value <= input$signif) %>%
                         select(-data, -friedman) %>%
                         formatTableWithTaxRank(vals$filteredPhyloseq, input$taxRank)
                       
                       vals$groupColumn <- input$groupColumn
                       vals$groupLabels <- input$groupLabels
                     },
                     error = function(e) {
                       vals$analysisMessage <- "Error: Friedman Test error. Please check if the block variables are unique with each group."
                     }
                   )
                 })
    

    
    output$analysisMessage <- renderText(HTML(vals$analysisMessage))
    
    ### Graphic panel
    # Dot
    observe({
      hideTab("graphicTabset", "Dot")
      req(input$plotType == "dot")
      showTab("graphicTabset", "Dot")
    })
    
    ### Result
    # friedmanTable
    output$friedmanTable <- DT::renderDataTable(DT::datatable(vals$friedmanTable, selection = "single", options = list(scrollX = TRUE)) %>%
                                                  DT::formatRound(c("chi_squared", "p_value"), 4))
    
    output$downloadFriedmanTable <- downloadHandler("friedman.tsv",
                                                    function(file) {
                                                      write_tsv(vals$friedmanTable, file)
                                                    })
    
    # friedmanPlot
    source("../function_plot.R", local = TRUE)
    output$friedmanPlot <- renderPlot(print(vals$formatedGg))
    
    observe({
      vals$formatedGg <- NULL
      req(vals$gg)
      vals$formatedGg <- formatGg(vals$gg)
    })
    
    observe({
      vals$gg <- NULL
      req(vals$friedmanTable)
    })
    
    observe({
      vals$gg <- NULL
      req(vals$friedmanTable, vals$tableWithGroup, input$friedmanTable_rows_selected, vals$groupColumn)
      
      selectedOTU <- vals$friedmanTable$OTU[input$friedmanTable_rows_selected]
      plotTable <- filter(vals$tableWithGroup, OTU == selectedOTU)
      
      groupComb <- as.list(data.frame(combn(vals$groupLabels, 2), stringsAsFactors = FALSE))
      maxVal <- max(plotTable$Value)
      yInterval <- maxVal / 10
      yPos <- seq(maxVal + yInterval, maxVal + yInterval * length(groupComb), length.out = length(groupComb))
      
      vals$gg <- plotFriedman(plotTable, input$plotType) +
        ggsignif::geom_signif(comparisons = groupComb, map_signif_level = !input$signifAnnotation, y_position = yPos,
                              test.args = list(paired = TRUE)) +
        labs(y = "Abundance") +
        ggtitle(selectedOTU)
    })
    
    plotFriedman <- function(plotTable, plotType) {
      gg <- plotTable %>%
        ggplot(aes_string(x = vals$groupColumn, y = "Value"))
      switch(plotType,
             box = {
               gg +
                 geom_boxplot(aes_string(group = vals$groupColumn, fill = vals$groupColumn))
             },
             notch = {
               gg +
                 geom_boxplot(aes_string(group = vals$groupColumn, fill = vals$groupColumn), notch = TRUE)
             },
             violin = {
               gg +
                 geom_violin(aes_string(group = vals$groupColumn, fill = vals$groupColumn)) +
                 geom_boxplot(aes_string(group = vals$groupColumn, fill = vals$groupColumn), color = "black", width = 0.05)
             },
             dot = {
               gg +
                 geom_boxplot(aes_string(group = vals$groupColumn), width = 0.5) +
                 geom_dotplot(aes_string(fill = vals$groupColumn, color = vals$groupColumn), binaxis = "y", 
                              stackdir = "center", dotsize = input$dotSize)
             })
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


















