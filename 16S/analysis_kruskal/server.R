library(shiny)
library(tidyverse)
library(phyloseq)
library(shinyjs)

shinyServer(
  function(input, output, session) {
    vals <- reactiveValues()
    
    output$kruskalReady <- reactive(isTruthy(vals$kruskalTable))
    outputOptions(output, "kruskalReady", suspendWhenHidden = FALSE)
    
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
        vals$kruskal <- NULL
      }
    )
    
    serverTaxRank()
    serverGroupColumnWithLabel()
    
    # analysisButton
    observeEvent(input$analysisButton,
                 {
                   vals$analysisMessage <- NULL
                   vals$modifiedPhyloseq <- NULL
                   vals$tableWithGroup <- NULL
                   vals$kruskalTable <- NULL
                   tryCatch(
                     {
                       req(vals$filteredPhyloseq, input$taxRank, input$abundanceType, 
                           input$groupColumn, input$groupLabels, input$signif)
                       vals$modifiedPhyloseq <- agglomerateTaxa(vals$filteredPhyloseq, input$taxRank) %>%
                         transformCount(input$abundanceType)
                       vals$tableWithGroup <- otuDataFrameWithTaxaRowname(vals$modifiedPhyloseq, input$taxRank) %>%
                         rownames_to_column("OTU") %>%
                         gather("Sample", "Value", -OTU) %>%
                         addGroupToTable(vals$filteredPhyloseq, input$groupColumn) %>%
                         filter(!!as.name(input$groupColumn) %in% input$groupLabels)
                       vals$kruskalTable <- select(vals$tableWithGroup, -Sample) %>%
                         group_by(OTU) %>%
                         nest() %>%
                         rowwise() %>%
                         mutate(kruskal = list(kruskal.test(as.formula(paste0("Value ~ ", input$groupColumn)), data))) %>%
                         mutate(chi_squared = kruskal$statistic, p_value = kruskal$p.value) %>%
                         filter(p_value <= input$signif) %>%
                         select(-data, -kruskal) %>%
                         formatTableWithTaxRank(vals$filteredPhyloseq, input$taxRank)
                         
                       vals$groupColumn <- input$groupColumn
                       vals$groupLabels <- input$groupLabels
                     }, 
                     error = function(e) {
                       vals$analysisMessage <- "Error: Kruskal-Wallis Test error."
                     }
                   )
                 }
    )
    
    output$analysisMessage <- renderText(HTML(vals$analysisMessage))
    
    ### Graphic panel
    # Dot
    observe({
      hideTab("graphicTabset", "Dot")
      req(input$plotType == "dot")
      showTab("graphicTabset", "Dot")
    })
    
    ### Result
    # kruskalTable
    output$kruskalTable <- DT::renderDataTable(DT::datatable(vals$kruskalTable, selection = "single", options = list(scrollX = TRUE)) %>%
                                                 DT::formatRound(c("chi_squared", "p_value"), 4))
    
    output$downloadKruskalTable <- downloadHandler("kruskal.tsv",
                                                   function(file) {
                                                     write_tsv(vals$kruskalTable, file)
                                                   })
    
    # kruskalPlot
    source("../function_plot.R", local = TRUE)
    output$kruskalPlot <- renderPlot(print(vals$formatedGg))
    
    observe({
      vals$formatedGg <- NULL
      req(vals$gg)
      vals$formatedGg <- formatGg(vals$gg)
    })
    
    observe({
      vals$gg <- NULL
      req(vals$kruskalTable)
    })
    
    observe({
      vals$gg <- NULL
      req(vals$kruskalTable, vals$tableWithGroup, input$kruskalTable_rows_selected, vals$groupColumn)
      
      selectedOTU <- vals$kruskalTable$OTU[input$kruskalTable_rows_selected]
      plotTable <- filter(vals$tableWithGroup, OTU == selectedOTU)
      
      groupComb <- as.list(data.frame(combn(vals$groupLabels, 2), stringsAsFactors = FALSE))
      maxVal <- max(plotTable$Value)
      yInterval <- maxVal / 10
      yPos <- seq(maxVal + yInterval, maxVal + yInterval * length(groupComb), length.out = length(groupComb))
      
      vals$gg <- plotKruskal(plotTable, input$plotType) +
        ggsignif::geom_signif(comparisons = groupComb, map_signif_level = !input$signifAnnotation, y_position = yPos) +
        labs(y = "Abundance") +
        ggtitle(selectedOTU)
    })
    
    plotKruskal <- function(plotTable, plotType) {
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

















