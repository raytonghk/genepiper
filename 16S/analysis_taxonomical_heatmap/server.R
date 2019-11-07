library(shiny)
library(tidyverse)
library(phyloseq)
library(shinyjs)

shinyServer(
  function(input, output, session) {
    vals <- reactiveValues()
    
    output$plotTableReady <- reactive(isTruthy(vals$plotTable))
    outputOptions(output, "plotTableReady", suspendWhenHidden = FALSE)
    
    source("../function_phyloseq.R")
    
    ### Load Data Panel
    source("../server_panel_load_data.R", local = TRUE)
    
    ### Filter Panel
    source("../server_panel_filter.R", local = TRUE)
    
    ### Parameter Panel
    source("../server_panel_parameter.R", local = TRUE)
    
    observe({
      req(vals$filteredPhyloseq)
      vals$plotTable <- NULL
    })
    
    serverTaxRank()
    
    # sampleGroupColumn
    observe({
      req(input$sampleOrderBy == "Group")
      updateSelectInput(session, "sampleGroupColumn", choices = characterVariableNames(vals$filteredPhyloseq))
    })
    
    # displayWhich
    observe({
      req(input$sampleOrderBy != "Group")
      updateSelectInput(session, "displayWhich", choices = list("Top Abundance" = "top", "Least Abundance" = "least"))
    })
    
    observe({
      req(input$sampleOrderBy == "Group")
      updateSelectInput(session, "displayWhich",
                        choices = list("Top Abundance" = "top",
                                       "Least Abundance" = "least",
                                       "Anova Significant" = "anovaSignificant",
                                       "Kruskal Significant" = "kruskalSignificant")
      )
    })
    
    # plotButton
    observeEvent(input$plotButton,
                 {
                   vals$plotMessage <- NULL
                   vals$plotTable <- NULL
                   tryCatch(
                     {
                       req(vals$filteredPhyloseq, input$taxRank, input$abundanceType)
                       vals$plotTable <- agglomerateTaxa(vals$filteredPhyloseq, input$taxRank) %>%
                         transformCount(input$abundanceType) %>%
                         rankPlotTable(input$taxRank) %>%
                         factorPlotTableSample(input$sampleOrderBy) %>%
                         factorPlotTableRank(input$taxaOrderBy) %>%
                         subsetPlotTableForDisplay(input$displayWhich) %>%
                         rescalePlotTable(input$rescaleMethod)
                       
                       vals$taxRank <- input$taxRank
                       vals$sampleOrderBy <- input$sampleOrderBy
                       if(input$sampleOrderBy == "Group") {
                         vals$groupColumn <- input$sampleGroupColumn
                       }
                     },
                     error = function(e) {
                       vals$plotMessage <- "Error: Create plot table error."
                     }
                   )
                 }
    )
    
    factorPlotTableSample <- function(plotTable, orderBy) {
      switch(orderBy,
             "Name" = plotTable,
             "Group" = factorPlotTableSampleByGroup(addGroupToTable(plotTable, vals$filteredPhyloseq, input$sampleGroupColumn), input$sampleGroupColumn),
             "Hierarchical Clustering" = factorPlotTableSampleByHClust(plotTable)
      )
    }
    
    factorPlotTableSampleByHClust <- function(plotTable) {
      sampleOrder <- spread(plotTable, input$taxRank, "Value") %>%
        column_to_rownames("Sample") %>%
        dist(input$sampleDistanceMethod) %>%
        hclust(input$sampleClusterMethod) %>%
        {.$labels[.$order]}
      mutate(plotTable, Sample = factor(Sample, levels = sampleOrder))
    }
    
    factorPlotTableRank <- function(plotTable, orderBy) {
      switch(orderBy,
             "Name" = plotTable,
             "Hierarchical Clustering" = factorPlotTableRankByHClust(plotTable)
      )
    }
    
    factorPlotTableRankByHClust <- function(plotTable) {
      rankOrder <- select(plotTable, one_of(c("Sample", "Value", input$taxRank))) %>%
        spread("Sample", "Value") %>%
        column_to_rownames(input$taxRank) %>%
        dist(input$taxaDistanceMethod) %>%
        hclust(input$taxaClusterMethod) %>%
        {.$labels[.$order]}
      mutate(plotTable, !!as.name(input$taxRank) := factor(!!as.name(input$taxRank), levels = rankOrder))
    }
    
    subsetPlotTableForDisplay <- function(plotTable, displayWhich) {
      switch(displayWhich,
             top = subsetTopAbundance(plotTable),
             least = subsetLeastAbundance(plotTable),
             anovaSignificant = subsetAnovaSignif(plotTable),
             kruskalSignificant = subsetKruskalSignif(plotTable))
    }
    
    subsetTopAbundance <- function(plotTable) {
      topTaxa <- plotTable %>%
        group_by(!!as.name(input$taxRank)) %>%
        tally(Value) %>%
        top_n(input$displayNumber) %>%
        .[[input$taxRank]]
      filter(plotTable, !!as.name(input$taxRank) %in% topTaxa)
    }
    
    subsetLeastAbundance <- function(plotTable) {
      leastTaxa <- plotTable %>%
        group_by(!!as.name(input$taxRank)) %>%
        tally(Value) %>%
        filter(n != 0) %>%
        top_n(-input$displayNumber) %>%
        .[[input$taxRank]]
      filter(plotTable, !!as.name(input$taxRank) %in% leastTaxa)
    }
    
    subsetAnovaSignif <- function(plotTable) {
      anovaSignif <- plotTable %>%
        group_by(!!as.name(input$taxRank)) %>%
        summarise(aov = summary(aov(as.formula(paste0("Value ~ ", input$sampleGroupColumn))))[[1]]$`Pr(>F)`[1]) %>%
        filter(!is.nan(aov), aov <= 0.05) %>%
        top_n(-input$displayNumber) %>%
        .[[input$taxRank]]
      filter(plotTable, !!as.name(input$taxRank) %in% anovaSignif)
    }
    
    subsetKruskalSignif <- function(plotTable) {
      kruskalSignif <- plotTable %>%
        group_by(!!as.name(input$taxRank)) %>%
        summarise(kruskal = kruskal.test(as.formula(paste0("Value ~ factor(", input$sampleGroupColumn, ")")))$p.value) %>%
        filter(!is.nan(kruskal), kruskal <= 0.05) %>%
        top_n(-input$displayNumber) %>%
        .[[input$taxRank]]
      filter(plotTable, !!as.name(input$taxRank) %in% kruskalSignif)
    }
    
    rescalePlotTable <- function(plotTable, method) {
      switch(method,
             None = plotTable,
             Normalization = rescalePlotTableByNormalization(plotTable),
             Standardization = rescalePlotTableByStandardization(plotTable)
      )
    }
    
    rescalePlotTableByNormalization <- function(plotTable) {
      plotTable %>%
        group_by(!!as.name(input$taxRank)) %>%
        mutate(Value = (Value - min(Value)) / (max(Value) - min(Value))) %>%
        ungroup()
    }
    
    rescalePlotTableByStandardization <- function(plotTable) {
      plotTable %>%
        group_by(!!as.name(input$taxRank)) %>%
        mutate(Value = (Value - mean(Value)) / sd(Value)) %>%
        ungroup()
    }
    
    ### Graphic Panel
    source("../server_panel_graphic.R", local = TRUE)
    
    ### Result
    source("../function_plot.R", local = TRUE)
    
    output$heatmap <- renderPlot(print(vals$formatedGg))
    
    observe({
      vals$formatedGg <- NULL
      req(vals$gg)
      vals$formatedGg <- changeColorScale(vals$gg, input$colorScale) %>%
        formatGg()
    })
    
    changeColorScale <- function(gg, colorScale) {
      switch (colorScale,
              "Default" = gg,
              "Heat Color" = gg + scale_fill_gradientn(colors = heat.colors(100)),
              "Green Red" = gg + scale_fill_gradientn(colors = colorRampPalette(c("green", "black", "red"))(100)),
              "Viridis" = gg + scale_fill_viridis_c(),
              "Green" = gg + scale_fill_gradientn(colors = colorRampPalette(c("black", "green"))(100)),
              "Red" = gg + scale_fill_gradientn(colors = colorRampPalette(c("black", "red"))(100))
      )
    }
    
    observe({
      vals$gg <- NULL
      req(vals$plotTable)
    })
    
    observe({
      req(vals$plotTable, vals$sampleOrderBy != "Group")
      vals$gg <- plotHeatmapWithoutGroup(vals$plotTable)
    })
    
    plotHeatmapWithoutGroup <- function(plotTable) {
      isolate(
        {
          plotTable %>%
            ggplot(aes_string(x = "Sample", y = vals$taxRank)) +
            geom_tile(aes(fill = Value)) +
            scale_fill_continuous(name = NULL)
        }
      )
    }
    
    observe({
      req(vals$plotTable, vals$sampleOrderBy == "Group")
      vals$gg <- plotHeatmapWithGroup(vals$plotTable)
    })
    
    plotHeatmapWithGroup <- function(plotTable) {
      isolate(
        {
          plotHeatmapWithoutGroup(plotTable) +
            theme(axis.text.x = element_text(color = getGroupFactorColor(vals$filteredPhyloseq, vals$groupColumn)))
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












































