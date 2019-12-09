library(shiny)
library(tidyverse)
library(phyloseq)
library(shinyjs)

shinyServer(
  function(input, output, session) {
    vals <- reactiveValues()
    
    output$correlationReady <- reactive(isTruthy(vals$correlation))
    outputOptions(output, "correlationReady", suspendWhenHidden = FALSE)
    
    source("../function_phyloseq.R")
    
    ### Load Data Panel
    source("../server_panel_load_data.R", local = TRUE)
    
    ### Filter Panel
    source("../server_panel_filter.R", local = TRUE)
    
    ### Parameter Panel
    source("../server_panel_parameter.R", local = TRUE)
    
    observe({
      req(vals$filteredPhyloseq)
      vals$correlation <- NULL
    })
    
    serverTaxRank()
    
    # analysisButton
    observeEvent(input$analysisButton,
                 {
                   vals$analysisMessage <- NULL
                   vals$modifiedPhyloseq <- NULL
                   vals$taxRank <- NULL
                   vals$correlation <- NULL
                   vals$includeAlphaIndex <- FALSE
                   tryCatch(
                     {
                       req(vals$filteredPhyloseq, input$taxRank, input$abundanceType, input$correlationType)
                       vals$modifiedPhyloseq <- agglomerateTaxa(vals$filteredPhyloseq, input$taxRank) %>%
                         transformCount(input$abundanceType)
                       vals$correlation <- getCorrelation()
                       vals$includeAlphaIndex <- input$includeAlphaIndex
                       vals$taxRank <- input$taxRank
                     },
                     error = function(e) {
                       vals$analysisMessage <- "Error: Correlation error."
                     }
                   )
                 }
    )
    
    getCorrelation <- function() {
      table <- tableForCorrelation()
      Hmisc::rcorr(as.matrix(table), type = input$correlationType)
    }
    
    tableForCorrelation <- function() {
      table <- otuDataFrameWithTaxaRowname(vals$modifiedPhyloseq, input$taxRank) %>%
        t()
      if(input$logTransform) {
        table <- logTransform(table)
      }
      table <- addNumericSampleDataToTable(table, vals$modifiedPhyloseq)
      if(input$includeAlphaIndex) {
        table <- addAlphaIndexToTable(table, vals$modifiedPhyloseq)
      }
      if(input$changeZeroToNA) {
        table <- changeZeroToNA(table)
      }
      table
    }
    
    logTransform <- function(table) {
      log(table + 1)
    }
    
    addNumericSampleDataToTable <- function(table, phyloseq) {
      cbind(table, get_variable(phyloseq, numericVariableNames(phyloseq)))
    }
    
    addAlphaIndexToTable <- function(table, phyloseq) {
      cbind(table, estimate_richness(phyloseq) %>%
              select(-starts_with("se.")))
    }
    
    changeZeroToNA <- function(table) {
      apply(table, c(1, 2), function(x){
        ifelse(x == 0, NA_integer_, x)
      })
    }
    
    output$analysisMessage <- renderText(HTML(vals$analysisMessage))
    
    ### Correlation Parameters Panel
    ## Graphic Tab
    source("../server_panel_graphic.R", local = TRUE)
    
    observe({
      hideTab("correlationTabset", "Graphic")
      req(input$resultTabset == "Plot")
      showTab("correlationTabset", "Graphic")
    })
    
    observe({
      req(vals$correlation)
      choices <- typeChoices()
      updateSelectInput(session, "xAxisType", choices = choices)
      updateSelectInput(session, "yAxisType", choices = choices)
    })
    
    typeChoices <- function() {
      choices <- list("All" = "all",
                      "Taxa" = "taxa")
      if(length(numericVariableNames(vals$modifiedPhyloseq)) > 0) {
        choices <- c(choices, "Sample Data" = "sam")
      }
      if(vals$includeAlphaIndex) {
        choices <- c(choices, "Alpha Index" = "alpha")
      }
      choices
    }
    
    observe({
      req(vals$correlation, input$xAxisType, vals$filteredCorrelationTable)
      choices <- labelChoices(input$xAxisType)
      choices <- choices[choices %in% unique(c(vals$filteredCorrelationTable$Variable_1, vals$filteredCorrelationTable$Variable_2))]
      updateCheckboxGroupInput(session, "xAxisLabel", choices = choices, selected = choices[1 : min(length(choices), 10)])
    })
    
    observe({
      req(vals$correlation, input$yAxisType, vals$filteredCorrelationTable)
      choices <- labelChoices(input$yAxisType)
      choices <- choices[choices %in% unique(c(vals$filteredCorrelationTable$Variable_1, vals$filteredCorrelationTable$Variable_2))]
      updateCheckboxGroupInput(session, "yAxisLabel", choices = choices, selected = choices[1 : min(length(choices), 10)])
    })
    
    labelChoices <- function(type) {
      switch(type,
             all = c(taxaLabel(), sampleDataLabel(), alphaLabel()),
             taxa = taxaLabel(),
             sam = sampleDataLabel(),
             alpha = alphaLabel()
      )
    }
    
    taxaLabel <- function() {
      otuDataFrameWithTaxaRowname(vals$modifiedPhyloseq, vals$taxRank) %>%
        rownames()
    }
    
    sampleDataLabel <- function() {
      numericVariableNames(vals$modifiedPhyloseq)
    }
    
    alphaLabel <- function() {
      c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher")
    }
    
    ### Result
    ## Table Tab
    output$correlationTable <- DT::renderDataTable(DT::datatable(vals$filteredCorrelationTable, options = list(scrollX = TRUE)) %>%
                                                     DT::formatRound(c("Correlation", "p_values"), digits = 4))
    
    observe({
      req(vals$correlation, input$correlationFilter, input$nFilter, input$pValueFilter)
      vals$correlationTable <- correlationTable()
      vals$filteredCorrelationTable <- filterCorrelationTable(vals$correlationTable) %>%
        filterUniquePairOfVariable()
        
    })
    
    correlationTable <- function() {
      rTable <- vals$correlation$r %>%
        as.data.frame() %>%
        rownames_to_column("Variable_1") %>%
        gather("Variable_2", "Correlation", -Variable_1)
      nTable <- vals$correlation$n %>%
        as.data.frame() %>%
        rownames_to_column("Variable_1") %>%
        gather("Variable_2", "n", -Variable_1)
      pTable <- vals$correlation$P %>%
        as.data.frame() %>%
        rownames_to_column("Variable_1") %>%
        gather("Variable_2", "p_values", -Variable_1)
      bind_cols(rTable, n = nTable$n, p_values = pTable$p_values) %>%
        as.data.frame()
    }
    
    filterCorrelationTable <- function(table) {
      table %>%
        filter(abs(Correlation) >= input$correlationFilter,
               n >= input$nFilter,
               p_values <= input$pValueFilter,
               Variable_1 != Variable_2)
    }
    
    filterUniquePairOfVariable <- function(table) {
      table %>%
        rowwise() %>%
        mutate(pair = list(c(Variable_1, Variable_2)[order(c(Variable_1, Variable_2))])) %>%
        ungroup() %>%
        filter(!duplicated(pair)) %>%
        select(-pair)
    }
    
    output$downloadCorrelationButton <- downloadHandler("correlation.tsv", function(file) write_tsv(vals$filteredCorrelationTable, file))
    
    ## Plot Tab
    source("../function_plot.R", local = TRUE)
    output$correlationPlot <- renderPlot(print(vals$formatedGg))
    
    observe({
      vals$formatedGg <- NULL
      req(vals$gg)
      vals$formatedGg <- formatGg(vals$gg)
    })
    
    observe({
      vals$gg <- NULL
      req(vals$filteredCorrelationTable)
    })
    
    observe({
      vals$gg <- NULL
      req(vals$correlationTable, length(input$xAxisLabel) > 0, length(input$yAxisLabel) > 0)
      vals$gg <- plotCorrelation()
    })
    
    plotCorrelation <- function() {
      vals$correlationTable %>%
        filter(Variable_1 %in% input$xAxisLabel,
               Variable_2 %in% input$yAxisLabel) %>%
        factorColumnBySum("Variable_1", "Correlation") %>%
        factorColumnBySum("Variable_2", "Correlation") %>%
        ggplot(aes(x = Variable_1, y = Variable_2)) +
        geom_tile(fill = "white") +
        geom_point(aes(color = Correlation, size = abs(Correlation))) +
        coord_equal() +
        scale_size_continuous(name = NULL, limits = c(0, 1)) +
        scale_color_gradientn(colors = colorRampPalette(c("blue", "red"))(100),
                              limits = c(-1, 1)) +
        labs(x = NULL, y = NULL) +
        scale_size_continuous(range = c(1, max(input$dotSize, 2)))
    }
    
    # downloadDialogButton
    source("../dialog_download.R", local = TRUE)
    
    observeEvent(input$downloadDialogButton, showModal(downloadImageDialog()))
    
    output$imageDownloadButton <- downloadHandler(filename = function() {
      paste0(input$imageFileName, ".png")
    },
                                                  content = function(file) {
                                                    ggsave(file, vals$formatedGg, "png", height = input$imageHeight, width = input$imageWidth)
                                                    removeModal()
                                                  }
    )
  }
)

















































