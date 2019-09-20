library(shiny)
library(tidyverse)
library(phyloseq)
library(shinyjs)

shinyServer(
  function(input, output, session) {
    vals <- reactiveValues()
    
    output$alphaTableReady <- reactive(isTruthy(vals$alphaTable))
    outputOptions(output, "alphaTableReady", suspendWhenHidden = FALSE)
    
    source("../function_phyloseq.R")
    
    ### Load Data Panel
    source("../server_panel_load_data.R", local = TRUE)
    
    ### Filter Panel
    source("../server_panel_filter.R", local = TRUE)
    
    ### Parameter Panel
    source("../server_panel_parameter.R", local = TRUE)
    
    observe({
      req(vals$filteredPhyloseq)
      vals$alphaTable <- NULL
    })
    
    serverTaxRank()
    
    # analysisButton
    observeEvent(input$analysisButton,
                 {
                   vals$analysisMessage <- NULL
                   vals$alphaTable <- NULL
                   tryCatch(
                     {
                       req(vals$filteredPhyloseq, input$taxRank)
                       phyloseq <- agglomerateTaxa(vals$filteredPhyloseq, input$taxRank)
                       vals$alphaTable <- estimate_richness(phyloseq)
                     },
                     error = function(e) {
                       vals$analysisMessage <- "Error: Alpha table estimation error."
                     }
                   )
                 }
    )
    
    output$analysisMessage <- renderText(HTML(vals$analysisMessage))
    
    ### Graphic Panel
    source("../server_panel_graphic.R", local = TRUE)
    
    # plotIndex
    observe({
      req(vals$alphaTable)
      updateSelectInput(session, "plotIndex", choices = c("All", alphaIndex(vals$alphaTable)))
    })
    
    alphaIndex <- function(alphaTable) {
      select(alphaTable, -starts_with("se.")) %>%
        colnames()
    }
    
    # Legend Panel
    observe({
      req(input$graphicGroupColumn)
      if(input$graphicGroupColumn == "None") {
        hideTab("graphicTabset", "Legend")
      } else {
        showTab("graphicTabset", "Legend")
      }
    })
    
    ### Result
    ## Table Tab
    output$alphaTable <- DT::renderDataTable(
      {
        req(vals$alphaTable)
        DT::datatable(vals$alphaTable, options = list(scrollX = TRUE)) %>%
          DT::formatRound(colnames(vals$alphaTable), digits = 4)
      }
    )
    
    # downloadAlphaTableButton
    output$downloadAlphaTableButton <- downloadHandler("alpha_tab.tsv",
                                                       content = function(file) {
                                                         req(vals$alphaTable)
                                                         write_tsv(vals$alphaTable, file)
                                                       }
    )
    
    ## Plot Tab
    source("../function_plot.R", local = TRUE)
    
    output$alphaPlot <- renderPlot(print(vals$formatedGg))
    
    observe({
      vals$formatedGg <- NULL
      req(vals$gg)
      vals$formatedGg <- formatGg(vals$gg)
    })
    
    observe({
      vals$gg <- NULL
      req(vals$alphaTable)
    })
    
    observe({
      req(vals$alphaTable, input$graphicGroupColumn == "None", input$plotIndex == "All")
      vals$gg <- plotAllDotWithoutGroup(vals$alphaTable)
    })
    
    plotAllDotWithoutGroup <- function(alphaTable) {
      plotTableWithoutGroup(alphaTable) %>%
        ggplot(aes(x = Sample, y = Value)) +
        geom_point(size = input$dotSize) +
        facet_wrap(~ Index, nrow = 1, scales = "free_y") +
        labs(x = NULL, y = NULL)
    }
    
    observe({
      req(vals$alphaTable, input$graphicGroupColumn == "None", input$plotIndex != "All")
      vals$gg <- plotDotWithoutGroup(vals$alphaTable, input$plotIndex)
    })
    
    plotDotWithoutGroup <- function(alphaTable, index) {
      plotTableWithoutGroup(alphaTable) %>%
        filter(Index == index) %>%
        ggplot(aes(x = Sample, y = Value)) +
        geom_point(size = input$dotSize) +
        labs(x = NULL, y = index)
    }
    
    plotTableWithoutGroup <- function(alphaTable) {
      rownames_to_column(alphaTable, "Sample") %>%
        select(-starts_with("se.")) %>%
        gather("Index", "Value", -Sample)
    }
    
    observe({
      req(vals$alphaTable, input$graphicGroupColumn != "None",
          input$plotType == "dot", input$plotIndex == "All")
      vals$gg <- plotAllDotWithGroup(vals$alphaTable, input$graphicGroupColumn)
    })
    
    plotAllDotWithGroup <- function(alphaTable, groupColumn) {
      isolate(
        {
          plotTableWithGroup(alphaTable, groupColumn) %>%
            ggplot(aes(x = Sample, y = Value, color = Group)) +
            geom_point(size = input$dotSize) +
            facet_wrap(~ Index, nrow = 1, scales = "free_y") +
            labs(x = NULL, y = NULL) +
            theme(axis.text.x = element_text(color = getGroupFactorColor(vals$filteredPhyloseq, groupColumn)))
        }
      )
    }
    
    observe({
      req(vals$alphaTable, input$graphicGroupColumn != "None",
          input$plotType == "dot", input$plotIndex != "All")
      vals$gg <- plotDotWithGroup(vals$alphaTable, input$graphicGroupColumn, input$plotIndex)
    })
    
    plotDotWithGroup <- function(alphaTable, groupColumn, index) {
      isolate(
        {
          plotTableWithGroup(alphaTable, groupColumn) %>%
            filter(Index == index) %>%
            ggplot(aes(x = Sample, y = Value, color = Group)) +
            geom_point(size = input$dotSize) +
            labs(x = NULL, y = index) +
            theme(axis.text.x = element_text(color = getGroupFactorColor(vals$filteredPhyloseq, groupColumn)))
        }
      )
    }
    
    observe({
      req(vals$alphaTable, input$graphicGroupColumn != "None",
          input$plotType == "box", input$plotIndex == "All")
      vals$gg <- plotAllBoxWithGroup(vals$alphaTable, input$graphicGroupColumn)
    })
    
    plotAllBoxWithGroup <- function(alphaTable, groupColumn) {
      isolate(
        {
          plotTableWithGroup(alphaTable, groupColumn) %>%
            ggplot(aes(x = Group, y = Value, fill = Group)) +
            geom_boxplot(aes(group = Group)) +
            facet_wrap(~ Index, nrow = 1, scales = "free_y") +
            labs(x = NULL, y = NULL) +
            theme(axis.text.x = element_text(colour = getGroupFactorLevelColor(vals$filteredPhyloseq, groupColumn)))
        }
      )
    }
    
    observe({
      req(vals$alphaTable, input$graphicGroupColumn != "None",
          input$plotType == "box", input$plotIndex != "All")
      vals$gg <- plotBoxWithGroup(vals$alphaTable, input$graphicGroupColumn, input$plotIndex)
    })
    
    plotBoxWithGroup <- function(alphaTable, groupColumn, index) {
      isolate(
        {
          plotTableWithGroup(alphaTable, groupColumn) %>%
            filter(Index == index) %>%
            ggplot(aes(x = Group, y = Value, fill = Group)) +
            geom_boxplot(aes(group = Group)) +
            labs(x = NULL, y = index) +
            theme(axis.text.x = element_text(colour = getGroupFactorLevelColor(vals$filteredPhyloseq, groupColumn)))
        }
      )
    }
    
    plotTableWithGroup <- function(alphaTable, groupColumn) {
      isolate(
        {
          rownames_to_column(alphaTable, "Sample") %>%
            select(-starts_with("se.")) %>%
            mutate(Group = get_variable(vals$filteredPhyloseq, groupColumn)) %>%
            gather("Index", "Value", -Sample, -Group) %>%
            arrange(Group) %>%
            mutate(Sample = factor(Sample, levels = unique(Sample)))
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










