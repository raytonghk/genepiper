library(shiny)
library(tidyverse)
library(phyloseq)
library(shinyjs)

shinyServer(
  function(input, output, session) {
    vals <- reactiveValues()
    
    output$pcoaReady <- reactive(isTruthy(vals$pcoa))
    outputOptions(output, "pcoaReady", suspendWhenHidden = FALSE)
    
    source("../function_phyloseq.R")
    
    ### Load Data Panel
    source("../server_panel_load_data.R", local = TRUE)
    
    ### Filter Panel
    source("../server_panel_filter.R", local = TRUE)
    
    ### Parameter Panel
    source("../server_panel_parameter.R", local = TRUE)
    
    observe({
      req(vals$filteredPhyloseq)
      vals$pcoa <- NULL
    })
    
    serverTaxRank()
    serverDistanceMethod()
    
    # analysisButton
    observeEvent(input$analysisButton,
                 {
                   vals$analysisMessage <- NULL
                   vals$modifiedPhyloseq <- NULL
                   vals$distanceMatrix <- NULL
                   vals$pcoa <- NULL
                   tryCatch(
                     {
                       req(vals$filteredPhyloseq, input$taxRank, input$abundanceType, input$distanceMethod)
                       vals$modifiedPhyloseq <- agglomerateTaxa(vals$filteredPhyloseq, input$taxRank) %>%
                         transformCount(input$abundanceType)
                       vals$distanceMatrix <- distanceMatrix(vals$modifiedPhyloseq, input$distanceMethod)
                       vals$pcoa <- ape::pcoa(vals$distanceMatrix)
                     },
                     error = function(e) {
                       vals$analysisMessage <- "Error: PCoA error."
                     }
                   )
                 }
    )
    
    output$analysisMessage <- renderText(HTML(vals$analysisMessage))
    
    ### Graphic Panel
    source("../server_panel_graphic.R", local = TRUE)
    
    ## Plot Axis Tab
    observe({
      req(vals$pcoa)
      choices <- colnames(vals$pcoa$vectors)
      updateCheckboxGroupInput(session, "plotAxis2d", choices = choices, selected = choices[1 : 2])
    })
    
    observe({
      req(vals$pcoa)
      choices <- colnames(vals$pcoa$vectors)
      updateCheckboxGroupInput(session, "plotAxis3d", choices = choices, selected = choices[1 : 3])
    })
    
    ## Envfit Tab
    source("../server_envfit.R", local = TRUE)
    
    ## Legend Tab
    observe({
      req(input$graphicGroupColumn)
      if(input$graphicGroupColumn == "None") {
        hideTab("graphicTabset", "Legend")
      } else {
        showTab("graphicTabset", "Legend")
      }
    })
    
    ### Result
    ## Distance Matrix Tab
    output$distanceMatrix <- DT::renderDataTable(
      {
        req(vals$distanceMatrix)
        distanceMatrix <- vals$distanceMatrix %>%
          as.matrix() %>%
          as.data.frame()
        DT::datatable(distanceMatrix, options = list(scrollX = TRUE)) %>%
          DT::formatRound(colnames(distanceMatrix), digits = 4)
      }
    )
    
    output$downloadDistanceMatrix <- downloadHandler("distance_matrix.rds", function(file) saveRDS(vals$distanceMatrix, file))
    
    ## Pcoa Tab
    output$eigenValuesTable <- DT::renderDataTable(DT::datatable(vals$pcoa$values, options = list(scrollX = TRUE)) %>%
                                                     DT::formatRound(colnames(vals$pcoa$values), digits = 4))
    
    output$downloadPCoAButton <- downloadHandler("pcoa.rds", function(file) saveRDS(vals$pcoa, file))
    
    ## Sample Tab
    output$sampleTable <- DT::renderDataTable(
      {
        req(vals$sampleTable)
        sampleTable <- vals$sampleTable
        DT::datatable(sampleTable, options = list(scrollX = TRUE)) %>%
                                                DT::formatRound(colnames(sampleTable), digits = 4)
      }
    )
    
    observe({
      vals$sampleTable <- NULL
      req(vals$pcoa)
      vals$sampleTable <- vals$pcoa$vectors %>%
        as.data.frame()
    })
    
    output$downloadSampleTableButton <- downloadHandler("pcoa_sample.tsv",
                                                        function(file) {
                                                          write.table(vals$sampleTable, file, sep = "\t", quote = FALSE)
                                                        })
    
    ## Permanova Tab
    source("../server_permanova.R", local = TRUE)
    
    ## 2D Plot Tab
    source("../function_plot.R", local = TRUE)
    output$plot2d <- renderPlot(print(vals$formatedGg))
    
    observe({
      vals$formatedGg <- NULL
      req(vals$gg)
      vals$formatedGg <- formatSquareGg(vals$gg)
    })
    
    observe({
      vals$gg <- NULL
      req(vals$pcoa)
    })
    
    observe({
      vals$gg <- NULL
      req(vals$pcoa, input$graphicGroupColumn, length(input$plotAxis2d) == 2)
      if(input$graphicGroupColumn == "None") {
        gg <- plotPcoaWithoutGroup()
      } else {
        gg <- plotPcoaWithGroup()
      }
      vals$gg <- addImportanceToAxisTitle(gg)
    })
    
    addImportanceToAxisTitle <- function(gg) {
      importance <- c(round(vals$pcoa$values[as.numeric(substring(input$plotAxis2d[1], 6)), "Relative_eig"] * 100, 2), 
                      round(vals$pcoa$values[as.numeric(substring(input$plotAxis2d[2], 6)), "Relative_eig"] * 100, 2))
      
      gg +
        labs(x = paste0(input$plotAxis2d[1], " (", importance[1], "%)"),
             y = paste0(input$plotAxis2d[2], " (", importance[2], "%)"))
    }
    
    
    plotPcoaWithoutGroup <- function() {
      gg <- ggplot()
      if(isTruthy(vals$envfit) && input$plotEnvfit) {
        gg <- plotEnvfit(gg, vals$envfit, vals$sampleTable, input$plotAxis2d, vals$envfitFactorLabel, input$envfitVectorLabel, 
                         input$envfitFactorDotSize, input$envfitFactorLabelSize, input$envfitVectorLineSize, input$envfitVectorLabelSize)
      }
      if(input$plotSample) {
        gg <- plotDotSampleWithoutGroup(gg, vals$sampleTable, input$plotAxis2d, input$labelSample, input$sampleDotSize, input$sampleLabelSize)
      }
      gg
    }
    
    plotPcoaWithGroup <- function() {
      sampleTable <- sampleTableWithGroup(vals$sampleTable, vals$modifiedPhyloseq, input$graphicGroupColumn)
      gg <- ggplot()
      if(isTruthy(vals$envfit) && input$plotEnvfit) {
        gg <- plotEnvfit(gg, vals$envfit, vals$sampleTable, input$plotAxis2d, vals$envfitFactorLabel, input$envfitVectorLabel, 
                         input$envfitFactorDotSize, input$envfitFactorLabelSize, input$envfitVectorLineSize, input$envfitVectorLabelSize)
      }
      if(input$plotSample) {
        gg <- plotDotSampleWithGroup(gg, sampleTable, input$plotAxis2d, input$graphicGroupColumn, input$labelSample, input$sampleDotSize, input$sampleLabelSize)
      }
      if(input$plotConvexHull) {
        gg <- plotConvexHull(gg, sampleTable, input$plotAxis2d, input$graphicGroupColumn)
      }
      if(input$plotSpider) {
        gg <- plotSpider(gg, sampleTable, input$plotAxis2d, input$graphicGroupColumn, input$spiderLineSize, input$spiderLabelSize)
      }
      if(input$plotEllipse) {
        gg <- plotEllipse(gg, sampleTable, input$plotAxis2d, input$graphicGroupColumn, input$ellipseType, input$ellipseSignif, input$ellipseLineSize)
      }
      gg
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
    
    ## 3D Plot Tab
    source("../server_3d_plot.R", local = TRUE)
    
    output$plot3d <- plotly::renderPlotly(vals$pl)
    
    observe({
      vals$pl <- NULL
      req(vals$pcoa, input$graphicGroupColumn3d, length(input$plotAxis3d) == 3)
      if(input$graphicGroupColumn3d == "None") {
        vals$pl <- plot3dWithoutGroup()
      } else {
        vals$pl <- plot3dWithGroup()
      }
    })
    
    plot3dWithoutGroup <- function() {
      pl <- plotly::plotly_empty()
      if(input$plotSample3d) {
        pl <- plot3dSampleWithoutGroup(pl, vals$sampleTable, input$plotAxis3d)
      }
      pl
    }
    
    plot3dWithGroup <- function() {
      pl <- plotly::plotly_empty()
      if(input$plotSample3d) {
        pl <- plot3dSampleWithGroup(pl, vals$sampleTable, input$plotAxis3d, vals$modifiedPhyloseq, input$graphicGroupColumn3d)
      }
      pl
    }
  }
)















































