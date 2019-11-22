library(shiny)
library(tidyverse)
library(phyloseq)
library(shinyjs)

shinyServer(
  function(input, output, session) {
    vals <- reactiveValues()
    
    output$pcaReady <- reactive(isTruthy(vals$pca))
    outputOptions(output, "pcaReady", suspendWhenHidden = FALSE)
    
    source("../function_phyloseq.R")
    
    ### Load Data Panel
    source("../server_panel_load_data.R", local = TRUE)
    
    ### Filter Panel
    source("../server_panel_filter.R", local = TRUE)
    
    ### Parameter Panel
    source("../server_panel_parameter.R", local = TRUE)
    
    observe({
      req(vals$filteredPhyloseq)
      vals$pca <- NULL
    })
    
    serverTaxRank()
    
    # analysisButton
    observeEvent(input$analysisButton,
                 {
                   vals$analysisMessage <- NULL
                   vals$modifiedPhyloseq <- NULL
                   vals$pca <- NULL
                   tryCatch(
                     {
                       req(vals$filteredPhyloseq, input$taxRank, input$abundanceType)
                       vals$modifiedPhyloseq <- agglomerateTaxa(vals$filteredPhyloseq, input$taxRank) %>%
                         transformCount(input$abundanceType)
                       vals$pca <- otuDataFrameWithTaxaRowname(vals$modifiedPhyloseq, input$taxRank) %>%
                         t() %>%
                         prcomp(scale. = input$scaleVariable)
                     },
                     error = function(e) {
                       vals$analysisMessage <- "Error: PCA error."
                     }
                   )
                 }
    )
    
    output$analysisMessage <- renderText(HTML(vals$analysisMessage))
    
    ### Graphic Panel
    source("../server_panel_graphic.R", local = TRUE)
    
    ## Plot Axis Tab
    observe({
      req(vals$pca)
      choices <- colnames(vals$pca$x)
      updateCheckboxGroupInput(session, "plotAxis2d", choices = choices, selected = choices[1 : 2])
    })
    
    observe({
      req(vals$pca)
      choices <- colnames(vals$pca$x)
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
    ## Output Tab
    output$pcaOutput <- renderPrint(summary(vals$pca))
    
    output$downloadPCAButton <- downloadHandler("pca.rds", function(file) saveRDS(vals$pca, file))
    
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
      req(vals$pca, vals$lam)
      vals$sampleTable <- vegan::scores(vals$pca, display = "sites") %>%
        t() %>%
        {
          . / vals$lam
        } %>%
        t() %>%
        as.data.frame()
    })
    
    observe({
      vals$lam <- NULL
      req(vals$pca)
      vals$lam <- vals$pca$sdev * sqrt(nrow(vals$pca$x))
    })
    
    output$downloadSampleTableButton <- downloadHandler("pca_sample.tsv",
                                                        function(file) {
                                                          write.table(vals$sampleTable, file, sep = "\t", quote = FALSE)
                                                        })
    
    ## Loading Tab
    output$taxaTable <- DT::renderDataTable(
      {
        req(vals$taxaTable)
        taxaTable <- vals$taxaTable
        DT::datatable(taxaTable, options = list(scrollX = TRUE)) %>%
          DT::formatRound(colnames(taxaTable), digits = 4)
      }
    )
    
    observe({
      vals$taxaTable <- NULL
      req(vals$pca, vals$lam)
      vals$taxaTable <- vegan::scores(vals$pca, display = "species") %>%
        t() %>%
        {
          . * vals$lam
        } %>%
        t() %>%
        as.data.frame()
    })
    
    output$downloadTaxaTableButton <- downloadHandler("pca_taxa.tsv",
                                                      function(file) {
                                                        write.table(vals$taxaTable, file, sep = "\t", quote = FALSE)
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
      req(vals$pca, input$graphicGroupColumn, length(input$plotAxis2d) == 2)
      if(input$graphicGroupColumn == "None") {
        gg <- plotPcaWithoutGroup()
      } else {
        gg <- plotPcaWithGroup()
      }
      vals$gg <- addImportanceToAxisTitle(gg)
    })
    
    addImportanceToAxisTitle <- function(gg) {
      importance <- summary(vals$pca)$importance %>%
      {
        .[2, input$plotAxis2d] * 100
      } %>%
        round(digits = 2)
      
      gg +
        labs(x = paste0(input$plotAxis2d[1], " (", importance[1], "%)"),
             y = paste0(input$plotAxis2d[2], " (", importance[2], "%)"))
    }
    
    plotPcaWithoutGroup <- function() {
      gg <- ggplot()
      if(input$plotTaxa) {
        gg <- plotLoading(gg)
      }
      if(isTruthy(vals$envfit) && input$plotEnvfit) {
        gg <- plotEnvfit(gg, vals$envfit, vals$sampleTable, input$plotAxis2d, vals$envfitFactorLabel, input$envfitVectorLabel, 
                         input$envfitFactorDotSize, input$envfitFactorLabelSize, input$envfitVectorLineSize, input$envfitVectorLabelSize)
      }
      if(input$plotSample) {
        gg <- plotDotSampleWithoutGroup(gg, vals$sampleTable, input$plotAxis2d, input$labelSample, input$sampleDotSize, input$sampleLabelSize)
      }
      gg
    }
    
    plotPcaWithGroup <- function() {
      sampleTable <- sampleTableWithGroup(vals$sampleTable, vals$modifiedPhyloseq, input$graphicGroupColumn)
      gg <- ggplot()
      if(input$plotTaxa) {
        gg <- plotLoading(gg)
      }
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
    
    plotLoading <- function(gg) {
      if(input$plotSample) {
        gg <- plotLoadingOnSampleScale(gg)
      } else {
        gg <- plotLoadingOnOwnScale(gg)
      }
    }
    
    plotLoadingOnSampleScale <- function(gg) {
      scaledTaxaTable <- fitCoordinate(vals$taxaTable, vals$sampleTable, input$plotAxis2d)
      ratio <- getCoordinateRatio(vals$taxaTable, vals$sampleTable, input$plotAxis2d)
      plotArrowLineTaxa(gg, scaledTaxaTable, input$plotAxis2d, input$labelTaxa, input$taxaLineSize, input$taxaLabelSize) +
        scale_x_continuous(sec.axis = sec_axis(~. * ratio)) +
        scale_y_continuous(sec.axis = sec_axis(~. * ratio))
    }
    
    plotLoadingOnOwnScale <- function(gg) {
      plotArrowLineTaxa(gg, vals$taxaTable, input$plotAxis2d, input$labelTaxa, input$taxaLineSize, input$taxaLabelSize)
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
    
    ## 3D Plot Tab
    source("../server_3d_plot.R", local = TRUE)
    
    output$plot3d <- plotly::renderPlotly(vals$pl)
    
    observe({
      vals$pl <- NULL
      req(vals$pca, input$graphicGroupColumn3d, length(input$plotAxis3d) == 3)
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








































