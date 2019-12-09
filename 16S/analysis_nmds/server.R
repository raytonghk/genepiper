library(shiny)
library(tidyverse)
library(phyloseq)
library(shinyjs)

shinyServer(
  function(input, output, session) {
    vals <- reactiveValues()
    
    output$nmdsReady <- reactive(isTruthy(vals$nmds))
    outputOptions(output, "nmdsReady", suspendWhenHidden = FALSE)
    
    source("../function_phyloseq.R")
    
    ### Load Data Panel
    source("../server_panel_load_data.R", local = TRUE)
    
    ### Filter Panel
    source("../server_panel_filter.R", local = TRUE)
    
    ### Parameter Panel
    source("../server_panel_parameter.R", local = TRUE)
    
    observe({
      req(vals$filteredPhyloseq)
      vals$nmds <- NULL
    })
    
    serverTaxRank()
    serverDistanceMethod()
    
    observe({
      req(vals$filteredPhyloseq, input$k)
      maxK <- floor((nsamples(vals$filteredPhyloseq) - 2) / 2)
      if(input$k < 2) {
        updateNumericInput(session, "k", value = 2)
      }
      if(input$k > maxK) {
        updateNumericInput(session, "k", value = maxK)
      }
    })
    
    
    
    # analysisButton
    observeEvent(input$analysisButton,
                 {
                   vals$analysisMessage <- NULL
                   vals$modifiedPhyloseq <- NULL
                   vals$distanceMatrix <- NULL
                   vals$nmds <- NULL
                   tryCatch(
                     {
                       req(vals$filteredPhyloseq, input$taxRank, input$abundanceType, input$distanceMethod)
                       vals$modifiedPhyloseq <- agglomerateTaxa(vals$filteredPhyloseq, input$taxRank) %>%
                         transformCount(input$abundanceType)
                       vals$distanceMatrix <- distanceMatrix(vals$modifiedPhyloseq, input$distanceMethod)
                       vals$k <- input$k
                       vals$nmds <- vegan::metaMDS(vals$distanceMatrix, k = input$k,
                                                   try = input$try, trymax = input$tryMax)
                     },
                     error = function(e) {
                       vals$analysisMessage <- "Error: NMDS error."
                     }
                   )
                 }
    )
    
    output$analysisMessage <- renderText(HTML(vals$analysisMessage))
    
    ### Graphic Panel
    source("../server_panel_graphic.R", local = TRUE)
    
    ## Plot Axis Tab
    observe({
      req(vals$nmds)
      choices <- colnames(vegan::scores(vals$nmds))
      updateCheckboxGroupInput(session, "plotAxis2d", choices = choices, selected = choices[1 : 2])
    })
    
    observe({
      req(vals$nmds)
      choices <- colnames(vegan::scores(vals$nmds))
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
    
    ## Output Tab
    output$nmdsOutput <- renderPrint(vals$nmds)
    
    output$downloadNMDSButton <- downloadHandler("nmds.rds", function(file) saveRDS(vals$nmds, file))
    
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
      req(vals$nmds)
      vals$sampleTable <- vegan::scores(vals$nmds) %>%
        as.data.frame()
    })
    
    output$downloadSampleTableButton <- downloadHandler("nmds_sample.tsv",
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
      req(vals$nmds)
    })
    
    observe({
      vals$gg <- NULL
      req(vals$nmds, input$graphicGroupColumn, length(input$plotAxis2d) == 2)
      if(input$graphicGroupColumn == "None") {
        vals$gg <- plotNmdsWithoutGroup()
      } else {
        vals$gg <- plotNmdsWithGroup()
      }
    })
    
    plotNmdsWithoutGroup <- function() {
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
    
    plotNmdsWithGroup <- function() {
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
    
    observe({
      req(vals$k)
      if(vals$k == 2) {
        hideTab("resultTabset", "3D Plot")
      } else {
        showTab("resultTabset", "3D Plot")
      }
    })
    
    output$plot3d <- plotly::renderPlotly(vals$pl)
    
    observe({
      vals$pl <- NULL
      req(vals$nmds, input$graphicGroupColumn3d, length(input$plotAxis3d) == 3)
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














































