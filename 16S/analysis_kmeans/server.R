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
    serverDistanceMethod()
    
    # plotButton
    observeEvent(input$plotButton,
                 {
                   vals$modifiedPhyloseq <- NULL
                   vals$plotMessage <- NULL
                   vals$plotTable <- NULL
                   tryCatch(
                     {
                       req(vals$filteredPhyloseq, input$taxRank, input$abundanceType)
                       vals$modifiedPhyloseq <- agglomerateTaxa(vals$filteredPhyloseq, input$taxRank) %>%
                         transformCount(input$abundanceType)
                       vals$plotTable <- getPlotTable()
                     },
                     error = function(e) {
                       vals$plotMessage <- "Error: Plot table error."
                     }
                   )
                 }
    )
    
    getPlotTable <- function() {
      switch(input$dimensionReductionMethod,
             ca = caPlot(),
             dca = dcaPlot(),
             pca = pcaPlot(),
             pcoa = pcoaPlot(),
             nmds = nmdsPlot())
    }
    
    caPlot <- function() {
      otuDataFrame(vals$modifiedPhyloseq) %>%
        t() %>%
        vegan::decorana(ira = 1) %>%
        summary(display = "sites") %>%
        .$`site.scores` %>%
        as.data.frame()
    }
    
    dcaPlot <- function() {
      otuDataFrame(vals$modifiedPhyloseq) %>%
        t() %>%
        vegan::decorana() %>%
        summary(display = "sites") %>%
        .$`site.scores` %>%
        as.data.frame()
    }
    
    pcaPlot <- function() {
      pca <- otuDataFrame(vals$modifiedPhyloseq) %>%
        t() %>%
        prcomp()
      lam <- pca$sdev * sqrt(nsamples(vals$modifiedPhyloseq))
      vegan::scores(pca, display = "sites") %>%
        as.data.frame() %>%
        {`rownames<-`(map2_df(., lam, ~{.x / .y}), rownames(.))}
    }
    
    pcoaPlot <- function() {
      ape::pcoa(distanceMatrix(vals$modifiedPhyloseq, input$distanceMethod)) %>%
        .$vectors %>%
        as.data.frame()
    }
    
    nmdsPlot <- function() {
      if(nsamples(vals$modifiedPhyloseq) > 5) {
        vegan::metaMDS(distanceMatrix(vals$modifiedPhyloseq, input$distanceMethod), k = (nsamples(vals$modifiedPhyloseq) - 2) / 2) %>%
          vegan::scores() %>%
          as.data.frame()
      } else {
        vals$plotMessage <- "Error: The number of sample for NMDS must more than 5."
      }
    }
    
    output$plotMessage <- renderText(HTML(vals$plotMessage))
    
    ### K-Means Parameters Panel
    source("../server_panel_graphic.R", local = TRUE)
    
    ## Plot Axis Tab
    observe({
      req(vals$plotTable)
      choices <- colnames(vals$plotTable)
      updateCheckboxGroupInput(session, "plotAxis", choices = choices, selected = choices[1:2])
    })
    
    ## Graphic Tab
    observe({
      hideTab("kmeansTabset", "Graphic")
      req(input$resultTabset %in% c("Plot", "Calinski-Harabasz Index (K-Means Only)"))
      showTab("kmeansTabset", "Graphic")
    })
    
    ### Result
    ## Output Tab
    output$kMeansOutput <- renderPrint(vals$kMeans)
    
    observe({
      vals$kMeansMessage <- NULL
      vals$kMeans <- NULL
      tryCatch(
        {
          req(vals$plotTable, input$plotAxis, input$kMeansType, input$k)
          vals$kMeans <- getKMeans(input$kMeansType, input$k)
        },
        error = function(e) {
          vals$kMeansMessage <- "Error: K-Means error."
        }
      )
    })
    
    getKMeans <- function(type, k) {
      table <- vals$plotTable[, input$plotAxis] %>%
        as.matrix()
      switch(type,
             km = kmeans(table, centers = k, algorithm = input$kMeansAlgorithm),
             kkm = kernlab::kkmeans(table, centers = k, kernel = input$kernel),
             sc = kernlab::specc(table, centers = k, kernel = input$kernel) 
      )
    }
    
    output$kMeansMessage <- renderText(HTML(vals$kMeansMessage))
    
    output$donwloadKMeans <- downloadHandler("kmeans.rds", function(file) saveRDS(vals$kMeans, file))
    
    ## Plot Tab
    source("../function_plot.R", local = TRUE)
    output$plotDimensionReduction <- renderPlot(print(vals$formatedGg))

    observe({
      vals$formatedGg <- NULL
      req(vals$gg)
      vals$formatedGg <- formatSquareGg(vals$gg)
    })

    observe({
      vals$gg <- NULL
      req(vals$plotTable, vals$kMeans)
      vals$gg <- plotKMeans()
    })
    
    plotKMeans <- function() {
      plotTable <- rownames_to_column(vals$plotTable, "Sample") %>%
        addKMeansClusterToPlotTable(vals$kMeans)
      gg <- plotDotSampleWithGroup(ggplot(), plotTable, input$plotAxis, "Cluster", input$labelSample, input$sampleDotSize, input$sampleLabelSize)
      if(input$plotCenter) {
        gg <- plotKmeansCenter(gg, vals$kMeans)
      }
      if(input$plotConvexHull) {
        gg <- plotConvexHull(gg, plotTable, input$plotAxis, "Cluster")
      }
      if(input$plotSpider) {
        gg <- plotSpider(gg, plotTable, input$plotAxis, "Cluster", input$spiderLineSize, input$spiderLabelSize)
      }
      if(input$plotEllipse) {
        gg <- plotEllipse(gg, plotTable, input$plotAxis, "Cluster", input$ellipseType, input$ellipseSignif, input$ellipseLineSize)
      }
      gg
    }
    
    addKMeansClusterToPlotTable <- function(plotTable, kmeans) {
      cluster <- kMeansCluster(kmeans)
      mutate(plotTable, Cluster = factor(cluster))
    }
    
    kMeansCluster <- function(kmeans) {
      if(input$kMeansType != "km") {
        kmeans@`.Data`
      } else {
        kmeans$cluster
      }
    }
    
    plotKmeansCenter <- function(gg, kmeans) {
      centerTable <- kmeansCenterTable(kmeans)
      gg +
        geom_point(aes(x = x, y = y), size = input$centerDotSize, color = "black", shape = 3, data = centerTable)
    }
    
    kmeansCenterTable <- function(kmeans) {
      if(input$kMeansType == "km") {
        kmeans$centers %>%
          as.data.frame() %>%
          `colnames<-`(c("x", "y"))
      } else {
        kernlab::centers(kmeans) %>%
          as.data.frame() %>%
          `colnames<-`(c("x", "y"))
      }
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
    
    ## Comparison Tab
    observe({
      req(vals$modifiedPhyloseq)
      updateSelectInput(session, "groupColumnToCompare", choices = characterVariableNames(vals$modifiedPhyloseq))
    })
    
    output$compareOutput <- renderPrint(
      {
        req(input$groupColumnToCompare, vals$adjustedRandIndex)
        cat("Group Column:", input$groupColumnToCompare, "\n\n")
        cat("Adjusted Rand Index:", vals$adjustedRandIndex, "\n\n")
      }
    )
    
    observe({
      vals$adjustedRandIndex
      req(vals$kMeans, input$groupColumnToCompare)
      vals$adjustedRandIndex <- mclust::adjustedRandIndex(kMeansCluster(vals$kMeans), groupFactor(vals$modifiedPhyloseq, input$groupColumnToCompare, FALSE))
    })
    
    ## Calinski-Harabasz Index Tab
    observe({
      hideTab("resultTabset", "Calinski-Harabasz Index (K-Means Only)")
      req(input$kMeansType == "km")
      showTab("resultTabset", "Calinski-Harabasz Index (K-Means Only)")
    })
    
    output$cHIndexPlot <- renderPlot(print(vals$cHIndexPlot))
    
    observe({
      vals$cHIndexPlot <- NULL
      req(vals$plotTable, length(input$plotAxis) == 2, input$kMeansType == "km")
      vals$cHIndexPlot <- cHIndexPlot()
    })
    
    cHIndexPlot <- function() {
      cHTable <- cHTable()
      plotCHIndex(cHTable)
    }
    
    cHTable <- function() {
      map_df(2 : min(nrow(vals$plotTable), 20), ~{
        cluster <- getKMeans(input$kMeansType, .x) %>%
          kMeansCluster()
        data.frame(k = .x, chIndex = fpc::calinhara(vals$plotTable[, input$plotAxis], cluster))
      })
    }
    
    plotCHIndex <- function(cHTable) {
      ggplot(cHTable, aes(x = k, y = chIndex)) +
        geom_line(size = input$lineSizeCh) +
        labs(y = "Calinski-Harabasz Index") +
        ggtitle(input$titleCh) +
        theme(axis.text.x = element_text(size = input$xAxisTextSizeCh,
                                         angle = input$xAxisTextAngleCh,
                                         hjust = ifelse(input$xAxisTextAngleCh != 0, 1, 0.5)),
              axis.title.x = element_text(size = input$xAxisTextSizeCh + 1),
              axis.text.y = element_text(size = input$yAxisTextSizeCh,
                                         angle = input$yAxisTextAngleCh),
              axis.title.y = element_text(size = input$yAxisTextSizeCh + 1))
    }
    
    observeEvent(input$downloadCHIndexDialogButton, showModal(downloadImageDialog("CH")))
    
    output$imageDownloadButtonCH <- downloadHandler(filename = function() {
      paste0(input$imageFileNameCH, ".png")
    },
                                                    content = function(file) {
                                                      ggsave(file, vals$cHIndexPlot, "png", height = input$imageHeightCH, width = input$imageWidthCH)
                                                      removeModal()
                                                    }
    )
  }
)












































