library(shiny)
library(tidyverse)
library(phyloseq)
library(shinyjs)

shinyServer(
  function(input, output, session) {
    vals <- reactiveValues()
    
    output$hcaReady <- reactive(isTruthy(vals$hca))
    outputOptions(output, "hcaReady", suspendWhenHidden = FALSE)
    
    source("../function_phyloseq.R")
    
    ### Load Data Panel
    source("../server_panel_load_data.R", local = TRUE)
    
    ### Filter Panel
    source("../server_panel_filter.R", local = TRUE)
    
    ### Parameter Panel
    source("../server_panel_parameter.R", local = TRUE)
    
    observe({
      req(vals$filteredPhyloseq)
      vals$hca <- NULL
    })
    
    serverTaxRank()
    serverDistanceMethod()
    
    # analysisButton
    observeEvent(input$analysisButton,
                 {
                   vals$analysisMessage <- NULL
                   vals$modifiedPhyloseq <- NULL
                   vals$hca <- NULL
                   vals$distanceMethod <- NULL
                   tryCatch(
                     {
                       req(vals$filteredPhyloseq, input$taxRank, input$abundanceType, input$agglomerateMethod, input$distanceMethod)
                       vals$modifiedPhyloseq <- agglomerateTaxa(vals$filteredPhyloseq, input$taxRank) %>%
                         transformCount(input$abundanceType)
                       distanceFunction<- hcaDistanceFunction(input$distanceMethod)
                       vals$hca <- pvclust::pvclust(otuDataFrame(vals$modifiedPhyloseq), input$agglomerateMethod, distanceFunction, nboot = input$nboot)
                       vals$distanceMethod <- input$distanceMethod
                     },
                     error = function() {
                       vals$analysisMessage <- "Error: HCA error."
                     }
                   )
                 }
    )
    
    hcaDistanceFunction <- function(distanceMethod) {
      switch (distanceMethod,
        unifrac = function(x) {
          x <- as.matrix(x) %>%
          {`rownames<-`(., gsub("\\..*", "", rownames(.)))}
          GUniFrac::GUniFrac(t(x), phy_tree(vals$modifiedPhyloseq), alpha = 1) %>%
            .$unifracs %>%
            .[,, "d_UW"] %>%
            as.dist() %>%
            `attr<-`("method", distanceMethod)
        },
        wunifrac = function(x) {
          x <- as.matrix(x) %>%
          {`rownames<-`(., gsub("\\..*", "", rownames(.)))}
          GUniFrac::GUniFrac(t(x), phy_tree(vals$modifiedPhyloseq), alpha = 1) %>%
            .$unifracs %>%
            .[,, "d_1"] %>%
            as.dist() %>%
            `attr<-`("method", distanceMethod)
        },
        function(x) {
          vegan::vegdist(t(x), distanceMethod)
        }
      )
    }
    
    ### Graphic Panel
    source("../server_panel_graphic.R", local = TRUE)
    
    ## Group Tab
    # groupStatistics
    output$groupStatistics <- renderPrint(printStatistics(vals$groupStatistics))
    
    observe({
      vals$groupStatistics <- NULL
      req(vals$modifiedPhyloseq, vals$distanceMatrix, vals$groupFactor)
      vals$groupStatistics <- fpc::cluster.stats(vals$distanceMatrix, vals$groupFactor)
    })
    
    output$downloadGroupStatisticsButton <- downloadHandler("group_stat.rds", function(file) saveRDS(vals$groupStatistics, file))
    
    ## PVClust Tab
    # pvrectStatistics
    output$pvrectStatistics <- renderPrint(printStatistics(vals$pvrectStatistics))
    
    observe({
      vals$pvrectStatistics <- NULL
      req(vals$modifiedPhyloseq, vals$hca, vals$distanceMatrix, input$plotPVRect, input$pvrectAlpha, input$graphicGroupColumn != "None")
      clust <- pvclust::pvpick(vals$hca, input$pvrectAlpha)
      if(!is.null(clust[[1]])) {
        clust <- clust %>%
          .$clusters %>%
          {
            map2(., seq_along(.), ~{
              `names<-`(rep(.y, length(.x)), .x)
            })
          } %>%
          unlist() %>%
          .[sample_names(vals$modifiedPhyloseq)] %>%
          `names<-`(., sample_names(vals$modifiedPhyloseq)) %>%
          .[order(.)] %>%
          accumulate(~{ifelse(is.na(.y), .x + 1, .y)})
        
        gf <- vals$groupFactor %>%
          `names<-`(sample_names(vals$modifiedPhyloseq)) %>%
          .[names(clust)]
        
        vals$pvrectStatistics <- fpc::cluster.stats(vals$distanceMatrix, clust, gf)
      }
    })
    
    output$downloadPVRectStatisticsButton <- downloadHandler("pvrect_stat.rds", function(file) saveRDS(vals$pvrectStatistics, file))
    
    ## Cut Tab
    # cutStatistics
    output$cutStatistics <- renderPrint(printStatistics(vals$cutStatistics))
    
    observe({
      vals$cutStatistics <- NULL
      req(vals$modifiedPhyloseq, vals$hca, vals$distanceMatrix, input$cutK)
      vals$cutStatistics <- fpc::cluster.stats(vals$distanceMatrix, cutree(vals$hca$hclust, input$cutK), vals$groupFactor)
    })
    
    output$downloadCutStatisticsButton <- downloadHandler("cut_stat.rds", function(file) saveRDS(vals$cutStatistics, file))
    
    observe({
      vals$distanceMatrix <- NULL
      req(vals$modifiedPhyloseq, vals$distanceMethod)
      vals$distanceMatrix <- distanceMatrix(vals$modifiedPhyloseq, vals$distanceMethod)
    })
    
    observe({
      vals$groupFactor <- NULL
      req(vals$modifiedPhyloseq, input$graphicGroupColumn != "None")
      vals$groupFactor <- groupFactor(vals$modifiedPhyloseq, input$graphicGroupColumn, FALSE) %>%
        as.numeric()
    })
    
    printStatistics <- function(groupStatistics) {
      cat("Number Of Samples:\t\t\t", groupStatistics$n, "\n")
      cat("Number Of Clusters:\t\t\t", groupStatistics$cluster.number, "\n")
      cat("Clusters' Size:\t\t\t\t", groupStatistics$cluster.size, "\n")
      cat("Clusters' Diameter:\t\t\t", groupStatistics$diameter, "\n")
      cat("Average Distance Within Clusters:\t", groupStatistics$average.distance, "\n")
      cat("Median Distance Within Clusters:\t", groupStatistics$median.distance, "\n")
      cat("Separation:\t\t\t\t", groupStatistics$separation, "\n")
      cat("Average Toother:\t\t\t", groupStatistics$average.toother, "\n")
      cat("Within Cluster Sum Of Square:\t\t", groupStatistics$within.cluster.ss, "\n")
      cat("Average Silhouette Width:\t\t", groupStatistics$avg.silwidth, "\n")
      cat("Correlation Between Distances:\t\t", groupStatistics$pearsongamma, "\n")
      cat("Calinski And Harabasz Index:\t\t", groupStatistics$ch, "\n")
      if(!is.null(groupStatistics$corrected.rand)) {
        cat("\nCompare To Group:\n")
        cat("Corrected Rand Index:\t\t\t", groupStatistics$corrected.rand, "\n")
      }
      if(!is.null(groupStatistics$vi)) {
        cat("Variation Of Information Index:\t\t", groupStatistics$vi, "\n")
      }
      invisible(groupStatistics)
    }
    
    ### Result
    ## Output Tab
    output$hcaOutput <- renderPrint(vals$hca)
    
    output$downloadHCAButton <- downloadHandler("hca.rds", function(file) saveRDS(vals$hca, file))
    
    ## Plot Tab
    output$plotHca <- renderPlot(plotHca(vals$hca))
    
    plotHca <- function(hca) {
      dendrogram <- as.dendrogram(hca$hclust, hang = 0.1)
      
      if(input$graphicGroupColumn == "None") {
        dendextend::`labels_cex<-`(dendrogram, value = input$labelSize) %>%
          plot(edgePar = list(lwd = input$lineSize))
      } else {
        dendextend::`labels_colors<-`(dendrogram, value = groupFactor(vals$modifiedPhyloseq, input$graphicGroupColumn, FALSE)[hca$hclust$order] %>%
                                          as.numeric()) %>%
          dendextend::`labels_cex<-`(value = input$labelSize) %>%
          plot(edgePar = list(lwd = input$lineSize))
      }
      if(input$plotPVClust) {
        text(hca, cex = input$pvclustLabelSize)
      }
      if(input$plotPVRect) {
        pvclust::pvrect(hca, input$pvrectAlpha, lwd = input$pvrectLineSize)
      }
      if(input$plotCut) {
        dendextend::rect.dendrogram(dendrogram, input$cutK, border = "blue", lwd = input$cutLineSize)
      }
    }
    
    # downloadDialogButton
    source("../dialog_download.R", local = TRUE)
    
    observeEvent(input$downloadDialogButton, showModal(downloadImageDialog()))
    
    output$imageDownloadButton <- downloadHandler(filename = function() {
      paste0(input$imageFileName, ".png")
    },
                                                  content = function(file) {
                                                    png(file, input$imageWidth * 300, input$imageHeight * 300)
                                                    plotHca(vals$hca)
                                                    dev.off()
                                                    removeModal()
                                                  }
    )
  }
)








































