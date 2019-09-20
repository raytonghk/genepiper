library(shiny)
library(tidyverse)
library(phyloseq)
library(shinyjs)

shinyServer(
  function(input, output, session) {
    vals <- reactiveValues()
    
    output$dataReady <- reactive(isTruthy(vals$modifiedPhyloseq))
    outputOptions(output, "dataReady", suspendWhenHidden = FALSE)
    
    output$taxRank <- reactive(vals$taxRank)
    outputOptions(output, "taxRank", suspendWhenHidden = FALSE)
    
    source("../function_phyloseq.R")
    
    ### Load Data Panel
    source("../server_panel_load_data.R", local = TRUE)
    
    ### Filter Panel
    source("../server_panel_filter.R", local = TRUE)
    
    ### Parameter Panel
    source("../server_panel_parameter.R", local = TRUE)
    
    observe({
      req(vals$filteredPhyloseq)
      vals$modifiedPhyloseq <- NULL
    })
    
    serverTaxRank()
    
    ## General Tab
    # prepareDataButton
    observeEvent(input$prepareDataButton,
                 {
                   vals$prepareDataMessage <- NULL
                   vals$modifiedPhyloseq <- NULL
                   vals$taxRank <- NULL
                   tryCatch(
                     {
                       req(vals$filteredPhyloseq, input$taxRank, input$abundanceType)
                       vals$modifiedPhyloseq <- agglomerateTaxa(vals$filteredPhyloseq, input$taxRank) %>%
                         transformCount(input$abundanceType)
                       vals$taxRank <- input$taxRank
                     },
                     error = function(e) {
                       vals$prepareDataMessage <- "Error: Prepare data error."
                     }
                   )
                 }
    )
    
    output$prepareDataMessage <- renderText(HTML(vals$prepareDataMessage))
    
    ## Table Y Tab
    observe({
      hideTab("parametersTabset", "Table Y")
      req(vals$modifiedPhyloseq)
      showTab("parametersTabset", "Table Y")
    })
    
    # rankFilterY
    observe({
      req(vals$taxRank, vals$modifiedPhyloseq)
      choices <- c(rank_names(vals$modifiedPhyloseq), "OTU")
      choices <- choices[1 : which(choices == vals$taxRank)]
      updateSelectInput(session, "rankFilterY", choices = choices)
    })
    
    # rankFilterLabelY
    observe({
      req(vals$modifiedPhyloseq, input$rankFilterY, input$rankFilterY != vals$taxRank)
      choices <- get_taxa_unique(vals$modifiedPhyloseq, input$rankFilterY)
      updateCheckboxGroupInput(session, "rankFilterLabelY", choices = choices, selected = choices)
    })
    
    # tableYTaxaLabels
    observe({
      req(vals$modifiedPhyloseq, input$rankFilterY, input$rankFilterY == vals$taxRank)
      choices <- taxaNames(vals$modifiedPhyloseq, vals$taxRank)
      updateCheckboxGroupInput(session, "tableYTaxaLabels", choices = choices)
    })
    
    observe({
      req(vals$modifiedPhyloseq, input$rankFilterY, input$rankFilterY != vals$taxRank, length(input$rankFilterLabelY) > 0)
      choices <- subsetTaxaPhyloseq(vals$modifiedPhyloseq, input$rankFilterY, input$rankFilterLabelY) %>%
        taxaNames(vals$taxRank)
      updateCheckboxGroupInput(session, "tableYTaxaLabels", choices = choices)
    })
    
    # analysisButtonY
    observe({
      vals$tableYOrdination <- NULL
      req(vals$tableY)
    })
    
    observeEvent(input$analysisButtonY,
                 {
                   vals$tableYMessage <- NULL
                   vals$tableYOrdination <- NULL
                   vals$ordinateMethodY <- NULL
                   tryCatch(
                     {
                       req(vals$tableY)
                       nf <- min(3, ncol(vals$tableY))
                       vals$tableYOrdination <- switch(input$ordinateMethodY,
                                                       pca = ade4::dudi.pca(vals$tableY, scale = input$scaleY, scannf = FALSE, nf = nf),
                                                       coa = ade4::dudi.coa(vals$tableY, scannf = FALSE, nf = nf),
                                                       pco = ade4::dudi.pco(vegan::vegdist(vals$tableY, input$distanceMethodY), scannf = FALSE, nf = nf),
                                                       nsc = ade4::dudi.nsc(vals$tableY, scannf = FALSE, nf = nf))
                       vals$ordinateMethodY <- input$ordinateMethodY
                     },
                     error = function(e) {
                       vals$tableYMessage <- "Error: Table Y error."
                     }
                   )
                 }
    ) 
    
    # tableYMessage
    output$tableYMessage <- renderText(HTML(vals$tableYMessage))
    
    observe({
      vals$tableYMessage <- NULL
      req(vals$modifiedPhyloseq)
      if(length(input$tableYTaxaLabels) < 2) {
        vals$tableYMessage <- "Error: The number of feature must more than 1."
      }
    })
    
    ## Table X Tab
    observe({
      hideTab("parametersTabset", "Table X")
      req(vals$modifiedPhyloseq, vals$ordinateMethodY)
      showTab("parametersTabset", "Table X")
    })
    
    # tableXMetaLabels
    observe({
      req(vals$modifiedPhyloseq)
      updateCheckboxGroupInput(session, "tableXMetaLabels", choices = variableNames(vals$modifiedPhyloseq))
    })
    
    # ordinateMethodX
    observe({
      req(vals$modifiedPhyloseq, length(input$tableXMetaLabels) > 0)
      if(all(input$tableXMetaLabels %in% characterVariableNames(vals$modifiedPhyloseq))) {
        choices <- list("Multiple Correspondence Analysis" = "acm")
      } else if(all(input$tableXMetaLabels %in% numericVariableNames(vals$modifiedPhyloseq))) {
        choices <- list("Principal Component Analysis" = "pca",
                        "Principal Coordinates Analysis" = "pco")
      } else if(any(input$tableXMetaLabels %in% characterVariableNames(vals$modifiedPhyloseq)) &&
                any(input$tableXMetaLabels %in% numericVariableNames(vals$modifiedPhyloseq))) {
        choices <- list("Hill & Smith Method" = "hillsmith")
      } else {
        choices <- NULL
      }
      updateSelectInput(session, "ordinateMethodX", choices = choices)
    })
    
    # analysisButtonX
    observe({
      vals$tableXOrdination <- NULL
      req(vals$tableX)
    })
    
    observeEvent(input$analysisButtonX,
                 {
                   vals$tableXMessage <- NULL
                   vals$tableXOrdination <- NULL
                   tryCatch(
                     {
                       req(vals$tableX, vals$tableYOrdination)
                       rowWeight <- rep(1, nrow(vals$tableX))
                       nf <- min(3, ncol(vals$tableX))
                       if(vals$ordinateMethodY %in% c("coa", "nsc")) {
                         rowWeight <- vals$tableYOrdination$lw[1 : nrow(vals$tableX)]
                       }
                       vals$tableXOrdination <- switch(input$ordinateMethodX,
                                                       acm = ade4::dudi.acm(convertDataFrameColumnToFactor(vals$tableX, colnames(vals$tableX)), rowWeight, FALSE, nf = nf),
                                                       pca = ade4::dudi.pca(vals$tableX, rowWeight, scale = input$scaleX, scannf = FALSE, nf = nf),
                                                       pco = ade4::dudi.pco(vegan::vegdist(vals$tableX, input$distanceMethodX), rowWeight, FALSE, nf = nf),
                                                       hillsmith = ade4::dudi.hillsmith(convertDataFrameColumnToFactor(vals$tableX, colnames(vals$tableX)[colnames(vals$tableX) %in% characterVariableNames(vals$modifiedPhyloseq)]), 
                                                                                        rowWeight, FALSE, nf = nf))
                     }
                   )
                 }
    )
    
    # tableXMessage
    output$tableXMessage <- renderText(HTML(vals$tableXMessage))
    
    observe({
      vals$tableXMessage <- NULL
      req(vals$modifiedPhyloseq)
      if(length(input$tableXMetaLabels) < 2) {
        vals$tableXMessage <- "Error: The number of feature must more than 1."
      }
    })
    
    ## Co-Inertia Tab
    observe({
      hideTab("parametersTabset", "Co-Inertia")
      req(vals$tableYOrdination, vals$tableXOrdination)
      showTab("parametersTabset", "Co-Inertia")
    })
    
    observe({
      vals$cia <- NULL
      req(vals$tableYOrdination, vals$tableXOrdination)
    })
    
    observeEvent(input$ciaButton,
                 {
                   vals$ciaMessage <- NULL
                   tryCatch(
                     {
                       req(vals$tableYOrdination, vals$tableXOrdination)
                       ordinationX <- vals$tableXOrdination
                       ordinationY <- vals$tableYOrdination
                       vals$cia <- ade4::coinertia(ordinationX, ordinationY, FALSE, min(ordinationX$nf, ordinationY$nf))
                     },
                     error = function(e) {
                       vals$ciaMessage <- "Error: Co-inertia error."
                     }
                   )
                   
                 }
    )
    
    ### Graphic Panel
    source("../server_panel_graphic.R", local = TRUE)
    
    ## Graphic_Group Tab
    observe({
      hideTab("graphicTabset", "Group")
      req(input$plotType %in% c("full", "ciaWeight", "cia"))
      showTab("graphicTabset", "Group")
    })
    
    
    ### Result
    ## Table Y Tab
    observe({
      req(input$parametersTabset == "Table Y")
      updateTabsetPanel(session, "resultTabset", selected = "Table Y")
    })
    
    ## Table Y_Table Tab
    observe({
      req(input$parametersTabset == "Table Y")
      updateTabsetPanel(session, "tableYTabset", selected = "Table")
    })
    
    output$tableY <- DT::renderDataTable({
      req(vals$tableY)
      DT::datatable(vals$tableY, options = list(scrollX = TRUE))
    })
    
    observe({
      vals$tableY <- NULL
      req(vals$modifiedPhyloseq, length(input$tableYTaxaLabels) > 1)
      vals$tableY <- otuDataFrameWithTaxaRowname(vals$modifiedPhyloseq, vals$taxRank) %>%
        t() %>%
        as.data.frame() %>%
        .[, input$tableYTaxaLabels]
    })
    
    ## Table Y_Output Tab
    observe({
      req(vals$tableYOrdination)
      updateTabsetPanel(session, "tableYTabset", selected = "Output")
    })
    
    output$tableYOutput <- renderPrint(vals$tableYOrdination)
    
    output$downloadTableYDudi <- downloadHandler("dudi_table_y.rds", function(file) saveRDS(vals$tableYOrdination, file))
    
    ## Table X Tab
    observe({
      req(input$parametersTabset == "Table X")
      updateTabsetPanel(session, "resultTabset", selected = "Table X")
    })
    
    ## Table X_Table Tab
    observe({
      req(input$parametersTabset == "Table X")
      updateTabsetPanel(session, "tableXTabset", selected = "Table")
    })
    
    output$tableX <- DT::renderDataTable({
      req(vals$tableX)
      DT::datatable(vals$tableX, options = list(scrollX = TRUE))
    })
    
    observe({
      vals$tableX <- NULL
      req(vals$modifiedPhyloseq, length(input$tableXMetaLabels) > 1)
      vals$tableX <- get_variable(vals$modifiedPhyloseq, input$tableXMetaLabels) %>%
        as.data.frame()
    })
    
    ## Table X_Output Tab
    observe({
      req(vals$tableXOrdination)
      updateTabsetPanel(session, "tableXTabset", selected = "Output")
    })
    
    output$tableXOutput <- renderPrint(vals$tableXOrdination)
    
    output$downloadTableXDudi <- downloadHandler("dudi_table_x.rds", function(file) saveRDS(vals$tableXOrdination, file))
    
    ## Co-Inertia Tab
    observe({
      req(input$parametersTabset == "Co-Inertia")
      updateTabsetPanel(session, "resultTabset", selected = "Co-Inertia")
    })
    
    ## Co-Inertia_Output Tab
    observe({
      req(vals$cia)
      updateTabsetPanel(session, "ciaTabset", selected = "Output")
    })
    
    output$ciaOutput <- renderPrint(vals$cia)
    
    output$downloadCIAButton <- downloadHandler("cia.rds", function(file) saveRDS(vals$cia, file))
    
    ## Co-Inertia_Plot Tab
    source("../function_plot.R", local = TRUE)
    
    output$ciaPlot <- renderPlot({
      req(vals$ggList, vals$layoutMatrix)
      gridExtra::grid.arrange(grobs = vals$ggList, layout_matrix = vals$layoutMatrix, 
                              width = rep(1, ncol(vals$layoutMatrix)), height = rep(1, nrow(vals$layoutMatrix)))
    })
    
    observe({
      vals$ggList <- NULL
      vals$layoutMatrix <- NULL
      req(vals$cia, input$plotType)
      vals$ggList <- switch(input$plotType,
                            full = plotFull(),
                            ciaWeight = plotCiaWeights(),
                            cia = plotCias(),
                            axes = plotAxes(),
                            weight = plotWeights(),
                            eigen = plotEigen()
                            )
    })
    
    plotFull <- function() {
      vals$layoutMatrix <- matrix(c(1, 4, 4, 2, 4, 4, 3, 6, 5), ncol = 3, byrow = TRUE)
      list(plotAxis(vals$cia$aX, c("AxcX1", "AxcX2"), "Axes Table X"), plotAxis(vals$cia$aY, c("AxcY1", "AxcY2"), "Axes Table Y"), plotEigenValues(),
           plotCia(), plotWeight(vals$cia$c1, c("CS1", "CS2"), "Canonical Weight Table X"), plotWeight(vals$cia$l1, c("RS1", "RS2"), "Canonical Weight Table Y"))
    }
    
    plotCiaWeights <- function() {
      vals$layoutMatrix <- matrix(c(1,1,1,1,3,2), ncol = 2, byrow = TRUE)
      list(plotCia(), plotWeight(vals$cia$c1, c("CS1", "CS2"), "Canonical Weight Table X"), plotWeight(vals$cia$l1, c("RS1", "RS2"), "Canonical Weight Table Y"))
    }
    
    plotCias <- function() {
      vals$layoutMatrix <- matrix(c(1))
      list(plotCia())
    }
    
    plotAxes <- function() {
      vals$layoutMatrix <- matrix(c(2, 1), byrow = TRUE, ncol = 2)
      list(plotAxis(vals$cia$aX, c("AxcX1", "AxcX2"), "Axes Table X"), plotAxis(vals$cia$aY, c("AxcY1", "AxcY2"), "Axes Table Y"))
    }
    
    plotWeights <- function() {
      vals$layoutMatrix <- matrix(c(2, 1), byrow = TRUE, ncol = 2)
      list(plotWeight(vals$cia$c1, c("CS1", "CS2"), "Canonical Weight Table X"), plotWeight(vals$cia$l1, c("RS1", "RS2"), "Canonical Weight Table Y"))
    }
    
    plotEigen <- function() {
      vals$layoutMatrix <- matrix(c(1))
      list(plotEigenValues())
    }
    
    plotAxis <- function(table, axis, title) {
      gg <- ggplot() +
        ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1)) +
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 0)
      plotArrowLine(gg, table, axis, TRUE, input$lineSize, input$labelSize) +
        coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1)) +
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              title = element_text(size = input$labelSize + 8)) +
        ggtitle(title)
    } 
    
    plotEigenValues <- function() {
      gg <- ggplot() +
        geom_bar(aes(x = seq_along(vals$cia$eig), y = vals$cia$eig), stat = "identity") +
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              title = element_text(size = input$labelSize + 8)) +
        ggtitle("Eigenvalues")
      formatSquareGgAspectRatio(gg)
    }
    
    plotWeight <- function(table, axis, title) {
      gg <- ggplot() +
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 0)
      plotArrowLine(gg, table, axis, TRUE, input$lineSize, input$labelSize) %>%
        formatSquareGg(FALSE) +
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              title = element_text(size = input$labelSize + 8)) +
        ggtitle(title)
    }
    
    plotCia <- function() {
      plotTable <- rownames_to_column(vals$cia$mX, "Sample") %>%
        bind_cols(vals$cia$mY) %>%
        rowwise() %>%
        mutate(meanX = mean(c(NorS1, NorS11)), meanY = mean(c(NorS2, NorS21))) %>%
        ungroup()
      if(input$graphicGroupColumn != "None") {
        groupColumn <- input$graphicGroupColumn
        plotTable <- mutate(plotTable, !!as.name(input$graphicGroupColumn) := groupFactor(vals$modifiedPhyloseq, input$graphicGroupColumn, FALSE))
      } else {
        groupColumn <- NULL
      }
      gg <- ggplot() +
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 0) +
        geom_point(aes_string(x = "NorS1" , y = "NorS2", color = groupColumn), data = plotTable, size = input$dotSize) +
        geom_segment(aes_string(x = "NorS1", y = "NorS2", xend = "NorS11", yend = "NorS21", color = groupColumn), arrow = arrow(), data = plotTable, size = input$lineSize) +
        geom_label(aes_string(x = "meanX", y = "meanY", label = "Sample", color = groupColumn), data = plotTable, size = input$labelSize) +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              title = element_text(size = input$labelSize + 8))
      gg <- formatSquareGg(gg, FALSE)
      if(input$plotLegend == FALSE) {
        gg <- gg +
          guides(color = FALSE)
      }
      gg
    }
    
    # downloadDialogButton
    source("../dialog_download.R", local = TRUE)
    
    observeEvent(input$downloadDialogButton, showModal(downloadImageDialog()))
    
    output$imageDownloadButton <- downloadHandler(paste0(input$imageFileName, ".png"),
                                                  content = function(file) {
                                                    req(vals$ggList)
                                                    png(file, input$imageWidth * 300, input$imageHeight * 300)
                                                    gridExtra::grid.arrange(grobs = vals$ggList, layout_matrix = vals$layoutMatrix, 
                                                                            width = rep(1, ncol(vals$layoutMatrix)), height = rep(1, nrow(vals$layoutMatrix)))
                                                    dev.off()
                                                    removeModal()
                                                  }
    )
  }
)















































