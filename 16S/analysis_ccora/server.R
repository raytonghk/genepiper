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
    
    # tableYMetaLabels
    observe({
      req(vals$modifiedPhyloseq)
      choices <- numericVariableNames(vals$modifiedPhyloseq)
      updateCheckboxGroupInput(session, "tableYMetaLabels", choices = choices)
    })
    
    # tableYMessage
    output$tableYMessage <- renderText(HTML(vals$tableYMessage))
    
    observe({
      vals$tableYMessage <- NULL
      req(vals$modifiedPhyloseq)
      variableNumber <- length(vals$tableYVariable)
      if(variableNumber < 2) {
        vals$tableYMessage <- "Error: The number of feature must more than 1."
      }
      if(variableNumber > (nsamples(vals$modifiedPhyloseq) - 2)) {
        vals$tableYMessage <- "Error: The numerbe of feature must less than n - 1."
      }
    })
    
    observe({
      vals$tableYVariable <- NULL
      req(input$tableYType)
      vals$tableYVariable <- switch(input$tableYType,
                                    taxa = input$tableYTaxaLabels,
                                    meta = input$tableYMetaLabels,
                                    both = c(input$tableYTaxaLabels, input$tableYMetaLabels)
      )
    })
    
    ## Table X Tab
    observe({
      hideTab("parametersTabset", "Table X")
      req(vals$modifiedPhyloseq)
      showTab("parametersTabset", "Table X")
    })
    
    # rankFilterX
    observe({
      req(vals$taxRank, vals$modifiedPhyloseq)
      choices <- c(rank_names(vals$modifiedPhyloseq), "OTU")
      choices <- choices[1 : which(choices == vals$taxRank)]
      updateSelectInput(session, "rankFilterX", choices = choices)
    })
    
    # rankFilterLabelX
    observe({
      req(vals$modifiedPhyloseq, input$rankFilterX, input$rankFilterX != vals$taxRank)
      choices <- get_taxa_unique(vals$modifiedPhyloseq, input$rankFilterX)
      updateCheckboxGroupInput(session, "rankFilterLabelX", choices = choices, selected = choices)
    })
    
    # tableXTaxaLabels
    observe({
      req(vals$modifiedPhyloseq, input$rankFilterX, input$rankFilterX == vals$taxRank)
      choices <- taxaNames(vals$modifiedPhyloseq, vals$taxRank)
      updateCheckboxGroupInput(session, "tableXTaxaLabels", choices = choices)
    })
    
    observe({
      req(vals$modifiedPhyloseq, input$rankFilterX, input$rankFilterX != vals$taxRank, length(input$rankFilterLabelX) > 0)
      choices <- subsetTaxaPhyloseq(vals$modifiedPhyloseq, input$rankFilterX, input$rankFilterLabelX) %>%
        taxaNames(vals$taxRank)
      updateCheckboxGroupInput(session, "tableXTaxaLabels", choices = choices)
    })
    
    # tableXMetaLabels
    observe({
      req(vals$modifiedPhyloseq)
      choices <- numericVariableNames(vals$modifiedPhyloseq)
      updateCheckboxGroupInput(session, "tableXMetaLabels", choices = choices)
    })
    
    # tableXMessage
    output$tableXMessage <- renderText(HTML(vals$tableXMessage))
    
    observe({
      vals$tableXMessage <- NULL
      req(vals$modifiedPhyloseq)
      variableNumber <- length(vals$tableXVariable)
      if(variableNumber < 2) {
        vals$tableXMessage <- "Error: The number of feature must more than 1."
      }
      if(variableNumber > (nsamples(vals$modifiedPhyloseq) - 2)) {
        vals$tableXMessage <- "Error: The numerbe of feature must less than n - 1."
      }
    })
    
    observe({
      vals$tableXVariable <- NULL
      req(input$tableXType)
      vals$tableXVariable <- switch(input$tableXType,
                                    taxa = input$tableXTaxaLabels,
                                    meta = input$tableXMetaLabels,
                                    both = c(input$tableXTaxaLabels, input$tableXMetaLabels)
      )
    })
    
    ## CCorA Tab
    observe({
      hideTab("parametersTabset", "CCorA")
      req(vals$tableX, vals$tableY)
      showTab("parametersTabset", "CCorA")
    })
    
    # ccoraButton
    observe({
      disable("ccoraButton")
      vals$ccora <- NULL
      req(vals$tableX, vals$tableY)
      enable("ccoraButton")
    })
    
    observeEvent(input$ccoraButton,
                 {
                   vals$ccora <- NULL
                   vals$ccoraMessage <- NULL
                   tryCatch(
                     {
                       req(vals$tableX, vals$tableY)
                       vals$ccora <- vegan::CCorA(vals$tableY, vals$tableX, input$standardiseTableY, input$standardiseTableX)
                     },
                     error = function(e) {
                       vals$ccoraMessage <- "Error: CCorA error."
                     }
                   )
                 }
    )
    
    output$ccoraMessage <- renderText(HTML(vals$ccoraMessage))
    
    ### Graphic Parameters
    ## Plot Axis Tab
    observe({
      req(vals$ccora)
      choices <- colnames(vals$ccora$Cy)
      updateCheckboxGroupInput(session, "plotAxis", choices = choices, selected = choices[1 : 2])
    })
    
    ## Object Tab
    observe({
      hideTab("graphicTabset", "Object")
      req(input$plotType, input$plotType %in% c("Layout", "Objects", "Biplots"))
      showTab("graphicTabset", "Object")
    })
    
    ## Variable Tab
    observe({
      hideTab("graphicTabset", "Variable")
      req(input$plotType, input$plotType %in% c("Layout", "Variables", "Biplots", "Variables Overlay"))
      showTab("graphicTabset", "Variable")
    })
    
    ### Result
    ## Table Y Tab
    observe({
      req(input$parametersTabset == "Table Y")
      updateTabsetPanel(session, "resultTabset", selected = "Table Y")
    })
    
    # tableY
    output$tableY <- DT::renderDataTable({
      req(vals$tableY)
      DT::datatable(vals$tableY, options = list(scrollX = TRUE))
    })
    
    observe({
      vals$tableY <- NULL
      req(vals$modifiedPhyloseq, vals$tableYVariable, length(vals$tableYVariable) >= 2, length(vals$tableYVariable) < (nsamples(vals$modifiedPhyloseq) - 1))
      vals$tableY <- variableTable(vals$tableYVariable)
    })
    
    variableTable <- function(labels) {
      otuDataFrameWithTaxaRowname(vals$modifiedPhyloseq, vals$taxRank) %>%
        t() %>%
        cbind(get_variable(vals$modifiedPhyloseq)) %>%
        as.data.frame() %>%
        .[, labels]
    }
    
    ## Table X Tab
    observe({
      req(input$parametersTabset == "Table X")
      updateTabsetPanel(session, "resultTabset", selected = "Table X")
    })
    
    # tableX
    output$tableX <- DT::renderDataTable({
      req(vals$tableX)
      DT::datatable(vals$tableX, options = list(scrollX = TRUE))
    })
    
    observe({
      vals$tableX <- NULL
      req(vals$modifiedPhyloseq, vals$tableXVariable, length(vals$tableXVariable) >= 2, length(vals$tableXVariable) < (nsamples(vals$modifiedPhyloseq) - 1))
      vals$tableX <- variableTable(vals$tableXVariable)
    })
    
    ## CCorA Tab
    observe({
      req(input$parametersTabset == "CCorA")
      updateTabsetPanel(session, "resultTabset", selected = "CCorA")
    })
    
    observe({
      hideTab("resultTabset", "CCorA")
      req(vals$ccora)
      showTab("resultTabset", "CCorA")
    })
    
    ## CCorA_Output Tab
    output$ccoraOutput <- renderPrint(vals$ccora)
    
    output$downloadCCorAButton <- downloadHandler("ccora.rds", function(file) saveRDS(vals$ccora, file))
    
    ## CCorA_Objects Tab
    ## CCorA_Objects_Table Y Tab
    output$tableYObjects <- DT::renderDataTable({
      req(vals$tableYObjects)
      DT::datatable(vals$tableYObjects, options = list(scrollX = TRUE)) %>%
        DT::formatRound(colnames(vals$tableYObjects), digits = 4)
    })
    
    observe({
      vals$tableYObjects <- NULL
      req(vals$ccora)
      vals$tableYObjects <- as.data.frame(vals$ccora$Cy)
    })
    
    ## CCorA_Objects_Table X Tab
    output$tableXObjects <- DT::renderDataTable({
      req(vals$tableXObjects)
      DT::datatable(vals$tableXObjects, options = list(scrollX = TRUE)) %>%
        DT::formatRound(colnames(vals$tableXObjects), digits = 4)
    })
    
    observe({
      vals$tableXObjects <- NULL
      req(vals$ccora)
      vals$tableXObjects <- as.data.frame(vals$ccora$Cx)
    })
    
    ## CCorA_Variables Tab
    ## CCorA_Variables_Table Y_Y Tab
    output$variablesTableYY <- DT::renderDataTable({
      req(vals$variablesTableYY)
      DT::datatable(vals$variablesTableYY, options = list(scrollX = TRUE)) %>%
        DT::formatRound(colnames(vals$variablesTableYY), digits = 4)
    })
    
    observe({
      vals$variablesTableYY <- NULL
      req(vals$ccora)
      vals$variablesTableYY <- as.data.frame(vals$ccora$corr.Y.Cy)
    })
    
    ## CCorA_Variables_Table Y_X Tab
    output$variablesTableYX <- DT::renderDataTable({
      req(vals$variablesTableYX)
      DT::datatable(vals$variablesTableYX, options = list(scrollX = TRUE)) %>%
        DT::formatRound(colnames(vals$variablesTableYX), digits = 4)
    })
    
    observe({
      vals$variablesTableYX <- NULL
      req(vals$ccora)
      vals$variablesTableYX <- as.data.frame(vals$ccora$corr.X.Cy)
    })
    
    ## CCorA_Variables_Table X_Y Tab
    output$variablesTableXY <- DT::renderDataTable({
      req(vals$variablesTableXY)
      DT::datatable(vals$variablesTableXY, options = list(scrollX = TRUE)) %>%
        DT::formatRound(colnames(vals$variablesTableXY), digits = 4)
    })
    
    observe({
      vals$variablesTableXY <- NULL
      req(vals$ccora)
      vals$variablesTableXY <- as.data.frame(vals$ccora$corr.Y.Cx)
    })
    
    ## CCorA_Variables_Table X_X Tab
    output$variablesTableXX <- DT::renderDataTable({
      req(vals$variablesTableXX)
      DT::datatable(vals$variablesTableXX, options = list(scrollX = TRUE)) %>%
        DT::formatRound(colnames(vals$variablesTableXX), digits = 4)
    })
    
    observe({
      vals$variablesTableXX <- NULL
      req(vals$ccora)
      vals$variablesTableXX <- as.data.frame(vals$ccora$corr.X.Cx)
    })
    
    ## CCorA_Plot Tab
    source("../function_plot.R", local = TRUE)
    
    output$ccoraPlot <- renderPlot({
      req(vals$ggList)
      gridExtra::grid.arrange(grobs = vals$ggList, ncol = 2)
    })
    
    observe({
      vals$ggList <- NULL
      req(vals$ccora, input$plotType, length(input$plotAxis) == 2)
      tryCatch(
        {
          vals$ggList <- switch(input$plotType,
                  Layout = plotLayout(),
                  Objects = plotObjects(),
                  Variables = plotVariables(),
                  Biplots = plotBiplots(),
                  "Variables Overlay" = plotOverlaies())
        },
        error = function(e) {}
      )
    })
    
    plotLayout <- function() {
      list(plotObject(vals$ccora$Cy, "Table Y", "red"), plotObject(vals$ccora$Cx, "Table X", "blue"), plotVariableTable(vals$ccora$corr.Y.Cy, color = "red"), plotVariableTable(vals$ccora$corr.X.Cx, color = "blue"))
    }
    
    plotObjects <- function() {
      list(plotObject(vals$ccora$Cy, "Table Y", "red"), plotObject(vals$ccora$Cx, "Table X", "blue"))
    }
    
    plotVariables <- function() {
      list(plotVariableTable(vals$ccora$corr.Y.Cy, "Table Y", "red"), plotVariableTable(vals$ccora$corr.X.Cx, "Table X", "blue"))
    }
    
    plotBiplots <- function() {
      list(plotBiplot(vals$ccora$Cy, vals$ccora$corr.Y.Cy, "Table Y", "red"), plotBiplot(vals$ccora$Cx, vals$ccora$corr.X.Cx, "Table X", "blue"))
    }
    
    plotOverlaies <- function() {
      list(plotOverlay(vals$ccora$corr.Y.Cy, vals$ccora$corr.X.Cy, "Table Y", "red", "blue"),
           plotOverlay(vals$ccora$corr.X.Cx, vals$ccora$corr.Y.Cx, "Table X", "blue", "red"))
    }
    
    plotObject <- function(table, title = NULL, color = "black") {
      plotDotSampleWithoutGroup(ggplot(), as.data.frame(table), input$plotAxis, input$labelObject, input$objectDotSize, input$objectLabelSize, color) %>%
        formatSquareGg() +
        ggtitle(title)
    }
    
    plotVariableTable <- function(table, title = NULL, color = "black") {
      gg <- ggplot() +
        ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = c(0.5, 1))) +
        geom_vline(aes(xintercept = 0)) +
        geom_hline(aes(yintercept = 0))
      plotArrowLine(gg, as.data.frame(table), input$plotAxis, TRUE, input$variableLineSize, input$variableLabelSize, color, color) %>%
        formatGg() +
        coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1)) +
        labs(x = input$plotAxis[1], y = input$plotAxis[2]) +
        ggtitle(title)
    }
    
    plotBiplot <- function(objectTable, variableTable, title = NULL, color = "black") {
      variableTable <- fitCoordinate(as.data.frame(variableTable), as.data.frame(objectTable), input$plotAxis)
      plotArrowLine(ggplot(), as.data.frame(variableTable), input$plotAxis, TRUE, input$variableLineSize, input$variableLabelSize, color, color) %>%
        plotDotSampleWithoutGroup(as.data.frame(objectTable), input$plotAxis, input$labelObject, input$objectDotSize, input$objectLabelSize, color) %>%
        formatSquareGg() +
        ggtitle(title)
    }
    
    plotOverlay <- function(baseTable, overlayTable, title = NULL, baseColor = "black", overlayColor = "black") {
      gg <- ggplot() +
        ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = c(0.5, 1))) +
        geom_vline(aes(xintercept = 0)) +
        geom_hline(aes(yintercept = 0))
      plotArrowLine(gg, as.data.frame(baseTable), input$plotAxis, TRUE, input$variableLineSize, input$variableLabelSize, baseColor, baseColor) %>%
        plotArrowLine(as.data.frame(overlayTable), input$plotAxis, TRUE, input$variableLineSize, input$variableLabelSize, overlayColor, overlayColor) %>%
        formatGg() +
        coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1)) +
        labs(x = input$plotAxis[1], y = input$plotAxis[2]) +
        ggtitle(title)
    }
    
    # downloadDialogButton
    source("../dialog_download.R", local = TRUE)
    
    observeEvent(input$downloadDialogButton, showModal(downloadImageDialog()))
    
    output$imageDownloadButton <- downloadHandler(paste0(input$imageFileName, ".png"),
                                                  content = function(file) {
                                                    req(vals$ggList)
                                                    png(file, input$imageWidth * 300, input$imageHeight * 300)
                                                    gridExtra::grid.arrange(grobs = vals$ggList, ncol = 2)
                                                    dev.off()
                                                    removeModal()
                                                  }
    )
  }
)












































