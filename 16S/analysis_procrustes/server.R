library(shiny)
library(tidyverse)
library(shinyjs)
library(phyloseq)

shinyServer(
  function(input, output, session) {
    vals <- reactiveValues()
    
    output$dataReady <- reactive(isTruthy(vals$validTargetTable) & isTruthy(vals$validToBeRotatedTable))
    outputOptions(output, "dataReady", suspendWhenHidden = FALSE)
    
    output$dataValidated <- reactive(vals$dataValidated)
    outputOptions(output, "dataValidated", suspendWhenHidden = FALSE)
    
    output$procReady <- reactive(isTruthy(vals$proc))
    outputOptions(output, "procReady", suspendWhenHidden = FALSE)
    
    ### Target Matrix
    output$targetFilepathMessage <- renderText(HTML(vals$targetFilepathMessage))
    
    observe(
      {
        req(input$targetFilepath$datapath, input$targetFileType)
        vals$targetFilepathMessage <- NULL
        vals$targetTable <- NULL
        tryCatch(
          {
            vals$targetTable <- switch(input$targetFileType,
                                       csv = read.table(input$targetFilepath$datapath, sep = ","),
                                       tsv = read.table(input$targetFilepath$datapath, sep = "\t"),
                                       rds = readRDS(input$targetFilepath$datapath))
          },
          error = function(e) {
            vals$targetFilepathMessage <- "Import file error!"
          }
        )
      }
    )
    
    observe(
      {
        req(vals$targetTable)
        vals$targetFilepathMessage <- NULL
        if(is.numeric(as.matrix(vals$targetTable))) {
          vals$validTargetTable <- vals$targetTable[order(rownames(vals$targetTable)),]
        } else {
          vals$targetFilepathMessage <- "Error: Imported table is not numeric."
        }
      }
    )
    
    ### Matrix To Be Rotated
    output$rotatedFilepathMessage <- renderText(HTML(vals$rotatedFilepathMessage))
    
    observe(
      {
        req(input$rotatedFilepath$datapath, input$rotatedFileType)
        vals$rotatedFilepathMessage <- NULL
        vals$toBeRotatedTable <- NULL
        tryCatch(
          {
            vals$toBeRotatedTable <- switch(input$rotatedFileType,
                                            csv = read.table(input$rotatedFilepath$datapath, sep = ","),
                                            tsv = read.table(input$rotatedFilepath$datapath, sep = "\t"),
                                            rds = readRDS(input$rotatedFilepath$datapath))
          },
          error = function(e) {
            vals$rotatedFilepathMessage <- "Import file error!"
          }
        )
      }
    )
    
    observe(
      {
        req(vals$toBeRotatedTable)
        vals$rotatedFilepathMessage <- NULL
        if(is.numeric(as.matrix(vals$toBeRotatedTable))) {
          vals$validToBeRotatedTable <- vals$toBeRotatedTable[order(rownames(vals$toBeRotatedTable)),]
        } else {
          vals$rotatedFilepathMessage <- "Error: Imported table is not numeric."
        }
      }
    )
    
    ### Parameters
    output$procMessage <- renderText(HTML(vals$procMessage))
    
    observe(
      {
        req(vals$validTargetTable, vals$validToBeRotatedTable)
        vals$procMessage <- NULL
        if(!identical(rownames(vals$validTargetTable), rownames(vals$validToBeRotatedTable))) {
          vals$procMessage <- "Error: The row names of two matrix is not match."
          vals$dataValidated <- FALSE
        } else {
          vals$dataValidated <- TRUE
        }
      }
    )
    
    observe(
      {
        req(vals$validTargetTable)
        axes <- colnames(vals$validTargetTable)
        updateCheckboxGroupInput(session, "targetTableAxes", choices = axes, selected = axes[1:2])
      }
    )
    
    output$targetTableAxesMessage <- renderText(HTML(vals$targetTableAxesMessage))
    
    observe(
      {
        vals$targetTableAxesMessage <- NULL
        req(length(input$targetTableAxes) != 2)
        vals$targetTableAxesMessage <- "Please select exactly 2 axes."
      }
    )
    
    observe(
      {
        req(vals$validToBeRotatedTable)
        axes <- colnames(vals$validToBeRotatedTable)
        updateCheckboxGroupInput(session, "rotatedTableAxes", choices = axes, selected = axes[1:2])
      }
    )
    
    output$rotatedTableAxesMessage <- renderText(HTML(vals$rotatedTableAxesMessage))
    
    observe(
      {
        vals$rotatedTableAxesMessage <- NULL
        req(length(input$rotatedTableAxes) != 2)
        vals$rotatedTableAxesMessage <- "Please select exactly 2 axes."
      }
    )
    
    observe(
      {
        disable("analysisButton")
        req(length(input$targetTableAxes) == 2, length(input$rotatedTableAxes) == 2)
        enable("analysisButton")
      }
    )
    
    observeEvent(
      input$analysisButton,
      {
        vals$proc <- NULL
        vals$procMessage <- NULL
        tryCatch(
          {
            X <- vals$validTargetTable[, input$targetTableAxes]
            Y <- vals$validToBeRotatedTable[, input$rotatedTableAxes]
            scale <- input$scale
            symmetric <- input$symmetric
            vals$proc <- vegan::procrustes(X, Y, scale = scale, symmetric = symmetric)
          },
          error = function(e) {
            vals$procMessage <- "Error: Procrustes analysis error!"
          }
        )
      }
    )
    
    ### Result
    ## Output Tab
    output$procOutput <- renderPrint(vals$proc)
    
    output$downloadProcButton <- downloadHandler("procrustes.rds",
                                                 function(file) {
                                                   saveRDS(vals$proc, file)
                                                 })
    
    ## Protest Tab
    output$protestOutput <- renderPrint(vals$protest)
    
    output$procMessage <- renderText(HTML(vals$protestMessage))
    
    observe(
      {
        req(vals$proc)
        vals$protestMessage <- NULL
        vals$protest <- NULL
        tryCatch(
          {
            X <- vals$validTargetTable[, input$targetTableAxes]
            Y <- vals$validToBeRotatedTable[, input$rotatedTableAxes]
            vals$protest <- vegan::protest(X, Y)
          },
          error = function(e) {
            vals$protestMessage <- "Error: Protest error!"
          }
        )
      }
    )
    
    output$downloadProtestButton <- downloadHandler("protest.rds",
                                                    function(file) {
                                                      saveRDS(vals$protest, file)
                                                    })
    
    ## Mantel Tab
    output$mantelOutput <- renderPrint(vals$mantel)
    
    output$mantelMessage <- renderText(HTML(vals$mantelMessage))
    
    observe(
      {
        req(vals$proc)
        vals$mantelMessage <- NULL
        vals$mantel <- NULL
        tryCatch(
          {
            X <- vals$validTargetTable[, input$targetTableAxes]
            Y <- vals$validToBeRotatedTable[, input$rotatedTableAxes]
            method <- tolower(input$corrMethod)
            vals$mantel <- vegan::mantel(dist(X), dist(Y), method)
          },
          error = function() {
            vals$mantelMessage <- "Error: Mantel error!"
          }
        )
      }
    )
    
    output$downloadMantelButton <- downloadHandler("mantel.rds",
                                                   function(file) {
                                                     saveRDS(vals$mantel, file)
                                                   })
    
    ## Plot Tab
    source("../function_plot.R", local = TRUE)
    output$procPlot <- renderPlot(print(vals$formatedGg))
    
    output$plotMessage <- renderText(HTML(vals$plotMessage))
    
    observe(
      {
        vals$formatedGg <- NULL
        req(vals$gg)
        vals$formatedGg <- formatGg(vals$gg)
      }
    )
    
    observe(
      {
        vals$gg <- NULL
        vals$plotMessage <- NULL
        req(vals$proc, length(input$targetTableAxes) == 2, length(input$rotatedTableAxes) == 2)
        tryCatch(
          {
            X <- as.data.frame(vals$proc$X)
            Y <- as.data.frame(vals$proc$Yrot)
            rot <- as.data.frame(vals$proc$rotation)
            vals$gg <- ggplot() +
              geom_point(aes(x = Y[, 1], y = Y[, 2]), size = input$dotSize) +
              geom_segment(aes(x = Y[, 1], y = Y[, 2], xend = X[, 1], yend = X[, 2]),
                           arrow = arrow(type = "closed", length = unit(2, "mm")), color = "blue", size = input$lineSize) +
              geom_abline(intercept = c(0, 0), slope = rot[1, 2] / rot[1, 1]) +
              geom_abline(intercept = c(0, 0), slope = rot[2, 2] / rot[2, 1]) +
              labs(x = "Dimension 1", y = "Dimension 2")
          },
          error = function(e) {
            vals$plotMessage <- "Error: Plot procrustes error."
          }
        )
      }
    )
    
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
