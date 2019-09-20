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
    source("../function_math.R")
    
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
    
    # plotButton
    observeEvent(input$plotButton,
                 {
                   vals$plotMessage <- NULL
                   vals$plotTable <- NULL
                   vals$index <- NULL
                   tryCatch(
                     {
                       req(vals$filteredPhyloseq, input$taxRank, input$alphaIndex)
                       phyloseq <- agglomerateTaxa(vals$filteredPhyloseq, input$taxRank)
                       vals$plotTable <- plotTable(phyloseq, input$alphaIndex)
                       vals$index <- input$alphaIndex
                     },
                     error = function(e) {
                       vals$plotMessage <- "Error: Create plot table error."
                     }
                   )
                 }
    )
    
    plotTable <- function(phyloseq, index) {
      sampleList <- sampleList(phyloseq)
      map2_df(sampleList, names(sampleList),
              ~{
                steps <- seq(1, length(.x), length.out = min(100, length(.x)))
                index <- unlist(getAlphaIndices(.x, steps, index))
                data.frame(Sample = .y, Step = steps, Index = index)
              })
    }
    
    sampleList <- function(phyloseq) {
      otuTable <- otuDataFrame(phyloseq)
      map(otuTable,
          ~{
            rep(rownames(otuTable), .x) %>%
              sample()
          }
      )
    }
    
    getAlphaIndices <- function(community, steps, index) {
      isolate(
        {
          map(steps,
              ~{
                subsample <- community[1 : .x]
                switch(index,
                       "Richness" = getRichness(subsample),
                       "Shannon Index" = getShannon(subsample),
                       "Inverse Simpson" = getInverseSimpson(subsample),
                       "Coverage" = getCoverage(subsample, input$countAsRare)
                )
              }
          )
        }
      )
    }
    
    output$plotMessage <- renderText(HTML(vals$plotMessage))
    
    # indexPlot
    source("../function_plot.R", local = TRUE)
    
    output$indexPlot <- renderPlot(print(vals$formatedGg))
    
    observe({
      vals$formatedGg <- NULL
      req(vals$gg)
      vals$formatedGg <- formatGg(vals$gg)
    })
    
    observe({
      vals$gg <- NULL
      req(vals$plotTable)
      vals$gg <- vals$plotTable %>%
        ggplot(aes(x = Step, y = Index)) +
        geom_line(aes(group = Sample, color = Sample), size = input$lineSize) +
        labs(y = vals$index)
    })
    
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



















































