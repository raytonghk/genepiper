library(shiny)
library(tidyverse)
library(shinyjs)

options(shiny.maxRequestSize = 1000*1024^2)

shinyServer(
  function(input, output, session) {
    vals <- reactiveValues()
    
    showTreeDesc <- function(model) {
      cat("Sequence file =", vals$filename, "\n")
      cat("Model =", model)
    }
    
    ### Sequence File
    observe(
      {
        req(input$fileType, input$filepath$datapath)
        vals$filepathMessage <- NULL
        vals$seqs <- NULL
        tryCatch(
          {
            vals$seqs <- phangorn::read.phyDat(input$filepath$datapath, input$fileType)
          },
          error = function(e) {
            vals$filepathMessage <- "Error: Cannot load file."
          }
        )
      }
    )
    
    output$filepathMessage <- renderText(HTML(vals$filepathMessage))
    
    ### Build Trees 
    observeEvent(
      input$buildTreesButton,
      {
        req(input$subModel, input$doGammaModel, input$doInvarSites, vals$seqs)
        vals$buildTreesMessage <- NULL
        vals$trees <- NULL
        vals$filename <- NULL
        tryCatch(
          {
            vals$trees <- phangorn::modelTest(vals$seqs, model = input$subModel, G = input$doGammaModel, I = input$doInvarSites)
            vals$filename <- input$filepath$name
          },
          error = function(e) {
            vals$buildTreesMessage <- "Cannot build trees. Please ensure the file is an aligned sequence file."
          }
        )
      }
    )
    
    output$buildTreesMessage <- renderText(HTML(vals$buildTreesMessage))
    
    ### Tree Models
    output$treeModelsTable <- DT::renderDataTable(
      {
        req(vals$trees)
        DT::datatable(vals$trees, selection = "single", caption = "Please select:", options = list(scrollX = TRUE)) %>%
          DT::formatRound(c("logLik", "AIC", "AICw", "AICc", "AICcw", "BIC"), 4)
      }
    )
    
    ### Tree
    output$treeDesc <- renderPrint(
      {
        req(vals$selectedModel)
        showTreeDesc(vals$selectedModel)
      }
    )
    
    observe(
      {
        req(input$treeModelsTable_rows_selected)
        vals$selectedModel <- NULL
        try(
          {
            vals$selectedModel <- vals$trees[input$treeModelsTable_rows_selected,] %>%
              unlist() %>%
              .[1]
          },
          silent = TRUE
        )
      }
    )
    
    observe(
      {
        req(vals$trees, vals$selectedModel)
        vals$tree <- NULL
        try(
          {
            modelEnv <- attr(vals$trees, "env")
            treeName <- paste0("tree_", vals$selectedModel)
            vals$tree <- eval(get(treeName, modelEnv), modelEnv) %>%
              phangorn::midpoint()
            vals$treeHeight <- max(length(vals$tree$tip.label) * 15, 1000)
            output$treePlot <- renderPlot(plot(vals$tree), height = vals$treeHeight)
          },
          silent = TRUE
        )
      }
    )
    
    observe(
      {
        disable("downloadTreeButton")
        req(vals$tree)
        enable("downloadTreeButton")
      }
    )
    
    output$downloadTreeButton <- downloadHandler(
      "tree.tre",
      function(file) {
        ape::write.tree(vals$tree, file)
      }
    )
  }
)
