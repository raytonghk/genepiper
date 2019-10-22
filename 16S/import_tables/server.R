library(shiny)
library(tidyverse)
library(phyloseq)
library(phangorn)
library(shinyjs)

options(shiny.maxRequestSize = 100*1024^2)

shinyServer(
  function(input, output, session) {
    vals <- reactiveValues()
    
    source("../function_import_data.R")
    
    ### OTU Table File
    observe(
      {
        req(input$otuFilePath$datapath, input$otuFileType)
        vals$otuFilePathMessage <- NULL
        vals$otuTable <- NULL
        isolate(
          {
            tryCatch(
              {
                vals$otuTable <- switch(input$otuFileType,
                                        csv = read_csv(input$otuFilePath$datapath, comment = ""),
                                        tsv = read_tsv(input$otuFilePath$datapath, comment = ""))
              },
              error = function(e) {
                vals$otuFilePathMessage <- "Import file error!"
              }
            )
          }
        )
      }
    )
    
    output$otuFilePathMessage <- renderText(HTML(vals$otuFilePathMessage))
    
    observe(
      {
        req(vals$otuTable)
        vals$otuFilePathMessage <- NULL
        vals$validOtuTable <- NULL
        if(validateOtuTable(vals$otuTable)) {
          vals$validOtuTable <- formatOtuTable(vals$otuTable)
        } else {
          vals$otuFilePathMessage <- "Import file format error!"
        }
      }
    )
    
    output$otuDetails <- renderPrint(otuDetails(vals$otuTable))
    
    ### Taxonomy Table File
    observe(
      {
        req(input$taxFilePath$datapath, input$taxFileType)
        vals$taxFilePathMessage <- NULL
        vals$taxTable <- NULL
        isolate(
          {
            tryCatch(
              {
                vals$taxTable <- switch(input$taxFileType,
                                        csv = read_csv(input$taxFilePath$datapath, comment = ""),
                                        tsv = read_tsv(input$taxFilePath$datapath, comment = ""))
              },
              error = function(e) {
                vals$taxFilePathMessage <- "Import file error!"
              }
            )
          }
        )
      }
    )
    
    output$taxFilePathMessage <- renderText(HTML(vals$taxFilePathMessage))
    
    observe(
      {
        req(vals$taxTable)
        vals$taxFilePathMessage <- NULL
        vals$validTaxTable <- NULL
        if(validateTaxTable(vals$taxTable)) {
          vals$validTaxTable <- formatTaxTable(vals$taxTable)
        } else {
          vals$taxFilePathMessage <- "Import file format error!"
        }
      }
    )
    
    output$taxDetails <- renderPrint(
      {
        req(vals$validTaxTable)
        taxDetails(vals$taxTable)
      }
    )
    
    ### Sample Data File
    observe(
      {
        req(input$samFilePath$datapath, input$samFileType)
        vals$samFilePathMessage <- NULL
        vals$samData <- NULL
        isolate(
          {
            tryCatch(
              {
                vals$samData <- switch(input$samFileType,
                                       csv = read_csv(input$samFilePath$datapath, comment = ""),
                                       tsv = read_tsv(input$samFilePath$datapath, comment = ""))
              },
              error = function(e) {
                vals$samFilePathMessage <- "Import file error!"
              }
            )
          }
        )
      }
    )
    
    output$samFilePathMessage <- renderText(HTML(vals$samFilePathMessage))
    
    observe(
      {
        req(vals$samData)
        vals$samFilePathMessage <- NULL
        vals$validSamData <- NULL
        if(validateSamData(vals$samData)) {
          vals$validSamData <- formatSampleData(vals$samData)
        } else {
          vals$samFilePathMessage <- "Import file format error!"
        }
      }
    )
    
    output$samDetails <- renderPrint(
      {
        req(vals$validSamData)
        sampleDetails(vals$samData)
      }
    )
    
    ### Phylogenetic Tree File
    observe(
      {
        req(input$treeFilePath$datapath)
        vals$treeFilePathMessage <- NULL
        vals$tree <- NULL
        isolate(
          {
            tryCatch(
              {
                vals$tree <- phangorn::midpoint(read_tree(input$treeFilePath$datapath)) %>%
                  formatTree()
              },
              error = function(e) {
                vals$treeFilePathMessage <- "Import file error!"
              }
            )
          }
        )
      }
    )
    
    output$treeDetails <- renderPrint(vals$tree)
    
    ### Phyloseq
    observe(
      {
        req(vals$validOtuTable, vals$validTaxTable)
        vals$phyloseqMessage <- NULL
        if(!all(rownames(vals$validOtuTable) %in% rownames(vals$validTaxTable))) {
          vals$phyloseqMessage <- "Taxa names are not match between OTU table and taxonomy table."
        }
      }
    )
    
    observe(
      {
        req(vals$validOtuTable, vals$validSamData)
        vals$phyloseqMessage <- NULL
        if(!all(colnames(vals$validOtuTable) %in% rownames(vals$validSamData))) {
          vals$phyloseqMessage <- "Sample names are not match between OTU table and sample data."
        }
      }
    )
    
    observe(
      {
        req(vals$validOtuTable, vals$tree)
        vals$phyloseqMessage <- NULL
        if(!all(rownames(vals$validOtuTable) %in% vals$tree$tip.label)) {
          vals$phyloseqMessage <- "Taxa names are not match between OTU table and phylogenetic tree."
        }
      }
    )
    
    output$phyloseqMessage <- renderText(HTML(vals$phyloseqMessage))
    
    observe(
      {
        req(vals$validOtuTable, vals$validTaxTable, vals$validSamData, !isTruthy(vals$tree))
        vals$phyloseq <- NULL
        try(
          {
            vals$phyloseq <- phyloseq(otu_table(as.matrix(vals$validOtuTable), taxa_are_rows = TRUE),
                                      tax_table(as.matrix(vals$validTaxTable)),
                                      sam_data(vals$validSamData))
          },
          silent = TRUE 
        )
        
      }
    )
    
    observe(
      {
        req(vals$validOtuTable, vals$validTaxTable, vals$validSamData, vals$tree)
        vals$phyloseq <- NULL
        try(
          {
            vals$phyloseq <- phyloseq(otu_table(as.matrix(vals$validOtuTable), taxa_are_rows = TRUE),
                                      tax_table(as.matrix(vals$validTaxTable)),
                                      sam_data(vals$validSamData),
                                      phy_tree(vals$tree))
          },
          silent = TRUE
        )
      }
    )
    
    output$phyloseqDetails <- renderPrint(vals$phyloseq)
    
    ### Project
    source("../dialog_project.R", local = TRUE)
    
    updateSelectInput(session, "project", choices = projectNames())
    
    observeEvent(
      input$newProjectButton,
      {
        showModal(newProjectDialog())
      }
    )
    
    ### Save
    observe(
      {
        disable("saveButton")
        req(vals$phyloseq, input$dataLabel)
        enable("saveButton")
      }
    )
    
    observeEvent(
      input$saveButton,
      {
        vals$saveMesssage <- NULL
        path <- getProjectPath(input$project)
        if(!dir.exists(path)) {
          system(paste0("mkdir ", path, " -m 777"))
        }
        
        filepath <- paste0(getProjectPath(input$project), "/", input$dataLabel, "_phyloseq.rds")
        if(file.exists(filepath)) {
          vals$saveMessage <- "Data label exists!"
        } else {
          saveRDS(vals$phyloseq, filepath)
          vals$saveMessage <- "Saved."
          disable("saveButton")
        }
      }
    )
    
    output$saveMessage <- renderText(HTML(vals$saveMessage))
    
    ### Download
    observe(
      {
        disable("downloadButton")
        req(vals$phyloseq)
        enable("downloadButton")
      }
    )
    
    output$downloadPhyloseqButton <- downloadHandler("phyloseq.rds",
                                             function(file) {
                                               saveRDS(vals$phyloseq, file)
                                             }
    )
  }
)







































