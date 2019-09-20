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
    
    observe(
      {
        req(input$filepath$datapath)
        vals$filepathMessage <- NULL
        vals$phyloseq
        isolate(
          {
            tryCatch(
              {
                vals$phyloseq <- readRDS(input$filepath$datapath)
              },
              error = function(e) {
                vals$filepathMessage <- "Import file error!"
              }
            )
          }
        )
      }
    )
    
    observe(
      {
        req(vals$phyloseq)
        phyloseq <- vals$phyloseq
        vals$filepathMessage <- NULL
        if(class(phyloseq) == "phyloseq") {
          vals$validPhyloseq <- phyloseq
        } else {
          vals$filepathMessage <- "Import data is not phyloseq"
        }
      }
    )
    
    output$filepathMessage <- renderText(HTML(vals$filepathMessage))
    
    ### Phyloseq
    observe(
      {
        req(vals$validPhyloseq)
        vals$phyloseqMessage <- NULL
        tryCatch(
          {
            vals$formatedPhyloseq <- formatPhyloseq(vals$validPhyloseq)
          },
          error = function(e) {
            vals$phyloseqMessage <- "Format phyloseq error!"
          }
        )
      }
    )
    
    output$phyloseqDetails <- renderPrint(vals$formatedPhyloseq)
    
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
        req(vals$formatedPhyloseq, input$dataLabel)
        enable("saveButton")
      }
    )
    
    observeEvent(
      input$saveButton,
      {
        vals$saveMesssage <- NULL
        filepath <- paste0(getProjectPath(input$project), "/", input$dataLabel, "_phyloseq.rds")
        if(file.exists(filepath)) {
          vals$saveMessage <- "Data label exists!"
        } else {
          saveRDS(vals$formatedPhyloseq, filepath)
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
        req(vals$formatedPhyloseq)
        enable("downloadButton")
      }
    )
    
    output$downloadPhyloseqButton <- downloadHandler("phyloseq.rds",
                                             function(file) {
                                               saveRDS(vals$formatedPhyloseq, file)
                                             }
    )
  }
)








