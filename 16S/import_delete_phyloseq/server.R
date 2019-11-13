library(shiny)
library(tidyverse)
library(shinyjs)
library(phyloseq)

shinyServer(
  function(input, output, session) {
    vals <- reactiveValues()
    
    source("../function_phyloseq.R")
    
    ### Load Data Panel
    source("../server_panel_load_data.R", local = TRUE)
    
    observe(
      {
        req(vals$project, vals$dataLabel)
        path <- getProjectPath(input$project)
        vals$filepath <- paste0(path, "/", input$dataLabel, "_phyloseq.rds")
      }
    )
    
    ### Delete
    observe(
      {
        disable("deleteDialogButton")
        req(vals$phyloseq)
        enable("deleteDialogButton")
      }
    )
    
    observeEvent(
      input$deleteDialogButton,
      {
        showModal(deleteDialog())
      }
    )
    
    deleteDialog <- function() {
      modalDialog(
        textOutput("proposedDataLabel"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("deleteButton", "Delete!")
        )
      )
    }
    
    output$proposedDataLabel <- renderText(HTML(vals$proposedDataLabel))
    
    observe(
      {
        vals$proposedDataLabel <- NULL
        req(vals$project, vals$dataLabel)
        vals$proposedDataLabel <- paste0("Delete ", vals$project, "/", vals$dataLabel, "?")
      }
    )
    
    observeEvent(
      input$deleteButton,
      {
        req(vals$filepath)
        system(paste0("rm ", vals$filepath))
        updateSelectInput(session, "dataLabel", choices = dataNames(vals$project))
        output$dataDetails <- NULL
        removeModal()
      }
    )
  }
)
