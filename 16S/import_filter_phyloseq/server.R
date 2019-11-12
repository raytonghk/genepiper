library(shiny)
library(tidyverse)
library(shinyjs)
library(phyloseq)

shinyServer(
  function(input, output, session) {
    vals <- reactiveValues()
    
    source("../function_phyloseq.R")
    source("../function_import_data.R")
    
    ### Load Data Panel
    source("../server_panel_load_data.R", local = TRUE)
    
    ### Filter Panel
    source("../server_panel_filter.R", local = TRUE)
    
    ### Project
    source("../dialog_project.R", local = TRUE)
    
    updateSelectInput(session, "projectToSave", choices = projectNames())
    
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
        req(vals$moduleFilteredPhyloseq, input$dataLabelToSave)
        enable("saveButton")
      }
    )
    
    observeEvent(
      input$saveButton,
      {
        vals$saveMesssage <- NULL
        filepath <- paste0(getProjectPath(input$projectToSave), "/", input$dataLabelToSave, "_phyloseq.rds")
        if(file.exists(filepath)) {
          vals$saveMessage <- "Data label exists!"
        } else {
          saveRDS(vals$moduleFilteredPhyloseq, filepath)
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
        req(vals$moduleFilteredPhyloseq)
        enable("downloadButton")
      }
    )
    
    output$downloadPhyloseqButton <- downloadHandler("phyloseq.rds",
                                                     function(file) {
                                                       saveRDS(vals$moduleFilteredPhyloseq, file)
                                                     }
    )
  }
)









