library(shiny)
library(tidyverse)
library(shinyjs)

shinyServer(
  function(input, output, session) {
    vals <- reactiveValues()
    vals$forwardFastqFiles <- NULL
    vals$reverseFastqFiles <- NULL
    
    ### Load Fastq Files
    source("../server_panel_load_fastq.R", local = TRUE)
    
    #### Forward
    observe({
      vals$forwardFastqFiles <- NULL
      disable("forwardConfirmButton")
      req(input$forwardFastq)
      updateActionButton(session, "forwardConfirmButton", "Confirm")
      enable("forwardConfirmButton")
    })
    
    observeEvent(input$forwardConfirmButton, {
      vals$forwardFastqFiles <- input$forwardFastq
      updateActionButton(session, "forwardConfirmButton", "Confirmed")
      disable("forwardConfirmButton")
    })
    
    #### Reverse
    observe({
      vals$reverseFastqFiles <- NULL
      disable("reverseConfirmButton")
      req(input$reverseFastq)
      updateActionButton(session, "reverseConfirmButton", "Confirm")
      enable("reverseConfirmButton")
    })
    
    observeEvent(input$reverseConfirmButton, {
      vals$reverseFastqFiles <- input$reverseFastq
      updateActionButton(session, "reverseConfirmButton", "Confirmed")
      disable("reverseConfirmButton")
    })
  }
)
