library(shiny)
library(tidyverse)
library(shinyjs)
library(dada2)

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
      isFastqMatched()
      disable("reverseConfirmButton")
      req(input$reverseFastq, vals$forwardFastqFiles)
      updateActionButton(session, "reverseConfirmButton", "Confirm")
      enable("reverseConfirmButton")
    })
    
    isFastqMatched <- function() {
      output$reverseFastqMessage <- NULL
      req(input$forwardFastq, input$reverseFastq, vals$forwardFastqFiles)
      if (length(input$reverseFastq) != length(input$forwardFastq)) {
        output$reverseFastqMessage <- renderText(HTML("The number of forward and reverse fastq files are not matched."))
      } else if (isFastqNameMatched) {
        output$reverseFastqMessage <- renderText(HTML("The name of forward and reverse fastq files are not matched."))
      }
    }
    
    isFastqNameMatched <- function() {
      map2_lgl(input$forwardFastq, input$reverseFastq, ~{
        matchFastqName()
      }) 
    }
    
    observeEvent(input$reverseConfirmButton, {
      vals$reverseFastqFiles <- input$reverseFastq
      updateActionButton(session, "reverseConfirmButton", "Confirmed")
      disable("reverseConfirmButton")
    })
    
    #### Forward Fastq Tab
    observe({
      req(vals$forwardFastqFiles)
      appendTab(
        "tabsetPipeline",
        tabPanel(
          title = "Forward Fastq",
          tags$br(),
          selectInput("forwardFq", "Quality Profile", NULL),
          plotOutput("forwardQp")
        )
      )
    })
    
    observe({
      hideTab("tabsetPipeline", "Forward Fastq")
      req(vals$forwardFastqFiles)
      showTab("tabsetPipeline", "Forward Fastq")
    })
    
    observe({
      req(vals$forwardFastqFiles)
      updateSelectInput(session, "forwardFq", choices = `names<-`(vals$forwardFastqFiles, basename(vals$forwardFastqFiles)))
    })
    
    observe({
      output$forwardQp <- NULL
      req(vals$forwardFastqFiles, input$forwardFq)
      output$forwardQp <- renderPlot(plotQualityProfile(input$forwardFq))
    })
  }
)
