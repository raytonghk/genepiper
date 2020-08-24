library(shiny)
library(tidyverse)
library(shinyjs)
library(dada2)

shinyServer(
  function(input, output, session) {
    vals <- reactiveValues()
    vals$forwardFastqFiles <- NULL
    vals$reverseFastqFiles <- NULL
    vals$fastqSampleNames <- NULL
    
    ### Load Fastq Files
    source("../server_panel_load_fastq.R", local = TRUE)
    
    #### Forward
    observe({
      vals$forwardFastqFiles <- NULL
      vals$reverseFastqFiles <- NULL
      disable("forwardConfirmButton")
      req(input$forwardFastq)
      updateActionButton(session, "forwardConfirmButton", "Confirm")
      updateActionButton(session, "reverseConfirmButton", "Confirm")
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
      fsns <- isFastqMatched()
      req(input$reverseFastq, vals$forwardFastqFiles, fsns)
      updateActionButton(session, "reverseConfirmButton", "Confirm")
      enable("reverseConfirmButton")
    })
    
    isFastqMatched <- function() {
      output$reverseFastqMessage <- NULL
      vals$fastqSampleNames <- NULL
      req(input$forwardFastq, input$reverseFastq, vals$forwardFastqFiles)
      if (length(input$reverseFastq) != length(input$forwardFastq)) {
        output$reverseFastqMessage <- renderText(HTML("The number of forward and reverse fastq files are not matched."))
        return(NULL)
      } 
      if (!isFastqNameMatched()) {
        output$reverseFastqMessage <- renderText(HTML("The name of forward and reverse fastq files are not matched."))
        return(NULL)
      }
      fsns <- map2_chr(input$forwardFastq, input$reverseFastq, ~{
        getFastqSampleName(.x, .y)
      })
      if (any(duplicated(fsns))) {
        output$reverseFastqMessage <- renderText(HTML("The sample names duplicated."))
        return(NULL)
      }
      vals$fastqSampleNames <- fsns
    }
    
    isFastqNameMatched <- function() {
      map2_lgl(input$forwardFastq, input$reverseFastq, ~{
        fsn <- getFastqSampleName(.x, .y)
        ifelse(is.null(fsn), FALSE, TRUE)
      }) %>%
      all()
    }
    
    getFastqSampleName <- function(x, y) {
      spl <- ifelse(strsplit(x, "[^[:alnum:]]+")[[1]] == strsplit(y, "[^[:alnum:]]+")[[1]], 
                    strsplit(x, "[^[:alnum:]]+")[[1]], 
                    NA_character_)
      if (all(!is.na(spl)) | is.na(spl[1])) {
        return(NULL)
      }
      spl[1 : min(which(is.na(spl))) - 1] %>%
        paste(collapse = "_")
    }
    
    observeEvent(input$reverseConfirmButton, {
      vals$reverseFastqFiles <- input$reverseFastq
      updateActionButton(session, "reverseConfirmButton", "Confirmed")
      disable("reverseConfirmButton")
    })
    
    #### Forward Fastq Tab
    observe({
      removeTab("tabsetPipeline", "Forward Fastq")
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
      updateTabsetPanel(session, "tabsetPipeline", selected = "Forward Fastq")
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
    
    #### Reverse Fastq Tab
    observe({
      removeTab("tabsetPipeline", "Reverse Fastq")
      req(vals$reverseFastqFiles)
      appendTab(
        "tabsetPipeline",
        tabPanel(
          title = "Reverse Fastq",
          tags$br(),
          selectInput("reverseFq", "Quality Profile", NULL),
          plotOutput("reverseQp")
        )
      )
      updateTabsetPanel(session, "tabsetPipeline", selected = "Reverse Fastq")
    })
    
    observe({
      req(vals$reverseFastqFiles)
      updateSelectInput(session, "reverseFq", choices = `names<-`(vals$reverseFastqFiles, basename(vals$reverseFastqFiles)))
    })
    
    observe({
      output$reverseQp <- NULL
      req(vals$reverseFastqFiles, input$reverseFq)
      output$reverseQp <- renderPlot(plotQualityProfile(input$reverseFq))
    })
    
    #### Filter And Trim Tab
    observe({
      removeTab("tabsetPipeline", "Filter And Trim")
      req(vals$forwardFastqFiles, vals$reverseFastqFiles)
      appendTab(
        "tabsetPipeline",
        tabPanel(
          title = "Filter And Trim",
          tags$br(),
          tags$span(numericInput("truncQF", "truncQ", 2),
                    numericInput("truncQR", NULL, 2))
        )
      )
    })
  }
)
