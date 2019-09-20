library(shiny)
library(tidyverse)
library(phyloseq)
library(shinyjs)

shinyServer(
  function(input, output, session) {
    vals <- reactiveValues()
    
    output$prcReady <- reactive(isTruthy(vals$prc))
    outputOptions(output, "prcReady", suspendWhenHidden = FALSE)
    
    source("../function_phyloseq.R")
    
    ### Load Data Panel
    source("../server_panel_load_data.R", local = TRUE)
    
    ### Filter Panel
    source("../server_panel_filter.R", local = TRUE)
    
    ### Parameter Panel
    source("../server_panel_parameter.R", local = TRUE)
    
    observe(
      {
        req(vals$filteredPhyloseq)
        vals$prc <- NULL
      }
    )
    
    serverTaxRank()
    
    # treatmentColumn
    observe(
      {
        req(vals$filteredPhyloseq)
        updateSelectInput(session, "treatmentColumn", choices = variableNames(vals$filteredPhyloseq))
      }
    )
    
    # timeColumn
    observe(
      {
        req(vals$filteredPhyloseq)
        updateSelectInput(session, "timeColumn", choices = numericVariableNames(vals$filteredPhyloseq))
      }
    )
    
    # analysisButton
    observeEvent(
      input$analysisButton,
      {
        vals$prcMessage <- NULL
        vals$prc <- NULL
        tryCatch(
          {
            req(vals$filteredPhyloseq, input$taxRank, input$abundanceType, input$treatmentColumn, input$timeColumn)
            treatmentValues()
            timeValues()
            vals$prc <- agglomerateTaxa(vals$filteredPhyloseq, input$taxRank) %>%
              transformCount(input$abundanceType) %>%
              otuDataFrameWithTaxaRowname(input$taxRank) %>%
              t() %>%
              logMatrix() %>%
              vegan::prc(vals$treatment, vals$time)
          },
          error = function(e) {
            if(e == "treatment") {
              vals$prcMessage <- "Error: Treatment values contain NA."
            } else if(e == "time") {
              vals$prcMessage <- "Error: Time values contain NA."
            } else {
              vals$prcMessage <- "Error: Principal Response Curve error."
            }
          }
        )
      }
    )
    
    logMatrix <- function(mat) {
      if(input$logMatrix) {
        log(mat + 1)
      } else {
        mat
      }
    }
    
    treatmentValues <- function() {
      value <- factor(get_variable(vals$filteredPhyloseq, input$treatmentColumn))
      if(any(is.na(value))) {
        stop("treatment")
      } else {
        vals$treatment <- value
      }
    }
    
    timeValues <- function() {
      value <- factor(get_variable(vals$filteredPhyloseq, input$timeColumn))
      if(any(is.na(value))) {
        stop("time")
      } else {
        vals$time <- value
      }
    }
    
    output$prcMessage <- renderText(HTML(vals$prcMessage))
    
    ### Result
    ## Output Tab
    output$prcOutput <- renderPrint(vals$prc)
    
    output$downloadPrcButton <- downloadHandler("prc.rds",
                                                function(file) {
                                                  saveRDS(vals$prc, file)
                                                })
    
    ## Coefficient Tab
    output$coeffTable <- DT::renderDataTable({
      summary(vals$prc) %>%
        .$coefficients %>%
        round(4) %>%
        DT::datatable(list(scrollX = TRUE))
    })
    
    ## Species Weight Tab
    output$specWeight <- DT::renderDataTable(
      {
        summary(vals$prc) %>%
          .$sp %>%
          round(4) %>%
          {data.frame(Weight = .)} %>%
          DT::datatable(list(scrollX = TRUE))
      }
    )
    
    ## Plot Tab
    source("../function_plot.R", local = TRUE)
    output$prcPlot <- renderPlot(print(vals$formatedGg))
    
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
        req(vals$prc)
        vals$gg <- plotPrc()
      }
    )
    
    plotPrc <- function() {
      req(vals$prc)
      sPrc <- summary(vals$prc)
      sPrc$coefficients %>%
        as.data.frame() %>%
        rownames_to_column("Treatment") %>%
        gather("Time", "Coeff", -Treatment) %>%
        ggplot() +
        geom_hline(yintercept = 0, color = "grey", size = input$lineSize) +
        geom_line(aes(x = as.numeric(Time), y = Coeff, group = Treatment, color = Treatment), size = input$lineSize) +
        scale_y_continuous(sec.axis = sec_axis(trans = ~ ., breaks = sPrc$sp, labels = names(sPrc$sp)),
                           limits = c(min(sPrc$sp), max(sPrc$sp))) +
        labs(y = "Effect", x = "Time")
    }
    
    # downloadDialogButton
    source("../dialog_download.R", local = TRUE)
    
    observeEvent(input$downloadDialogButton, showModal(downloadImageDialog()))
    
    output$imageDownloadButton <- downloadHandler(paste0(input$imageFileName, ".png"),
                                                  content = function(file) {
                                                    ggsave(file, vals$formatedGg, "png", height = input$imageHeight, width = input$imageWidth)
                                                    removeModal()
                                                  }
    )
    
    ## Anova Tab
    output$anovaOutput <- renderPrint(vals$anova)
    
    observe(
      {
        vals$anova <- NULL
        req(vals$prc, vals$treatment)
        ctrl <- permute::how(plots = permute::Plots(vals$treatment, "free"),
                             within = permute::Within("series"),
                             nperm = 99)
        vals$anova <- anova(vals$prc, permutation = ctrl, first = TRUE)
      }
    )
  }
)
























