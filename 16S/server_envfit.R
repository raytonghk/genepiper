output$envfitReady <- reactive(isTruthy(vals$envfit))
outputOptions(output, "envfitReady", suspendWhenHidden = FALSE)

output$envfitOutput <- renderPrint(vals$envfit)

observe({
  req(vals$sampleTable, input$plotAxis2d, length(input$plotAxis2d) == 2)
  vals$envfit <- NULL
  vals$envfitMessage <- NULL
  tryCatch(
    {
      isolate(
        {
          vals$envfit <- vals$sampleTable %>%
            select(one_of(input$plotAxis2d)) %>%
            vegan::envfit(ord = ., env = get_variable(vals$modifiedPhyloseq), na.rm = TRUE)
        }
      )
    },
    error = function(e) {
      vals$envfitMessage <- "Error: Envfit error."
    }
  )
})

output$envfitMessage <- renderText(HTML(vals$envfitMessage))

output$downloadEnvfit <- downloadHandler("envfit.rds", function(file) {
  saveRDS(vals$envfit, file)
})

observe({
  req(vals$envfit)
  choices <- rownames(vegan::scores(vals$envfit, display = "vector"))
  updateCheckboxGroupInput(session, "envfitVectorLabel", choices = choices)
})

observe({
  req(vals$envfit)
  cols <- characterVariableNames(vals$modifiedPhyloseq)
  removeUI("#envfitFactorContainer")
  insertUI("#envfitFactor", "beforeBegin", tags$div(id = "envfitFactorContainer"))
  map(cols, ~{
    id <- gsub("\\.", "_", .x)
    insertUI("#envfitFactorContainer", "beforeEnd",
             tags$div(
               checkboxInput(paste0("envfitFactorCol", id), .x, FALSE),
               conditionalPanel(
                 condition = paste0("input.envfitFactorCol", id, " == true"),
                 checkboxGroupInput(paste0("envfitFactorCol", id, "Label"), NULL, unique(get_variable(vals$modifiedPhyloseq, .x)))
               )
             ))
    observe({
      req(input[[paste0("envfitFactorCol", id)]])
      updateCheckboxGroupInput(session, paste0("envfitFactorCol", id, "Label"), selected = unique(get_variable(vals$modifiedPhyloseq, .x)))
    })
    observe({
      req(input[[paste0("envfitFactorCol", id)]] == FALSE)
      updateCheckboxGroupInput(session, paste0("envfitFactorCol", id, "Label"), selected = c())
    })
  })
})

observe({
  req(vals$modifiedPhyloseq)
  cols <- characterVariableNames(vals$modifiedPhyloseq)
  
  vals$envfitFactorLabel <- NULL
  vals$envfitFactorLabel <- map(cols, ~{
    id <- gsub("\\.", "_", .x)
    if(isTruthy(input[[paste0("envfitFactorCol", id)]])) {
      paste0(.x, input[[paste0("envfitFactorCol", id, "Label")]])
    }
  }) %>%
    unlist()
})















