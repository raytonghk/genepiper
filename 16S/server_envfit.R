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
  updateCheckboxGroupInput(session, "envfitVectorLabel", choices = choices, selected = choices)
})

observe({
  req(vals$envfit)
  choices <- rownames(vegan::scores(vals$envfit, display = "factor"))
  updateCheckboxGroupInput(session, "envfitFactorLabel", choices = choices, selected = choices)
})