output$filterColumnType <- reactive("None")
outputOptions(output, "filterColumnType", suspendWhenHidden = FALSE)

### Filter Sample Column
observe({
  req(vals$phyloseq)
  updateSelectInput(session, "filterSampleColumn", choices = c("Please Select", variableNames(vals$phyloseq)))
})

# reset select column when command line entered
observe({
  req(input$filterSampleText != "")
  updateSelectInput(session, "filterSampleColumn", selected = "Please Select")
})

# reset command line when select column
observe({
  req(input$filterSampleColumn != "Please Select")
  updateTextInput(session, "filterSampleText", value = "")
})

# check filter column type
observe({
  req(vals$phyloseq, input$filterSampleColumn, input$filterSampleColumn != "Please Select")
  output$filterColumnType <- reactive(variableType(vals$phyloseq, input$filterSampleColumn))
})

### Filter Sample Checkbox
observe({
  req(vals$phyloseq, input$filterSampleColumn,
      input$filterSampleColumn != "Please Select",
      variableType(vals$phyloseq, input$filterSampleColumn) == "character"
  )
  updateCheckboxGroupInput(session, "filterSampleCheckbox",
                           choices = unique(get_variable(vals$phyloseq, input$filterSampleColumn)),
                           selected = unique(get_variable(vals$phyloseq, input$filterSampleColumn)))
})

### Filter Button
observeEvent(input$filterButton,
             {
               vals$filterSampleMessage <- NULL
               vals$filteredPhyloseq <- NULL
               tryCatch(
                 {
                   req(vals$phyloseq)
                   vals$filteredPhyloseq <- getFilteredPhyloseq(vals$phyloseq, input$filterSampleText, input$filterSampleColumn)
                 },
                 error = function(e) {
                   vals$filterSampleMessage <- "Error: Filter is not correct."
                 }
               )
             }
)

getFilteredPhyloseq <- function(phyloseq, filterText, filterColumn) {
  if(filterText == "" && filterColumn != "Please Select") {
    filterText <- filterTextByColumn(phyloseq, filterColumn)
  }
  eval(parse(text = paste0("subset_samples(phyloseq, ", filterText, ")")))
}

filterTextByColumn <- function(phyloseq, filterColumn) {
  variableType <- variableType(phyloseq, filterColumn)
  if(variableType == "numeric") {
    filterTextByNumericColumn(filterColumn)
  } else if(variableType == "character") {
    filterTextByCharacterColumn(filterColumn)
  }
}

filterTextByNumericColumn <- function(filterColumn) {
  paste(filterColumn, input$filterSampleOperator, input$filterSampleNumeric)
}

filterTextByCharacterColumn <- function(filterColumn) {
  paste(filterColumn, "%in% c(", paste(shQuote(input$filterSampleCheckbox), collapse = ", "), ")")
}

output$filterSampleMessage <- renderText(vals$filterSampleMessage)

### Filtered Phyloseq Details
output$filteredPhyloseqDetails <- renderPrint(showFilteredPhyloseq(vals$filteredPhyloseq))

showFilteredPhyloseq <- function(phyloseq) {
  cat("Filtered Phyloseq: \n\n")
  phyloseq::show(phyloseq)
}











