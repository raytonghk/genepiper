### Extdata
EXTDATA_PATH <- "/srv/shiny-server/extdata/"

getFastqFiles <- function(filterString) {
  tryCatch({
    list.files(EXTDATA_PATH, filterString, recursive = TRUE)
  }, error = function(e){})
}

### Load Forward Fastq Files
observe({
  req(input$forwardFilterString)
  updateCheckboxGroupInput(session, "forwardFastq", choices = getFastqFiles(input$forwardFilterString))
})

observeEvent(input$forwardSelectAllButton, {
  updateCheckboxGroupInput(session, "forwardFastq", selected = getFastqFiles(input$forwardFilterString))
})

observeEvent(input$forwardCleanAllButton, {
  updateCheckboxGroupInput(session, "forwardFastq", choices = getFastqFiles(input$forwardFilterString))
})

### Load Reverse Fastq Files
observe({
  req(input$reverseFilterString)
  updateCheckboxGroupInput(session, "reverseFastq", choices = getFastqFiles(input$reverseFilterString))
})

observeEvent(input$reverseSelectAllButton, {
 updateCheckboxGroupInput(session, "reverseFastq", selected = getFastqFiles(input$reverseFilterString))
})

observeEvent(input$reverseCleanAllButton, {
 updateCheckboxGroupInput(session, "reverseFastq", choices = getFastqFiles(input$reverseFilterString))
})
