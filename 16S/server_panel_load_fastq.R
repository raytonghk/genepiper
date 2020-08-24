### Extdata
EXTDATA_PATH <- "/srv/shiny-server/extdata/"

getFastqFiles <- function(filterString) {
  tryCatch({
    list.files(EXTDATA_PATH, filterString, recursive = TRUE, full.names = TRUE)
  }, error = function(e){})
}

### Load Forward Fastq Files
observe({
  req(input$forwardFilterString)
  files <- getFastqFiles(input$forwardFilterString)
  updateCheckboxGroupInput(session, "forwardFastq", choiceValues = files, choiceNames = basename(files))
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
  files <- getFastqFiles(input$reverseFilterString)
  updateCheckboxGroupInput(session, "reverseFastq", choiceValues = files, choiceNames = basename(files))
})

observeEvent(input$reverseSelectAllButton, {
 updateCheckboxGroupInput(session, "reverseFastq", selected = getFastqFiles(input$reverseFilterString))
})

observeEvent(input$reverseCleanAllButton, {
 updateCheckboxGroupInput(session, "reverseFastq", choices = getFastqFiles(input$reverseFilterString))
})
