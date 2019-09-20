output$dataLoaded <- reactive(isTruthy(vals$phyloseq))
outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)

output$filteredDataLoaded <- reactive(isTruthy(vals$filteredPhyloseq))
outputOptions(output, "filteredDataLoaded", suspendWhenHidden = FALSE)

### Project
PROJECT_ROOT_PATH <- "/srv/shiny-server/data/"

projectNames <- function() {
  unique(c("Default", unlist(getProjects())))
}

getProjects <- function() {
  list.dirs(getProjectPath(), FALSE)
}

getProjectPath <- function(project = NULL) {
  paste0(PROJECT_ROOT_PATH, project)
}

updateSelectInput(session, "project", choices = projectNames())

### Data Label
observe({
  req(input$project)
  updateSelectInput(session, "dataLabel", choices = dataNames(input$project))
})

dataNames <- function(project) {
  list.files(getProjectPath(project), "phyloseq.rds") %>%
    gsub("_phyloseq.rds", "", .)
}

### Load Data Button
observeEvent(input$loadDataButton,
             {
               req(input$project, input$dataLabel)
               phyloseq <- getData(input$project, input$dataLabel)
               vals$project <- input$project
               vals$dataLabel <- input$dataLabel
               vals$phyloseq <- phyloseq
               vals$filteredPhyloseq <- phyloseq
             }
)

getData <- function(project, dataLabel) {
  readRDS(paste0(getProjectPath(project), "/", dataLabel, "_phyloseq.rds"))
}

### Data Details 
output$dataDetails <- renderPrint({
  req(vals$phyloseq)
  showDataDetails(vals$project, vals$dataLabel, vals$phyloseq)
})

showDataDetails <- function(project, data, phyloseq) {
  cat("Data from:", paste0(project, "/", data), "\n\n")
  cat(phyloseq::show(phyloseq), "\n\n")
}




