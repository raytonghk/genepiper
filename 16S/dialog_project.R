newProjectDialog <- function() {
  modalDialog(
    h4("New Project Name:"),
    textInput("newProjectName", NULL),
    errorOutput("newProjectMessage"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("newProjectSaveButton", "Save")
    ),
    easyClose = FALSE
  )
}

observeEvent(
  input$newProjectSaveButton,
  {
    req(input$newProjectName)
    vals$newProjectMessage <- NULL
    path <- getProjectPath(input$newProjectName)
    if(dir.exists(path)) {
      vals$newProjectMessage <- "Project exists!"
    } else {
      system(paste0("mkdir ", path, " -m 777"))
      updateSelectInput(session, "project", choices = getProjects(), selected = input$newProjectName)
      removeModal()
    }
  }
)

output$newProjectMessage <- renderText(HTML(vals$newProjectMessage))