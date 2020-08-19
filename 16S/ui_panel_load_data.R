uiPanelLoadData <- function() {
  wellPanel(
    class = "left-column",
    tags$div(
      tags$div(class = "pull-right",
               shiny_iconlink() %>%
                 bs_embed_popover(title = "Load Data",
                                  placement = "left",
                                  html = "true",
                                  content = "Retrieve stored data in the project by data label.")),
      h4("Load Data")
    ),
    selectInput("project", "Project", NULL),
    selectInput("dataLabel", "Data Label", NULL),
    conditionalPanel(
      condition = "input.dataLabel",
      actionButton("loadDataButton", "Load Data")
    ),
    verbatimTextOutput("dataDetails")
  )
}










