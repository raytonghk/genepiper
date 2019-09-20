uiTabPanelDistanceMatrix <- function() {
  tabPanel(
    title = "Distance Matrix",
    DT::dataTableOutput("distanceMatrix"),
    downloadButton("downloadDistanceMatrix", "Download Distance Matrix")
  )
}

uiTabPanelEigenValues <- function() {
  tabPanel(
    title = "Eigen Values",
    DT::dataTableOutput("eigenValuesTable")
  )
}

uiTabPanelPermanova <- function() {
  tabPanel(
    title = "Permanova",
    selectInput("permanovaGroupColumn", "Group Column", NULL, width = "45%"),
    selectInput("permanovaDistanceMethod", "Distance Method", NULL, width = "45%") %>%
      shinyInput_label_embed(
        shiny_iconlink() %>%
          bs_embed_popover(title = "Distance Method",
                           placement = "left",
                           html = "true",
                           content = tags$div(
                             tags$div(
                               tags$a(target = "_blank", href = "https://www.rdocumentation.org/packages/phyloseq/versions/1.16.2/topics/UniFrac", "phyloseq::UniFrac")
                             ),
                             tags$a(target = "_blank", href = "https://www.rdocumentation.org/packages/vegan/versions/2.4-2/topics/vegdist", "vegan::vegdist")
                           ))
      ),
    verbatimTextOutput("permanovaOutput"),
    tags$br(),
    errorOutput("permanovaMessage")
  )
}

uiTabPanelPlot2d <- function() {
  tabPanel(
    title = "2D Plot",
    plotOutput("plot2d", height = "800px"),
    actionButton("downloadDialogButton", "Download")
  )
}

uiTabPanelPlot3d <- function() {
  tabPanel(
    title = "3D Plot",
    plotly::plotlyOutput("plot3d", height = "800px")
  )
}

uiTabPanelSampleTable <- function() {
  tabPanel(
    title = "Sample",
    DT::dataTableOutput("sampleTable"),
    downloadButton("downloadSampleTableButton")
  )
}

uiTabPanelTaxaTable <- function(title = "Taxa") {
  tabPanel(
    title = title,
    DT::dataTableOutput("taxaTable"),
    downloadButton("downloadTaxaTableButton")
  )
}





