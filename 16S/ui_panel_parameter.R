uiAbundanceType <- function(choices = c("Raw Count", "Rarefied Count", "Relative Abundance"), width = "90%", id = NULL) {
  selectInput(paste0("abundanceType", id), "Abundance Type", choices,  width = width)
}

uiAnalysisButton <- function() {
  tags$div(
    errorOutput("analysisMessage"),
    actionButton("analysisButton", "Analysis")
  )
}

uiAlphaIndex <- function(alphaChoices = c("Richness", "Shannon Index", "Inverse Simpson", "Coverage"),
                         alphaWidth = "90%", rareValue = 1, rareWidth = "90%") {
  tags$div(
    selectInput("alphaIndex", "Diversity Index To Plot", alphaChoices, width = alphaWidth),
    conditionalPanel(
      condition = "input.alphaIndex == \'Coverage\'",
      numericInput("countAsRare", "Count For Defining Rare Species", rareValue, width = rareWidth)
    )
  )
}

uiDistanceMethod <- function(width = "90%") {
  selectInput("distanceMethod", "Distance Method", NULL, width = width) %>%
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
    )
}

uiGroupColumnWithLabel <- function(label = "Group Column", width = "90%", id = NULL) {
  tags$div(
    selectInput(paste0("groupColumn", id), label, NULL, width = width),
    checkboxGroupInput(paste0("groupLabels", id), "Select label", NULL),
    errorOutput(paste0("groupLabelMessage", id))
  )
}

uiPlotButton <- function() {
  tags$div(
    errorOutput("plotMessage"),
    actionButton("plotButton", "Plot")
  )
}

uiPrepareDataButton <- function() {
  tags$div(
    errorOutput("prepareDataMessage"),
    actionButton("prepareDataButton", "Prepare Data")
  )
}

uiSignif <- function(id = NULL, value = 0.05, min = 0, max = 1, width = "90%") {
  numericInput(paste0("signif", id), "Significant Level", value, min, max, width = width)
}

uiTabPanelTableXWithRankFilter <- function() {
  tabPanel(
    title = "Table X",
    selectInput("rankFilter", "Filter Rank", NULL, width = "90%"),
    checkboxGroupInput("tableXLabel", "Select Labels:", NULL),
    tags$div(
      tags$div(
        class = "column-left",
        actionButton("tableXSelectAllButton", "Select All")
      ),
      tags$div(
        class = "column-right",
        actionButton("tableXClearAllButton", "Clear All")
      )
    )
  )
}

uiTabPanelTableY <- function() {
  tabPanel(
    title = "Table Y",
    checkboxGroupInput("tableYLabel", "Select Labels:", NULL),
    tags$div(
      tags$div(
        class = "column-left",
        actionButton("tableYSelectAllButton", "Select All")
      ),
      tags$div(
        class = "column-right",
        actionButton("tableYClearAllButton", "Clear All")
      )
    )
  )
}

uiTaxRank <- function(choices = NULL, width = "90%", id = NULL) {
  selectInput(paste0("taxRank", id), "Taxonomic Rank For Agglomeration", choices, width = width)
}

uiDisplayFilter <- function(id = NULL, choices = list("Top Abundance" = "top", "All" = "all"), displayNumber = 10) {
  tags$div(
    tags$div(
      class = "column-left",
      style = "width: auto; margin-right: 2%",
      h5("Display:", style = "font-weight: bold;")
    ),
    tags$div(
      class = "column-right",
      style = "width: 30%",
      selectInput(paste0("displayFilter", id), NULL, choices)
    ),
    tags$div(
      class = "column-right",
      style = "width: 30%",
      conditionalPanel(
        condition = paste0("input.displayFilter", id, " != 'all'"),
        numericInput(paste0("displayNumber", id), NULL, displayNumber)
      )
    )
  )
}







