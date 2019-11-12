uiPanelFilter<- function() {
  wellPanel(
    class = "left-column filter-panel",
    h4("Filter"),
    h5("Sample:"),
    textInput("filterSampleText", "Command Line", placeholder = "e.g. Blood == \'A\'") %>%
      shinyInput_label_embed(
        shiny_iconlink() %>%
          bs_embed_popover(title = "Filter Expression",
                           placement = "left",
                           html = "true",
                           content = "Expression should use symbol of column name and return a vector of logical.")
      ),
    h5("- OR -"),
    selectInput("filterSampleColumn", "Filter By Column", NULL),
    conditionalPanel(
      condition = "output.filterColumnType == \'numeric\'",
      selectInput("filterSampleOperator", NULL, choices = c("==", ">", ">=", "<", "<=")),
      numericInput("filterSampleNumeric", NULL, 0)
    ),
    conditionalPanel(
      condition = "output.filterColumnType == \'character\'",
      checkboxGroupInput("filterSampleCheckbox", "Please Select", NULL)
    ),
    actionButton("filterButton", "Filter"),
    errorOutput("filterSampleMessage"),
    verbatimTextOutput("filteredPhyloseqDetails")
  )
}

uiPanelFilterModule <- function(classId) {
  wellPanel(
    class = classId,
    h4("Filter"),
    tags$div(
      id = "filterPanelFooter",
      tags$div(
        class = "column-left",
        style = "width: 45%",
        actionButton("addFilter", "+")
      ),
      tags$div(
        class = "column-right",
        actionButton("filterModuleButton", "Filter")
      )
    ),
    verbatimTextOutput("moduleFilteredPhyloseqDetails")
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
}