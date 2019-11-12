library(shiny)
library(shinyjs)
library(bsplus)
source("../style.R")
source("../ui_additional.R")
source("../ui_panel_load_data.R")
source("../ui_panel_filter.R")

shinyUI(
  fluidPage(
    useShinyjs(),
    theme = "../style.css",
    tags$script(type = "text/javascript", src = "../www/busy.js"),
    tags$script(type = "text/javascript", src = "../www/controls.js"),
    tags$div(class = "busy", p("Busy..."), img(src = "../www/hour-glass.gif")),
    tags$script(HTML("window.onload = function() { parent.postMessage('importFilterPhyloseqLoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h5("Filter phyloseq is a helper function to filter samples or taxa of the stored phyloseq. After filtering, data will be saved as a new data in the Genepiper.")
      ),
      
      column(
        width = 4,
        uiPanelLoadData()
      ),
      
      column(
        width = 4,
        uiPanelFilterModule("middle-column")
        # wellPanel(
        #   class = "middle-column",
        #   style = "overflow-y: scroll; max-height: 1000px",
        #   h4("Filter"),
        #   h5("Sample:"),
        #   textInput("filterSampleText", "Command Line", placeholder = "e.g. Blood == \'A\'") %>%
        #     shinyInput_label_embed(
        #       shiny_iconlink() %>%
        #         bs_embed_popover(title = "Filter Expression",
        #                          placement = "left",
        #                          html = "true",
        #                          content = "Expression should use symbol of column name and return a vector of logical.")
        #     ),
        #   h5("- OR -"),
        #   selectInput("filterSampleColumn", "Filter By Column", NULL),
        #   conditionalPanel(
        #     condition = "output.filterColumnType == \'numeric\'",
        #     selectInput("filterSampleOperator", NULL, choices = c("==", ">", ">=", "<", "<=")),
        #     numericInput("filterSampleNumeric", NULL, 0)
        #   ),
        #   conditionalPanel(
        #     condition = "output.filterColumnType == \'character\'",
        #     checkboxGroupInput("filterSampleCheckbox", "Please Select", NULL)
        #   ),
        #   actionButton("filterButton", "Filter"),
        #   errorOutput("filterSampleMessage"),
        #   verbatimTextOutput("filteredPhyloseqDetails")
        # )
      ),
      
      column(
        width = 4,
        wellPanel(
          class = "right-column",
          tags$div(
            tags$div(class = "pull-right",
                     shiny_iconlink() %>%
                       bs_embed_popover(title = "Project",
                                        placement = "left",
                                        html = "true",
                                        content = tags$div(tags$p("Project is to group related data for storage."),
                                                           tags$p("New Project button can help to create a new project title.")))),
            h4("Project:")
          ),
          selectInput("projectToSave", NULL, NULL),
          actionButton("newProjectButton", "New Project")
        ),
        
        wellPanel(
          class = "right-column",
          h4("Save:"),
          textInput("dataLabelToSave", "Data Label") %>%
            shinyInput_label_embed(
              shiny_iconlink() %>%
                bs_embed_popover(title = "Data Label",
                                 placement = "left",
                                 html = "true",
                                 content = "Data label is to name the imported data. The data will be recalled by the data label within Genepiper")
            ),
          errorOutput("saveMessage"),
          actionButton("saveButton", "Save")
        ),
        
        wellPanel(
          class = "right-column",
          h4("Download Phyloseq RDS:"),
          downloadButton("downloadPhyloseqButton")
        )
      )
    )
  )
)
