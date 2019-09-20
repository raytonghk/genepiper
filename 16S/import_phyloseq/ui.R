library(shiny)
library(shinyjs)
library(bsplus)
source("../style.R")
source("../ui_additional.R")

shinyUI(
  fluidPage(
    useShinyjs(),
    theme = "../style.css",
    tags$script(type = "text/javascript", src = "../www/busy.js"),
    tags$script(type = "text/javascript", src = "../www/controls.js"),
    tags$div(class = "busy", p("Busy..."), img(src = "../www/hour-glass.gif")),
    tags$script(HTML("window.onload = function() { parent.postMessage('importPhyloseqLoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h5("The imported phyloseq object should be in RDS file type and should contains OTU table, Taxonomy table and Sample Data. Tree is optional but highly recommend including ML tree which use for 'Taxonomical tree analysis' and calculating 'Unifrac' distance.")
      ),
      
      column(
        width = 4,
        wellPanel(
          class = "left-column",
          tags$div(
            tags$div(class = "pull-right",
                     shiny_iconlink() %>%
                       bs_embed_popover(title = "Phyloseq RDS file",
                                        placement = "left",
                                        html = "true",
                                        content = tags$div(tags$p("RDS file contains phyloseq-class object which must contain OTU table, Taxonomy table and Sample data."),
                                                           tags$p("Please noted that the OTU names and sample names may be changed if not a syntactically valid names.")))),
            h4("RDS File:")
          ),
          fileInput("filepath", "File Path"),
          errorOutput("filepathMessage")
        )
      ),
      
      column(
        width = 4,
        wellPanel(
          class = "middle-column",
          h4("Phyloseq:"),
          errorOutput("phyloseqMessage"),
          verbatimTextOutput("phyloseqDetails")
        )
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
          selectInput("project", NULL, NULL),
          actionButton("newProjectButton", "New Project")
        ),
        
        wellPanel(
          class = "right-column",
          h4("Save:"),
          textInput("dataLabel", "Data Label") %>%
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