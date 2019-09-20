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
    tags$script(HTML("window.onload = function() { parent.postMessage('importTablesLoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h5("Imported tables was used to construct the phyloseq object and store in the genepiper."),
        h5(tags$span(class = "bold", "All tables"), "must not contain row names and row numbers."),
        h5(tags$span(class = "bold", "OTU table"), "must be provided and should contains only one character column and without 'NA' value. The first column should be the OTU name column."),
        h5(tags$span(class = "bold", "Taxonomy table"), "must be provided and should contains all character columns. The first column should be OTU name and each taxonomical rank in individual column."),
        h5(tags$span(class = "bold", "Sample data"), "must be provided and should contains at least two columns. The first column should be the sample name."),
        h5(tags$span(class = "bold", "Phylogenetic tree"), "is optional but highly recommended. It was used for calculating 'Unifrac' distance and taxonomical tree analysis. A helper function was provided to build the ML tree by OTU representative sequences.")
      ),
      
      column(
        width = 4,
        wellPanel(
          class = "left-column",
          tags$div(
            tags$div(class = "pull-right",
                     shiny_iconlink() %>%
                       bs_embed_popover(title = "OTU Table Format",
                                        placement = "left",
                                        html = "true",
                                        content = "<UL>
                                        <LI>no row names and row numbers
                                        <LI>the first column contains OTU names
                                        <LI>no 'NA' value
                                        </UL>")),
            h4("OTU Table File:")
          ),
          selectInput("otuFileType", "File Type", list("Comma-separated file" = "csv",
                                                       "Tab-separated file" = "tsv")),
          fileInput("otuFilePath", "File Path"),
          errorOutput("otuFilePathMessage"),
          verbatimTextOutput("otuDetails")
        ),
        
        wellPanel(
          class = "left-column",
          tags$div(
            tags$div(class = "pull-right",
                     shiny_iconlink() %>%
                       bs_embed_popover(title = "Taxonomy Table Format",
                                        placement = "left",
                                        html = "true",
                                        content = "<UL>
                                        <LI>no row names and row numbers
                                        <LI>the first column contains OTU names
                                        <LI>all columns are character>
                                        <LI>each taxonomical rank in individual column
                                        </UL>")),
            h4("Taxonomy Table File:")
          ),
          selectInput("taxFileType", "File Type", list("Comma-separated file" = "csv",
                                                       "Tab-separated file" = "tsv")),
          fileInput("taxFilePath", "File Path"),
          errorOutput("taxFilePathMessage"),
          verbatimTextOutput("taxDetails")
        ),
        
        wellPanel(
          class = "left-column",
          tags$div(
            tags$div(class = "pull-right",
                     shiny_iconlink() %>%
                       bs_embed_popover(title = "Sample Data Format",
                                        placement = "left",
                                        html = "true",
                                        content = "<UL>
                                        <LI>no row names and row numbers<br>
                                        <LI>the first column contains sample names<br>
                                        <LI>at least two columns
                                        </UL>")),
            h4("Sample Data File:")
          ),
          selectInput("samFileType", "File Type", list("Comma-separated file" = "csv",
                                                       "Tab-separated file" = "tsv")),
          fileInput("samFilePath", "File Path"),
          errorOutput("samFilePathMessage"),
          verbatimTextOutput("samDetails")
        ),
        
        wellPanel(
          class = "left-column",
          tags$div(
            tags$div(class = "pull-right",
                     shiny_iconlink() %>%
                       bs_embed_popover(title = "Phylogenetic Tree",
                                        placement = "left",
                                        html = "true",
                                        content = tags$div(tags$p("Phylogenetic Tree was used for 'Unifrac' distance calculation and taxonomical tree analysis."),
                                                           tags$p("A helper function was provided to build the ML tree by OTU representative sequences.")))),
            h4("Phylogenetic Tree File (Optional):")
          ),
          fileInput("treeFilePath", "File Path (Newick or Nexus format)"),
          errorOutput("treeFilePathMessage"),
          verbatimTextOutput("treeDetails")
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







































