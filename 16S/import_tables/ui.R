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
        h5("Imported tables will be used to construct a phyloseq-class object and stored in the GenePiper environment. This file could be exported in the Download Phyloseq RDS panel."),
        h5(tags$span(class = "bold", "All tables"), "should be in either tab- or comma-delimited text format. See SampleData for examples of these table format"),
        h5(tags$span(class = "bold", "OTU table"), "must be provided. Abundance matrix of taxa (rows) by samples (columns).The first column should contain the OTU/ASV/species/gene names (characters);  first row should contain sample names (characters). Absolute number of the sequencing reads in the matrix without 'NA' value."),
        h5(tags$span(class = "bold", "Taxonomy table"), "must be provided and should contain all character columns. The first column should match that of the OTU table."),
        h5(tags$span(class = "bold", "Sample data"), "must be provided and should contain at least two columns. The first column should be the sample names."),
        h5(tags$span(class = "bold", "Phylogenetic tree"), "should be either in Newick or Nexus format; is optional but highly recommended. It will be used for calculation of the 'Unifrac' distance (Lozupone & Knight 2005) and taxonomical tree analysis. A helper function is available in the main page to build a Maximum Likelihood tree from representative sequences provided in fasta format.")
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
                                        <LI>no row names or row numbers
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
                                        <LI>all columns are characters
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
                                        content = tags$div(tags$p("Phylogenetic Tree will be used for Unifrac distance calculation and taxonomical tree analysis."),
                                                           tags$p("A helper function is available in the main page to build the ML tree from representative sequences.")))),
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
                                        content = tags$div(tags$p("Group related dataset into project."),
                                                           tags$p("Press New Project button to create and name a new project.")))),
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
                                 content = "Data label provides name to the imported data. You may recall the dataset by this data label within GenePiper")
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
