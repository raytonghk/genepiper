library(shiny)
library(shinyjs)
library(bsplus)
source("../style.R")
source("../ui_additional.R")
source("../ui_panel_load_data.R")
source("../ui_panel_filter.R")
source("../ui_panel_parameter.R")
source("../ui_panel_graphic.R")

LABEL_SIZE <- 1

shinyUI(
  fluidPage(
    useShinyjs(),
    theme = "../style.css",
    tags$script(type = "text/javascript", src = "../www/busy.js"),
    tags$script(type = "text/javascript", src = "../www/controls.js"),
    tags$div(class = "busy", p("Busy..."), img(src = "../www/hour-glass.gif")),
    tags$script(HTML("window.onload = function() { parent.postMessage('analysisTaxonomicalVennDiagramLoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h4("Taxonomical Venn Diagram"),
        tags$div(
          class = "desc",
          p("This module provides options to plot a venn diagram comparing the membership of microbial communities at a user-specified taxonomic rank for up to 5 samples or groups. Venn diagram is commonly used to viusalize the comparison of lists of objects.It is most often displayed as two or more circles in which each circle corresponds to a data set, and the overlap between the circles corresponds to the overlap between the data sets. User may then quickly observe the common and unique set of taxa amongst the samples/ groups of samples they are analyzing."),
          p("GenePiper utilises the VennDiagram package (Chen & Boutros, 2011) to generate the plot. User may specify the taxonomic rank of taxa for the plot. From the Group column, user may select either 'Sample' or a varaible from the sample data table to group the samples for comparison."),
          a(target = "_blank", href="https://github.com/raytonghk/genepiper/wiki/13.-Taxonomical-Venn-Diagram", "See our tutorial about this module.")          
          ),
        tags$div(
          class = "ref",
	h5("Chen H, Boutros PC (2011) VennDiagram: a package for the generation of highly-customizable Venn and Euler diagrams in R. BMC Bioinformatics 12, 35, doi:10.1186/1471-2105-12-35."),
          h5("Ruskey F (2001) A Survey of Venn Diagrams. The Electronic Jornal of Combinatorics, DS#5, https://www.combinatorics.org/files/Surveys/ds5/ds5v2-2001/VennEJC.html"),
          h5("McMurdie PJ, Holmes S (2013) phyloseq: An R Package for Reproducible Interactive Analysis and Graphics of Microbiome Census Data. PLoS ONE 8(4): e61217. https://doi.org/10.1371/journal.pone.0061217"),
          h5("Sudarikov K, Tyakht A, Alexeev D (2017) Methods for the metagenomic data visualization and analysis. Curr Issues Mol Biol. 24:37-58.")
        )
      ),
      
      column(
        width = 4,
        uiPanelLoadData(),
        
        conditionalPanel(
          condition = "output.dataLoaded",
          uiPanelFilter()
        )
      ),
      
      column(
        width = 8,
        conditionalPanel(
          condition = "output.filteredDataLoaded",
          wellPanel(
            class = "middle-column",
            tags$div(
              tags$div(
                class = "parameter-panel",
                h4("Parameters:"),
                uiTaxRank(),
                selectInput("groupColumnVenn", "Group Column", NULL, width = "90%"),
                checkboxGroupInput("groupLabel", "Please select:", NULL),
                errorOutput("groupLabelMessage"),
                uiPlotButton()
              ),
              
              tags$div(
                class = "graphic-panel",
                tabsetPanel(
                  uiTabPanelTitle(),
                  uiTabPanelLabel(LABEL_SIZE)
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.plotListReady == true",
              tags$hr(),
              plotOutput("vennPlot", height = "800px"),
              actionButton("downloadDialogButton", "Download")
            )
          )
        )
      )
    )
  )
)



