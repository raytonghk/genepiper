library(shiny)
library(shinyjs)
library(bsplus)
source("../style.R")
source("../ui_additional.R")
source("../ui_panel_load_data.R")
source("../ui_panel_filter.R")
source("../ui_panel_parameter.R")
source("../ui_panel_graphic.R")

LEGEND_TEXT_SIZE <- 10

shinyUI(
  fluidPage(
    useShinyjs(),
    theme = "../style.css",
    tags$script(type = "text/javascript", src = "../www/busy.js"),
    tags$script(type = "text/javascript", src = "../www/controls.js"),
    tags$div(class = "busy", p("Busy..."), img(src = "../www/hour-glass.gif")),
    tags$script(HTML("window.onload = function() { parent.postMessage('analysisTaxonomicalTreeLoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h4("Taxonomical Tree"),
         class = "desc",
          p("This module provides options to display phylogenetic trees at a user-specified taxonomic rank with annotation about the samples. Branches of the phylogenetic tree will be collapsed according to the taxonomic rank selected. Prevalence and abundance filters are provided ad hoc. User may select a variable from the sample data table to customise the colour and shape of the annotation symbols. There is also an option to display the abundance information in terms of the annotation symbol size."),
          p("GenePiper utilises the plot_tree function from the phyloseq package (McMurdie & Holmes, 2013) to generate the plot. The phylogenetic tree uploaded to the data will be displayed. GenePiper provides a helper function to build Maximum Likelihood phylogenetic tree from a multiple seqeunce aligment file."), a(target = "_blank", href="https://joey711.github.io/phyloseq/plot_tree-examples.html", "Check out the phyloseq tutorial; ")
          a(target = "_blank", href="https://github.com/raytonghk/genepiper/wiki/12.-Taxonomical-Tree", "See also our tutorial about this module.")          
          ),

        tags$div(
          class = "ref",
          h5("McMurdie PJ, Holmes S (2013) phyloseq: An R Package for Reproducible Interactive Analysis and Graphics of Microbiome Census Data. PLoS ONE 8(4): e61217. https://doi.org/10.1371/journal.pone.0061217")
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
                uiPrevalenceFilter(),
                uiDisplayFilter(),
                checkboxInput("displayTipLabel", "Display Tip Labels?", FALSE),
                checkboxInput("displayNodeLabel", "Display Node Labels?", FALSE),
                selectInput("columnForColor", "Column For Color", NULL, width = "90%"),
                selectInput("columnForShape", "Column For Shape", NULL, width = "90%"),
                selectInput("columnForSize", "Column For Size", NULL, width = "90%"),
                numericInput("plotHeight", "Plot Height", 25, 0, width = "90%"),
                conditionalPanel(
                  condition = "input.columnForSize == \'Abundance\'",
                  selectInput("abundanceType", "Display Abundance As", c("Raw Count", "Rarefied Count", "Relative Abundance"), width = "90%")
                ),
                uiPlotButton()
              ),

              tags$div(
                class = "graphic-panel",
                tabsetPanel(
                  id = "graphicTabset",
                  uiTabPanelTitle(),
                  uiTabPanelLegend(LEGEND_TEXT_SIZE)
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.treeReady == true",
              tags$hr(),
              plotOutput("treePlot", height = "auto"),
              actionButton("downloadDialogButton", "Download")
            )
          )
        )
      )
    )
  )
)
