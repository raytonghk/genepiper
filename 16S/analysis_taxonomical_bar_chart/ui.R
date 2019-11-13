library(shiny)
library(shinyjs)
library(bsplus)
source("../style.R")
source("../ui_additional.R")
source("../ui_panel_load_data.R")
source("../ui_panel_filter.R")
source("../ui_panel_parameter.R")
source("../ui_panel_graphic.R")

X_AXIS_TEXT_SIZE <- 10
X_AXIS_TEXT_ANGLE <- 45
Y_AXIS_TEXT_SIZE <- 10
Y_AXIS_TEXT_ANGLE <- 0
LEGEND_TEXT_SIZE <- 10

shinyUI(
  fluidPage(
    useShinyjs(),
    theme = "../style.css",
    tags$script(type = "text/javascript", src = "../www/busy.js"),
    tags$script(type = "text/javascript", src = "../www/controls.js"),
    tags$div(class = "busy", p("Busy..."), img(src = "../www/hour-glass.gif")),
    tags$script(HTML("window.onload = function() { parent.postMessage('analysisTaxonomicalBarChartLoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h4("Taxonomical Bar Chart"),
        tags$div(
          class = "desc",
          p("This module provides options to plot a stacked bar-chart showing the composition of microbial communities at a specified taxonomic rank.This is commonly done at a higher taxonomic rank to have an overview of the microbiome in the cohort."),
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
                uiAbundanceType(),
                uiPrevalenceFilter(),
                uiDisplayFilter(),
                uiPlotButton()
              ),
              
              tags$div(
                class = "graphic-panel",
                h4("Graphic Parameters:"),
                tabsetPanel(
                  uiTabPanelGroup(),
                  uiTabPanelTitle(),
                  uiTabPanelXYAxis(X_AXIS_TEXT_SIZE, X_AXIS_TEXT_ANGLE, Y_AXIS_TEXT_SIZE, Y_AXIS_TEXT_ANGLE),
                  uiTabPanelLegend(LEGEND_TEXT_SIZE)
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.modifiedPhyloseqReady == true",
              tags$hr(),
              plotOutput("barPlot", height = "800px"),
              actionButton("downloadDialogButton", "Download")
            )
          )
        )
      )
    )
  )
)














































