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
        h4("Taxonomical Venn Diagram")
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



























