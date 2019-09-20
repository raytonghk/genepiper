library(shiny)
library(shinyjs)
library(bsplus)
source("../style.R")
source("../ui_additional.R")
source("../ui_panel_load_data.R")
source("../ui_panel_filter.R")
source("../ui_panel_parameter.R")
source("../ui_panel_graphic.R")

LINE_WIDTH <- 1
X_AXIS_TEXT_SIZE <- 10
X_AXIS_TEXT_ANGLE <- 0
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
    tags$script(HTML("window.onload = function() { parent.postMessage('analysisDiversityIndexCurveLoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h4("Diversity index curve"),
        tags$div(
          class = "desc",
          p("A diversity index is a quantitative measure that reflects how many different types (such as species) there are in a dataset (a community), and simultaneously takes into account how evenly the basic entities (such as individuals) are distributed among those types.
            When diversity indices are used in ecology, the types of interest are usually species, but they can also be other categories, such as genera, families, functional types or haplotypes. The entities of interest are usually individual plants or animals, and the measure of abundance can be, for example, number of individuals, biomass or coverage. In demography, the entities of interest can be people, and the types of interest various demographic groups. In information science, the entities can be characters and the types the different letters of the alphabet. The most commonly used diversity indices are simple transformations of the effective number of types (also known as 'true diversity'), but each diversity index can also be interpreted in its own right as a measure corresponding to some real phenomenon (but a different one for each diversity index).
            Many indices only account for categorical diversity between subjects or entities. Such indices however do not account for the total variation (diversity) that can be held between subjects or entities which occurs only when both categorical and qualitative diversity are calculated."),
          a(target = "_blank", href = "https://en.wikipedia.org/wiki/Diversity_index", "From Wikipedia")
          ),
        tags$div(
          class = "ref",
          h5("Heck, K.L., van Belle, G. & Simberloff, D. (1975). Explicit calculation of the rarefaction diversity measurement and the determination of sufficient sample size. Ecology 56, 1459--1461."),
          h5("Hurlbert, S.H. (1971). The nonconcept of species diversity: a critique and alternative parameters. Ecology 52, 577--586.")
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
                uiAlphaIndex(),
                uiPlotButton()
              ),
              
              tags$div(
                class = "graphic-panel",
                h4("Graphic Parameters:"),
                tabsetPanel(
                  uiTabPanelLine(LINE_WIDTH),
                  uiTabPanelTitle(),
                  uiTabPanelXYAxis(X_AXIS_TEXT_SIZE, X_AXIS_TEXT_ANGLE, Y_AXIS_TEXT_SIZE, Y_AXIS_TEXT_ANGLE),
                  uiTabPanelLegend(LEGEND_TEXT_SIZE)
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.plotTableReady == true",
              tags$hr(),
              plotOutput("indexPlot", height = "800px"),
              actionButton("downloadDialogButton", "Download")
            )
          )
        )
      )
    )
  )
)




































