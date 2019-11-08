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
          p("Rarefaction is a resampling framework that selects, at random, 1, 2, ..., n units (generally without replacement) until all units in the sample have been accumulated. For four decades (Heck et al. 1983), biologists (and others) have used rarefaction to equalize the information content of individual-based abundance samples. Since the effects of differences in sample size on diversity statistics for two or more samples can usually be substantially reduced by comparing at the same level of species accumulation.

In this module, user may create rarefaction curves based on one of the following meaures: Richness, which refers to the number of ASVs/OTUs detected in the sample; the alpha-diversity measures of the Shannon Index and the Inverse Simpson index and the Good's coverage estimation.  
Rarefaction curves are often plotted at the ASVs/OTUs level to assess the sampling effort as the species richness tends to reach a clear asymptote at sufficient sampling depth. Users may as well select to plot these curves at any taxonomic rank if the interest is to compare the phylotypes. 
"),

          a(target = "_blank", href = "http://viceroy.eeb.uconn.edu/estimates/EstimateSPages/EstSUsersGuide/EstimateSUsersGuide.htm#Introduction", "Adopted from EstimateS by Robert K. Colwell")

          ),

        tags$div(

          class = "ref",
h5("Colwell, R. K. (2013). EstimateS: Statistical estimation of species richness and shared species from samples. Version 9. User's Guide and application published at: http://purl.oclc.org/estimates."),
h5("Chao A, Gotelli NJ, Hsieh TC, Sander EL, Ma KH, Colwell RK, Ellison AM. (2014) Rarefaction and extrapolation with Hill numbers: a framework for sampling and estimation in species diversity studies. Ecological monographs. 84(1):45-67."),
h5("Chao A, Jost L. (2012) Coverage‐based rarefaction and extrapolation: standardizing samples by completeness rather than size. Ecology. 93(12):2533-47."),

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
