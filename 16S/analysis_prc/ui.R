library(shiny)
library(shinyjs)
library(bsplus)
source("../style.R")
source("../ui_additional.R")
source("../ui_panel_load_data.R")
source("../ui_panel_filter.R")
source("../ui_panel_parameter.R")
source("../ui_panel_graphic.R")
source("../ui_panel_result.R")

X_AXIS_TEXT_SIZE <- 10
X_AXIS_TEXT_ANGLE <- 0
Y_AXIS_TEXT_SIZE <- 10
Y_AXIS_TEXT_ANGLE <- 0
LEGEND_TEXT_SIZE <- 10
LINE_SIZE <- 1

shinyUI(
  fluidPage(
    useShinyjs(),
    theme = "../style.css",
    tags$script(type = "text/javascript", src = "../www/busy.js"),
    tags$script(type = "text/javascript", src = "../www/controls.js"),
    tags$div(class = "busy", p("Busy..."), img(src = "../www/hour-glass.gif")),
    tags$script(HTML("window.onload = function() { parent.postMessage('analysisPRCLoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h4("Principal Response Curve"),
        tags$div(
          class = "desc",
          p("In multivariate statistics, principal response curves (PRC) are used for analysis of treatment effects in experiments with a repeated measures design."),
          p("First developed as a special form of redundancy analysis, PRC allow temporal trends in control treatments to be corrected for, which allows the user to estimate the effects of the treatment levels without them being hidden by the overall changes in the system. An additional advantage of the method in comparison to other multivariate methods is that it gives a quantification of the treatment response of individual species that are present in the different groups."),
          a(target = "_blank", href = "https://en.wikipedia.org/wiki/Principal_response_curve", "From Wikipedia")
        ),
        
        tags$div(
          class = "ref",
          h5("van den Brink, P.J. & ter Braak, C.J.F. (1999). Principal response curves: Analysis of time-dependent multivariate responses of biological community to stress. Environmental Toxicology and Chemistry, 18, 138–148.")
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
                checkboxInput("logMatrix", "Log Matrix Values?", TRUE),
                selectInput("treatmentColumn", "Treatment Column", NULL, width = "90%"),
                selectInput("timeColumn", "Time Column", NULL, width = "90%"),
                errorOutput("prcMessage"),
                actionButton("analysisButton", "Analysis")
              ),
              
              tags$div(
                class = "graphic-panel",
                conditionalPanel(
                  condition = "input.prcTabset == 'Plot'",
                  tabsetPanel(
                    id = "graphicTabset",
                    uiTabPanelTitle(),
                    uiTabPanelXYAxis(X_AXIS_TEXT_SIZE, X_AXIS_TEXT_ANGLE, Y_AXIS_TEXT_SIZE, Y_AXIS_TEXT_ANGLE),
                    uiTabPanelLegend(LEGEND_TEXT_SIZE),
                    uiTabPanelLine(LINE_SIZE)
                  )
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.prcReady",
              tags$hr(),
              tabsetPanel(
                id = "prcTabset",
                tabPanel(
                  title = "Output",
                  verbatimTextOutput("prcOutput"),
                  downloadButton("downloadPrcButton")
                ),
                
                tabPanel(
                  title = "Coefficient",
                  DT::dataTableOutput("coeffTable")
                ),
                
                tabPanel(
                  title = "Species Weight",
                  DT::dataTableOutput("specWeight")
                ),
                
                tabPanel(
                  title = "Plot",
                  plotOutput("prcPlot", height = "800px"),
                  actionButton("downloadDialogButton", "Download")
                ),
                
                tabPanel(
                  title = "Permutation",
                  verbatimTextOutput("anovaOutput")
                )
              )
            )
          )
        )
      )
    )
  )
)
