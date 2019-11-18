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

DOT_SIZE <- 1
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
    tags$script(HTML("window.onload = function() { parent.postMessage('analysisKruskalLoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
        
    fluidRow(
      wellPanel(
        class = "desc-box",
        h4("Kruskal-Wallis Test"),
        tags$div(
          class = "desc",
          h5("The Kruskal–Wallis test by ranks, Kruskal–Wallis H test (named after William Kruskal and W. Allen Wallis), or one-way ANOVA on ranks is a non-parametric method for testing whether samples originate from the same distribution. It is used for comparing two or more independent samples of equal or different sample sizes. It extends the Mann–Whitney U test, which is used for comparing only two groups. The parametric equivalent of the Kruskal–Wallis test is the one-way analysis of variance (ANOVA)."),
          a(target = "_blank", href = "https://en.wikipedia.org/wiki/Kruskal–Wallis_one-way_analysis_of_variance", "From Wikipedia")
        ),
        tags$div(
          class = "ref",
          h5("Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 115–120.")
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
                uiAbundanceType(c("Rarefied Count", "Relative Abundance")),
                uiGroupColumnWithLabel(),
                uiSignif(),
                uiAnalysisButton()
              ),
              
              tags$div(
                class = "graphic-panel",
                h4("Graphic Parameters:"),
                tabsetPanel(
                  id = "graphicTabset",
                  tabPanel(
                    title = "Plot Type",
                    selectInput("plotType", "Plot Type", choices = list("Box Plot" = "box",
                                                                        "Notched Box Plot" = "notch",
                                                                        "Violin Plot" = "violin",
                                                                        "Dot Box Plot" = "dot"),
                                width = "90%")
                  ),
                  
                  tabPanel(
                    title = "Mann Whitney",
                    checkboxInput("signifAnnotation", "Significant Annotation?", TRUE)
                  ),
                  
                  uiTabPanelDot(DOT_SIZE, step = 0.1),
                  uiTabPanelTitle(),
                  uiTabPanelXYAxis(X_AXIS_TEXT_SIZE, X_AXIS_TEXT_ANGLE, Y_AXIS_TEXT_SIZE, Y_AXIS_TEXT_ANGLE),
                  uiTabPanelLegend(LEGEND_TEXT_SIZE)
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.kruskalReady == true",
              tags$hr(),
              tags$div(
                tags$div(
                  class = "column-left",
                  DT::dataTableOutput("kruskalTable"),
                  downloadButton("downloadKruskalTable", "Download Table")
                ),
                tags$div(
                  class = "column-right",
                  h5("The plot shows the post-hoc test with Mann-Whitney test.", style = "padding-left: 10px"),
                  tags$br(),
                  plotOutput("kruskalPlot", height = "600px"),
                  actionButton("downloadDialogButton", "Download Plot")
                )
              )
            )
          )
        )
      )
    )
  )
)
