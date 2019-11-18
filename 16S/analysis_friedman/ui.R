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
    tags$script(HTML("window.onload = function() { parent.postMessage('analysisFriedmanLoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h4("Friedman Test"),
        tags$div(
          class = "desc",
          h5("The Friedman test is a non-parametric statistical test developed by Milton Friedman. Similar to the parametric repeated measures ANOVA, it is used to detect differences in treatments across multiple test attempts. The procedure involves ranking each row (or block) together, then considering the values of ranks by columns. Applicable to complete block designs, it is thus a special case of the Durbin test."),
          a(target = "_blank", href = "https://en.wikipedia.org/wiki/Friedman_test", "From Wikipedia")
        ),
        tags$div(
          class = "ref",
          h5("Myles Hollander and Douglas A. Wolfe (1973), Nonparametric Statistical Methods. New York: John Wiley & Sons. Pages 139–146.")
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
                uiTaxRank(),
                uiAbundanceType(c("Rarefied Count", "Relative Abundance")),
                uiGroupColumnWithLabel(),
                selectInput("blockColumn", "Block Column", NULL, width = "90%"),
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
                    title = "Wilcoxon Signed-rank",
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
              condition = "output.friedmanReady == true",
              tags$hr(),
              tags$div(
                tags$div(
                  class = "column-left",
                  DT::dataTableOutput("friedmanTable"),
                  downloadButton("downloadFriedmanTable", "Download Table")
                ),
                tags$div(
                  class = "column-right",
                  h5("The plot shows the post-hoc test with Wilcoxon Signed-rank test.", style = "padding-left: 10px"),
                  tags$br(),
                  plotOutput("friedmanPlot", height = "600px"),
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

























