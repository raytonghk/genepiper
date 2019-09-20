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

DOT_SIZE <- 6
CORRELATION_FILTER <- 0.5
N_FILTER <- 5
P_VALUES_FILTER <- 0.05
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
    tags$script(HTML("window.onload = function() { parent.postMessage('analysisCorrelationLoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h4("Correlation Analysis"),
        tags$div(
          class = "desc",
          p("In statistics, dependence or association is any statistical relationship, whether causal or not, between two random variables or bivariate data. Correlation is any of a broad class of statistical relationships involving dependence, though in common usage it most often refers to how close two variables are to having a linear relationship with each other. Familiar examples of dependent phenomena include the correlation between the physical statures of parents and their offspring, and the correlation between the demand for a limited supply product and its price."),
          a(target = "_blank", herf = "https://en.wikipedia.org/wiki/Correlation_and_dependence", "From Wikipedia")
        ),
        tags$div(
          class = "ref",
          h5("Hollander M. and Wolfe D.A. (1973). Nonparametric Statistical Methods. New York: Wiley."),
          h5("Press WH, Flannery BP, Teukolsky SA, Vetterling, WT (1988): Numerical Recipes in C. Cambridge: Cambridge University Press.")
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
                selectInput("correlationType", "Correlation Method", width = "90%",
                            choices = list("Pearson" = "pearson",
                                           "Spearman" = "spearman")
                ),
                tags$div(
                  tags$div(
                    class = "column-left",
                    checkboxInput("logTransform", "Log Transform Abundance?", FALSE)
                  ),
                  tags$div(
                    class = "column-right",
                    checkboxInput("includeAlphaIndex", "Include Alpha Index?", FALSE)
                  )
                ),
                checkboxInput("changeZeroToNA", "Change Zero To NA?", FALSE),
                uiAnalysisButton()
              ),
              
              tags$div(
                class = "graphic-panel",
                conditionalPanel(
                  condition = "output.correlationReady == true",
                  h4("Correlation Parameters:"),
                  tabsetPanel(
                    id = "correlationTabset",
                    tabPanel(
                      title = "Filter",
                      numericInput("correlationFilter", "Correlation Filter", CORRELATION_FILTER, 0, 1),
                      numericInput("nFilter", "Number Of Samples Filter", N_FILTER, 0),
                      numericInput("pValueFilter", "P Values Filter", P_VALUES_FILTER, 0, 1)
                    ),
                    
                    tabPanel(
                      title = "Graphic",
                      tabsetPanel(
                        tabPanel(
                          title = "Display",
                          selectInput("xAxisType", "X Axis Type", NULL),
                          checkboxGroupInput("xAxisLabel", "Select Labels:", NULL),
                          selectInput("yAxisType", "Y Axis Type", NULL),
                          checkboxGroupInput("yAxisLabel", "Select Labels:", NULL)
                        ),
                        uiTabPanelDot(DOT_SIZE),
                        uiTabPanelTitle(),
                        uiTabPanelXYAxis(X_AXIS_TEXT_SIZE, X_AXIS_TEXT_ANGLE, Y_AXIS_TEXT_SIZE, Y_AXIS_TEXT_ANGLE),
                        uiTabPanelLegend(LEGEND_TEXT_SIZE)
                      )
                    )
                  )
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.correlationReady == true",
              tags$hr(),
              tabsetPanel(
                id = "resultTabset",
                tabPanel(
                  title = "Table",
                  DT::dataTableOutput("correlationTable"),
                  downloadButton("downloadCorrelationButton", "Download Correlation Table")
                ),
                
                tabPanel(
                  title = "Plot",
                  plotOutput("correlationPlot", height = "800px"),
                  actionButton("downloadDialogButton", "Download")
                )
              )
            )
          )
        )
      )
    )
  )
)















































