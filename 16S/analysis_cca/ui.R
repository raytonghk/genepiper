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

SAMPLE_DOT_SIZE <- 1
SAMPLE_LABEL_SIZE <- 5
TAXA_DOT_SIZE <- 1
TAXA_LABEL_SIZE <- 5
SPIDER_LINE_SIZE <- 1
SPIDER_LABEL_SIZE <- 5
ELLIPSE_LINE_SIZE <- 1
ELLIPSE_SIGNIF <- 0.05
X_AXIS_TEXT_SIZE <- 10
X_AXIS_TEXT_ANGLE <- 0
Y_AXIS_TEXT_SIZE <- 10
Y_AXIS_TEXT_ANGLE <- 0
LEGEND_TEXT_SIZE <- 10
ENVFIT_VECTOR_LINE_SIZE <- 1
ENVFIT_VECTOR_LABEL_SIZE <- 5
ENVFIT_FACTOR_DOT_SIZE <- 1
ENVFIT_FACTOR_LABEL_SIZE <- 5
CONSTRAIN_LINE_SIZE <- 1
CONSTRAIN_LABEL_SIZE <- 5
CENTROID_SIZE <- 1
CENTROID_LABEL_SIZE <- 5

shinyUI(
  fluidPage(
    useShinyjs(),
    theme = "../style.css",
    tags$script(type = "text/javascript", src = "../www/busy.js"),
    tags$script(type = "text/javascript", src = "../www/controls.js"),
    tags$div(class = "busy", p("Busy..."), img(src = "../www/hour-glass.gif")),
    tags$script(HTML("window.onload = function() { parent.postMessage('analysisCCALoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h4("Canonical Correspondence Analysis (Unimodal)"),
        tags$div(
          class = "desc",
          p("In applied statistics, canonical correspondence analysis (CCA) is a multivariate constrained ordination technique that extracts major gradients among combinations of explanatory variables in a dataset. The requirements of a CCA are that the samples are random and independent. Also, the data are categorical and that the independent variables are consistent within the sample site and error-free."),
          a(target = "_blank", href = "https://en.wikipedia.org/wiki/Canonical_correspondence_analysis", "From Wikipedia")
        ),
        tags$div(
          class = "ref",
          h5("Legendre, P. and Legendre, L. (2012) Numerical Ecology. 3rd English ed. Elsevier."),
          h5("McCune, B. (1997) Influence of noisy environmental data on canonical correspondence analysis. Ecology 78, 2617-2623."),
          h5("Palmer, M. W. (1993) Putting things in even better order: The advantages of canonical correspondence analysis. Ecology 74,2215-2230."),
          h5("Ter Braak, C. J. F. (1986) Canonical Correspondence Analysis: a new eigenvector technique for multivariate direct gradient analysis. Ecology 67, 1167-1179.")
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
                tabsetPanel(
                  id = "parametersTabset",
                  tabPanel(
                    title = "General",
                    uiTaxRank(),
                    uiAbundanceType(),
                    uiPrepareDataButton()
                  ),
                  uiTabPanelTableXWithRankFilter(),
                  uiTabPanelTableY(),
                  
                  tabPanel(
                    title = "CCA",
                    textInput("ccaFormula", "Formula (Right Hand Side)", placeholder = "(Optional) e.g. A + B", width = "90%") %>%
                      shinyInput_label_embed(
                        shiny_iconlink() %>%
                          bs_embed_popover(title = "Table Y Design",
                                           placement = "left",
                                           html = "true",
                                           content = tags$p("For right hand side of the model formula, please refer to", tags$a(target = "_blank", href = "https://www.rdocumentation.org/packages/vegan/versions/2.4-2/topics/cca", "vegan::cca"), "."))
                      ),
                    actionButton("ccaButton", "Analysis"),
                    errorOutput("ccaMessage")
                  )
                )
              ),
              
              tags$div(
                class = "graphic-panel",
                conditionalPanel(
                  condition = "input.ccaTabset == 'Plot'",
                  h4("Graphic Parameters:"),
                  tabsetPanel(
                    id = "graphicTabset",
                    uiTabPanelPlotSampleDotLabel(SAMPLE_DOT_SIZE, SAMPLE_LABEL_SIZE),
                    uiTabPanelPlotTaxaDotLabel(TAXA_DOT_SIZE, TAXA_LABEL_SIZE),
                    
                    tabPanel(
                      title = "Biplot",
                      numericInput("constrainLineSize", "Line Width", CONSTRAIN_LINE_SIZE),
                      numericInput("constrainLabelSize", "Label Size", CONSTRAIN_LABEL_SIZE)
                    ),
                    
                    tabPanel(
                      title = "Centroid",
                      checkboxInput("plotCentroid", "Plot Centroids?", FALSE),
                      conditionalPanel(
                        condition = "input.plotCentroid == true",
                        numericInput("centroidSize", "Centroid Size", CENTROID_SIZE),
                        checkboxInput("labelCentroid", "Label Centroid?", FALSE),
                        conditionalPanel(
                          condition = "input.labelCentroid == true",
                          numericInput("centroidLabelSize", "Centroid Label Size", CENTROID_LABEL_SIZE)
                        )
                      )
                    ),
                    uiTabPanelPlotAxis2d(),
                    uiTabPanelGroupOrdination(SPIDER_LINE_SIZE, SPIDER_LABEL_SIZE, ELLIPSE_LINE_SIZE, ELLIPSE_SIGNIF),
                    uiTabPanelEnvfit(ENVFIT_VECTOR_LINE_SIZE, ENVFIT_VECTOR_LABEL_SIZE, ENVFIT_FACTOR_DOT_SIZE, ENVFIT_FACTOR_LABEL_SIZE),
                    uiTabPanelTitle(),
                    uiTabPanelXYAxis(X_AXIS_TEXT_SIZE, X_AXIS_TEXT_ANGLE, Y_AXIS_TEXT_SIZE, Y_AXIS_TEXT_ANGLE),
                    uiTabPanelLegend(LEGEND_TEXT_SIZE)
                  )
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.dataReady == true",
              tags$hr(),
              tabsetPanel(
                id = "resultTabset",
                tabPanel(
                  title = "Table X",
                  DT::dataTableOutput("tableX")
                ),
                
                tabPanel(
                  title = "Table Y",
                  DT::dataTableOutput("tableY")
                ),
                
                tabPanel(
                  title = "CCA",
                  tabsetPanel(
                    id = "ccaTabset",
                    tabPanel(
                      title = "Output",
                      verbatimTextOutput("ccaOutput"),
                      downloadButton("downloadCCAButton", "Download CCA")
                    ),
                    
                    uiTabPanelTaxaTable(),
                    uiTabPanelSampleTable(),
                    
                    tabPanel(
                      title = "Constraint",
                      DT::dataTableOutput("constraintTable"),
                      downloadButton("downloadConstraintTableButton")
                    ),
                    
                    tabPanel(
                      title = "Biplot",
                      DT::dataTableOutput("biplotTable")
                    ),
                    
                    tabPanel(
                      title = "Centroid",
                      DT::dataTableOutput("centroidTable")
                    ),
                    
                    uiTabPanelPermanova(),
                    
                    tabPanel(
                      title = "Plot",
                      plotOutput("ccaPlot", height = "800px"),
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
  )
)











































