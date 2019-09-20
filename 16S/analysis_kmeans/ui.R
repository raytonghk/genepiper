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

CENTER_DOT_SIZE <- 1
SAMPLE_DOT_SIZE <- 1
SAMPLE_LABEL_SIZE <- 5
SPIDER_LINE_SIZE <- 1
SPIDER_LABEL_SIZE <- 5
ELLIPSE_LINE_SIZE <- 1
ELLIPSE_SIGNIF <- 0.05
X_AXIS_TEXT_SIZE <- 10
X_AXIS_TEXT_ANGLE <- 0
Y_AXIS_TEXT_SIZE <- 10
Y_AXIS_TEXT_ANGLE <- 0
LEGEND_TEXT_SIZE <- 10
CH_LINE_SIZE <- 1
CH_X_AXIS_TEXT_SIZE <- 10
CH_X_AXIS_TEXT_ANGLE <- 0
CH_Y_AXIS_TEXT_SIZE <- 10
CH_Y_AXIS_TEXT_ANGLE <- 0

shinyUI(
  fluidPage(
    useShinyjs(),
    theme = "../style.css",
    tags$script(type = "text/javascript", src = "../www/busy.js"),
    tags$script(type = "text/javascript", src = "../www/controls.js"),
    tags$div(class = "busy", p("Busy..."), img(src = "../www/hour-glass.gif")),
    tags$script(HTML("window.onload = function() { parent.postMessage('analysisKmeansLoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h4("K-Means Clustering"),
        tags$div(
          class = "desc",
          p("k-means clustering is a method of vector quantization, originally from signal processing, that is popular for cluster analysis in data mining. k-means clustering aims to partition n observations into k clusters in which each observation belongs to the cluster with the nearest mean, serving as a prototype of the cluster. "),
          a(target = "_blank", href = "https://en.wikipedia.org/wiki/K-means_clustering", "From Wikipedia")
        ),
        tags$div(
          class = "ref",
          h5("Forgy, E. W. (1965) Cluster analysis of multivariate data: efficiency vs interpretability of classifications. Biometrics 21, 768–769."),
          h5("Hartigan, J. A. and Wong, M. A. (1979). A K-means clustering algorithm. Applied Statistics 28, 100–108."),
          h5("Lloyd, S. P. (1957, 1982) Least squares quantization in PCM. Technical Note, Bell Laboratories. Published in 1982 in IEEE Transactions on Information Theory 28, 128–137."),
          h5("MacQueen, J. (1967) Some methods for classification and analysis of multivariate observations. In Proceedings of the Fifth Berkeley Symposium on Mathematical Statistics and Probability, eds L. M. Le Cam & J. Neyman, 1, pp. 281–297. Berkeley, CA: University of California Press.")
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
            h4("Parameters:"),
            tags$div(
              tags$div(
                class = "parameter-panel",
                uiTaxRank(),
                uiAbundanceType(),
                selectInput("dimensionReductionMethod", "Dimension Reduction Method", width = "90%",
                            choices = list("Correspondence Analysis" = "ca",
                                           "Detrended Correspondence Analysis" = "dca",
                                           "Principal Component Analysis" = "pca",
                                           "Principal Coordinates Analysis" = "pcoa",
                                           "Non-Metric Multidimensional Scaling" = "nmds")
                ),
                conditionalPanel(
                  condition = "input.dimensionReductionMethod == 'pcoa' | input.dimensionReductionMethod == 'nmds'",
                  uiDistanceMethod()
                ),
                uiPlotButton()
              ),
              
              tags$div(
                class = "graphic-panel",
                conditionalPanel(
                  condition = "output.plotTableReady == true",
                  h4("K-Means Parameters"),
                  tabsetPanel(
                    id = "kmeansTabset",
                    uiTabPanelPlotAxis(),
                    
                    tabPanel(
                      title = "K-Means",
                      selectInput("kMeansType", "K-Means Type",
                                  choices = list("K-Means" = "km",
                                                 "Kernel K-Means" = "kkm",
                                                 "Spectral Clustering" = "sc")
                      ),
                      numericInput("k", "K", 2, 2),
                      conditionalPanel(
                        condition = "input.kMeansType != 'km'",
                        selectInput("kernel", "Kernel",
                                    choices = list("Radial Basis Kernel" = "rbfdot",
                                                   "Polynomial Kernel" = "polydot",
                                                   "Linerar Kernel" = "vanilladot",
                                                   "Hyperbolic Tangent Kernel" = "tanhdot",
                                                   "Laplacian Kernel" = "laplacedot",
                                                   "Bessel Kernel" = "besseldot",
                                                   "ANOVA RBF Kernel" = "anovadot",
                                                   "Spline Kernel" = "splinedot",
                                                   "String Kernel" = "stringdot")
                        )
                      ),
                      conditionalPanel(
                        condition = "input.kMeansType == 'km'",
                        selectInput("kMeansAlgorithm", "Algorithm", c("Hartigan-Wong", "Lloyd", "MacQueen"))
                      ),
                      checkboxInput("plotCenter", "Plot Centers?", FALSE),
                      conditionalPanel(
                        condition = "input.plotCenter == true",
                        numericInput("centerDotSize", "Dot Size", CENTER_DOT_SIZE)
                      )
                    ),
                    
                    tabPanel(
                      title = "Graphic",
                      conditionalPanel(
                        condition = "input.resultTabset == 'Plot'",
                        tabsetPanel(
                          tabPanel(
                            title = "Sample",
                            numericInput("sampleDotSize", "Dot Size", SAMPLE_DOT_SIZE),
                            uiConditionalLabelSampleSize(SAMPLE_LABEL_SIZE)
                          ),
                          
                          tabPanel(
                            title = "Cluster",
                            checkboxInput("plotConvexHull", "Plot Convex Hull?", FALSE),
                            uiConditionalPlotSpider(SPIDER_LINE_SIZE, SPIDER_LABEL_SIZE),
                            uiConditionalPlotEllipse(ELLIPSE_LINE_SIZE, ELLIPSE_SIGNIF)
                          ),
                          uiTabPanelTitle(),
                          uiTabPanelXYAxis(X_AXIS_TEXT_SIZE, X_AXIS_TEXT_ANGLE, Y_AXIS_TEXT_SIZE, Y_AXIS_TEXT_ANGLE),
                          uiTabPanelLegend(LEGEND_TEXT_SIZE)
                        )
                      ),
                      conditionalPanel(
                        condition = "input.resultTabset == 'Calinski-Harabasz Index (K-Means Only)'",
                        tabsetPanel(
                          uiTabPanelLine(CH_LINE_SIZE, "Ch"),
                          uiTabPanelTitle("Ch"),
                          uiTabPanelXYAxis(CH_X_AXIS_TEXT_SIZE, CH_X_AXIS_TEXT_ANGLE, CH_Y_AXIS_TEXT_SIZE, CH_Y_AXIS_TEXT_ANGLE, "Ch")
                        )
                      )
                    )
                  )
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.plotTableReady == true",
              tags$hr(),
              tabsetPanel(
                id = "resultTabset",
                tabPanel(
                  title = "Output",
                  verbatimTextOutput("kMeansOutput"),
                  errorOutput("kMeansMessage"),
                  downloadButton("donwloadKMeans")
                ),
                
                tabPanel(
                  title = "Plot",
                  plotOutput("plotDimensionReduction", height = "800px"),
                  actionButton("downloadDialogButton", "Download")
                ),
                
                tabPanel(
                  title = "Comparison",
                  selectInput("groupColumnToCompare", "Group Column To Compare", NULL, width = "45%"),
                  verbatimTextOutput("compareOutput")
                ),
                
                tabPanel(
                  title = "Calinski-Harabasz Index (K-Means Only)",
                  plotOutput("cHIndexPlot", height = "800px"),
                  errorOutput("cHIndexMessage"),
                  actionButton('downloadCHIndexDialogButton', "Download")
                )
              )
            )
          )
        )
      )
    )
  )
)













































