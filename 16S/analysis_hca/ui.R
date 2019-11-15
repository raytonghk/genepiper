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

LINE_SIZE <- 1
LABEL_SIZE <- 1
PVC_LABEL_SIZE <- 1
PVR_LINE_SIZE <- 1
CUT_LINE_SIZE <- 1

shinyUI(
  fluidPage(
    useShinyjs(),
    theme = "../style.css",
    tags$script(type = "text/javascript", src = "../www/busy.js"),
    tags$script(type = "text/javascript", src = "../www/controls.js"),
    tags$div(class = "busy", p("Busy..."), img(src = "../www/hour-glass.gif")),
    tags$script(HTML("window.onload = function() { parent.postMessage('analysisHCALoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        tags$div(
          class = "desc",
          p("In data mining and statistics, hierarchical clustering (also called hierarchical cluster analysis or HCA) is a method of cluster analysis which seeks to build a hierarchy of clusters."),
          p("In order to decide which clusters should be combined (for agglomerative), or where a cluster should be split (for divisive), a measure of dissimilarity between sets of observations is required. In most methods of hierarchical clustering, this is achieved by use of an appropriate metric (a measure of distance between pairs of observations), and a linkage criterion which specifies the dissimilarity of sets as a function of the pairwise distances of observations in the sets."),
          a(target = "_blank", href="https://en.wikipedia.org/wiki/Hierarchical_clustering", "From Wikipedia")
        ),
        tags$div(
          class = "ref",
          h5("Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. Wadsworth & Brooks/Cole. (S version.)"),
          h5("Everitt, B. (1974). Cluster Analysis. London: Heinemann Educ. Books."),
          h5("Hartigan, J.A. (1975). Clustering Algorithms. New York: Wiley."),
          h5("Sneath, P. H. A. and R. R. Sokal (1973). Numerical Taxonomy. San Francisco: Freeman."),
          h5("Anderberg, M. R. (1973). Cluster Analysis for Applications. Academic Press: New York."),
          h5("Gordon, A. D. (1999). Classification. Second Edition. London: Chapman and Hall / CRC"),
          h5("Murtagh, F. (1985). “Multidimensional Clustering Algorithms”, in COMPSTAT Lectures 4. Wuerzburg: Physica-Verlag (for algorithmic details of algorithms used)."),
          h5("McQuitty, L.L. (1966). Similarity Analysis by Reciprocal Pairs for Discrete and Continuous Data. Educational and Psychological Measurement, 26, 825–831."),
          h5("Legendre, P. and L. Legendre (2012). Numerical Ecology, 3rd English ed. Amsterdam: Elsevier Science BV."),
          h5("Murtagh, Fionn and Legendre, Pierre (2014). Ward's hierarchical agglomerative clustering method: which algorithms implement Ward's criterion? Journal of Classification 31 (forthcoming).")
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
                uiAbundanceType(),
                uiDistanceMethod(),
                selectInput("agglomerateMethod", "Agglomeration Method", width = "90%",
                            choices = list(
                              "Ward" = "ward.D",
                              "Ward with clustering criterion" = "ward.D2",
                              "Single Linkage" = "single",
                              "Complete Linkage" = "complete",
                              "UPGMA" = "average",
                              "WPGMA" = "mcquitty"
                            )
                ),
                numericInput("nboot", "Number Of Bootstrap Replications", 10, 0, width = "90%"),
                uiAnalysisButton()
              ),
              
              tags$div(
                class = "graphic-panel",
                conditionalPanel(
                  condition = "input.resultTabset == 'Plot'",
                  h4("Graphic Parameters:"),
                  tabsetPanel(
                    tabPanel(
                      title = "Group",
                      selectInput("graphicGroupColumn", "Group Column", NULL),
                      conditionalPanel(
                        condition = "input.graphicGroupColumn != 'None'",
                        h5("Statistics:", style = "font-weight: bold"),
                        verbatimTextOutput("groupStatistics"),
                        downloadButton("downloadGroupStatisticsButton")
                      )
                    ),
                    
                    tabPanel(
                      title = "PVClust",
                      checkboxInput("plotPVClust", "Plot PVClust?", FALSE),
                      conditionalPanel(
                        condition = "input.plotPVClust == true",
                        numericInput("pvclustLabelSize", "Label Size", PVC_LABEL_SIZE)
                      ),
                      checkboxInput("plotPVRect", "Plot PVRect?", FALSE),
                      conditionalPanel(
                        condition = "input.plotPVRect == true",
                        numericInput("pvrectLineSize", "Line Width", PVR_LINE_SIZE),
                        numericInput("pvrectAlpha", "Alpha For PVRect", 0.95, 0, 1),
                        h5("Statistics:", style = "font-weight: bold"),
                        verbatimTextOutput("pvrectStatistics"),
                        downloadButton("downloadPVRectStatisticsButton")
                      )
                    ),
                    
                    tabPanel(
                      title = "Cut",
                      checkboxInput("plotCut", "Cut Tree?", FALSE),
                      conditionalPanel(
                        condition = "input.plotCut == true",
                        numericInput("cutLineSize", "Line Width", CUT_LINE_SIZE),
                        numericInput("cutK", "k", 2, 2),
                        h5("Statistics:", style = "font-weight: bold"),
                        verbatimTextOutput("cutStatistics"),
                        downloadButton("downloadCutStatisticsButton")
                      )
                    ),
                    uiTabPanelLine(LINE_SIZE),
                    uiTabPanelLabel(LABEL_SIZE)
                  )
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.hcaReady == true",
              tags$hr(),
              tabsetPanel(
                id = "resultTabset",
                tabPanel(
                  title = "Output",
                  verbatimTextOutput("hcaOutput"),
                  downloadButton("downloadHCAButton", "Download HCA")
                ),
                
                tabPanel(
                  title = "Plot",
                  plotOutput("plotHca", height = "800px"),
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


















































