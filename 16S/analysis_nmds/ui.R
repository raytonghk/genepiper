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

shinyUI(
  fluidPage(
    useShinyjs(),
    theme = "../style.css",
    tags$script(type = "text/javascript", src = "../www/busy.js"),
    tags$script(type = "text/javascript", src = "../www/controls.js"),
    tags$div(class = "busy", p("Busy..."), img(src = "../www/hour-glass.gif")),
    tags$script(HTML("window.onload = function() { parent.postMessage('analysisNMDSLoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h4("Non-metric multidimensional scaling (NMDS)"),
        tags$div(
          class = "desc",
          p("Non-metric multidimensional scaling (NMDS) is an exploratory multivariate method generally efficient at identifying underlying gradients and at representing relationships based on various types of distance measures. The NMDS algorithm ranks distances between objects, and uses these ranks to map the objects nonlinearly onto a simplified, two-dimensional ordination space so as to preserve their ranked differences, and not the original distances (Shepard, 1966). The procedure works as follows: the objects are first placed randomly in the ordination space (the desired number of dimensions has to be defined a priori), and their distances in this initial configuration are compared by monotonic regression with the distances in the original data matrix based on a stress function (values between 0 and 1). The latter indicates how different the ranks on the ordination configuration are from the ranks in the original distance matrix. Several iterations of the NMDS procedure are generally implemented so as to obtain the lowest stress value possible (i.e. the best goodness of fit) based on different random initial positions of the objects in the ordination space. "),
          p("In NMDS ordination, the proximity between objects corresponds to their similarity, but the ordination distances do not correspond to the original distances among objects. Because NMDS preserves the order of objects, NMDS ordination axes can be freely rescaled, rotated, or inverted, as needed for a better visualisation or interpretation. Because of the iterative procedure, NMDS is more computer intensive than eigenanalyses such as PCoA, PCA, or CA. However, constant improvement in computing power makes this limitation less of a problem nowadays."),
          p("NMDS makes few assumptions about the distribution of data, and is often used for biological molecular data. For data sets with many different gradients of variance, NMDS ordination can be superior to that of other ordination techniques (Minchin 1987). That is because with N axis = 2 defined before the analysis, all data set variance is utilised to distribute objects in a two-dimensional NMDS ordination plot, whereas the first two dimensions of the PCA/CA/PCoA ordination only display a part of that variance (Legendre & Legendre 2012). NMDS, however, does not perform a simultaneous ordination of both variables and objects. Note that NMDS is not an eigenvector-based gradient analysis technique but rather is a mapping method. Each of its ordination axes does not correspond to a particular gradient in the original data set, and its goal is to represent ranks of pairwise dissimilarities among objects."),
          p("Summarized from Ramette 2007, Paliy & Shankar 2016. "),
          p("GenePiper utlizes the 'metaMDS' function from 'vegan' package to plot NMDS. Options for indirect gradient analyses are provided in the graphical parameters: Group and Envfit, where metadata could be mapped into the ordination by the colour of the data points, or by fitting the environmental vectors as arrows and dots via the vegan::envfit function that overlay onto the ordination."),
          a(target = "_blank", href="https://github.com/raytonghk/genepiper/wiki/18.-Non-metric-Multidimensional-Scaling", "See our tutorial on NMDS here.")
        ),
        tags$div(
          class = "ref",
          h5("Faith DP, Minchin PR & Belbin L (1987) Compositional dissimilarity as a robust measure of ecological distance. Vegetatio 69, 57–68."),
          h5("Legendre P & Legendre L (2012) Numerical Ecology. Vol. 24. Elsevier."),
          h5("Minchin PR (1987) An evaluation of relative robustness of techniques for ecological ordinations. Vegetatio 69, 89–107."),
          h5("Oksanen J (2007) Multivariate analysis of ecological communities in R: vegan tutorial, http://cc.oulu.fi/~jarioksa/opetus/metodi/vegantutor.pdf)"),
          h5("Oksanen J (2015) Vegan: an introduction to ordination, https://cran.r-project.org/web/packages/vegan/vignettes/intro-vegan.pdf"),
          h5("Paliy O & Shankar V (2016) Application of multivariate statistical techniques in microbial ecology. Molecular ecology. 25(5):1032-57."),
          h5("Ramette A (2007) Multivariate analyses in microbial ecology. FEMS microbiology ecology. 62(2):142-60."),
          h5("Shepard RN (1966) Metric structures in ordinal data. J Math Psychol 3: 287–315.")
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
                tags$div(
                  h5("Number Of Try:", style = "font-weight: bold"),
                  tags$div(
                    class = "column-left",
                    style = "width: 40%",
                    numericInput("try", "Min", 20, width = "90%")
                  ),
                  tags$div(
                    class = "column-right",
                    sytle = "width: 40%",
                    numericInput("tryMax", "Max", 20, width = "90%")
                  )
                ),
                uiAnalysisButton()
              ),
              tags$div(
                class = "graphic-panel",
                conditionalPanel(
                  condition = "input.resultTabset == '2D Plot'",
                  h4("Graphic Parameters:"),
                  tabsetPanel(
                    id = "graphicTabset",
                    uiTabPanelPlotSampleDotLabel(SAMPLE_DOT_SIZE, SAMPLE_LABEL_SIZE),
                    uiTabPanelPlotAxis2d(),
                    uiTabPanelGroupOrdination(SPIDER_LINE_SIZE, SPIDER_LABEL_SIZE, ELLIPSE_LINE_SIZE, ELLIPSE_SIGNIF),
                    uiTabPanelEnvfit(ENVFIT_VECTOR_LINE_SIZE, ENVFIT_VECTOR_LABEL_SIZE, ENVFIT_FACTOR_DOT_SIZE, ENVFIT_FACTOR_LABEL_SIZE),
                    uiTabPanelTitle(),
                    uiTabPanelXYAxis(X_AXIS_TEXT_SIZE, X_AXIS_TEXT_ANGLE, Y_AXIS_TEXT_SIZE, Y_AXIS_TEXT_ANGLE),
                    uiTabPanelLegend(LEGEND_TEXT_SIZE)
                  )
                ),
                
                conditionalPanel(
                  condition = "input.resultTabset == '3D Plot'",
                  h4("Graphic Parameters:"),
                  tabsetPanel(
                    uiTabPanelPlotSample3dLabel(),
                    uiTabPanelPlotAxis3d(),
                    uiTabPanelGroup3d()
                  )
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.nmdsReady == true",
              tags$hr(),
              tabsetPanel(
                id = "resultTabset",
                uiTabPanelDistanceMatrix(),
                
                tabPanel(
                  title = "Output",
                  verbatimTextOutput("nmdsOutput"),
                  downloadButton("downloadNMDSButton", "Download NMDS")
                ),
                
                uiTabPanelSampleTable(),
                uiTabPanelPermanova(),
                uiTabPanelPlot2d(),
                uiTabPanelPlot3d()
              )
            )
          )
        )
      )
    )
  )
)


