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
    tags$script(HTML("window.onload = function() { parent.postMessage('analysisPCoALoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h4("Principal Coordinates Analysis (PCoA)"),
        tags$div(
          class = "desc",
          p("Principal Coordinates Analysis (PCoA), also known as Multidimensional scaling (MDS), is an exploratory multivariate method commonly used for microbiome data. It is a conceptual extension of the PCA technique that it uses a linear (Euclidean) mapping of the distance or dissimilarities between objects (samples) onto the ordination space (i.e. projection in a Cartesian space), and the algorithm attempts to explain most of the variance in the original data set (Gower, 1966)."),
          p("As opposed to PCA, PCoA works with any dissimilarity measure and so specific association coefficients that better deal with the problem of the presence of many double zeros in data sets can be surmounted. Moreover, PCoA does not provide a direct link between the components and the original variables and so the interpretation of variable contribution may be more difficult. This is because PCoA components, instead of being linear combinations of the original variables as in PCA, are complex functions of the original variables depending on the selected dissimilarity measure. Besides, the non-Euclidean nature of some distance measures does not allow for a full representation of the extracted variation into a Euclidean ordination space. In that case, the non-Euclidean variation cannot be represented and the percent of total variance cannot be computed with exactness. The choice of the dissimilarity measure is thus of great importance, and subsequent transformation of the data to correct for negative eigenvalues is sometimes necessary (see Legendre & Legendre, 2012, section 9.2.4. for how to correct for such negative eigenvalues). "),
          p("Objects are represented as points in the ordination space. Eigenvalues are also used here to measure how much variance is accounted for by the largest synthetic variables on each PCoA synthetic axis. Although there is no direct, linear relationship between the components and the original variables, it is still possible to correlate object scores on the main axis (or axes) with the original variables to assess their contribution to the ordination."),
          p("Summarized from Ramette 2007, Paliy & Shankar 2016"),
          p("GenePiper utilises the `pcoa` function from the `ape` package for the PCoA analysis. Options for indirect gradient analyses are provided in the graphical parameters: Group and Envfit, where metadata could be mapped into the ordination by the colour of the data points, or by fitting the environmental vectors as arrows and dots via the vegan::envfit function that overlay onto the ordination."),
          a(target = "_blank", href="https://github.com/raytonghk/genepiper/wiki/17.-Principal-Coordinates-Analysis/", "See our tutorial on PCoA here.")
        ),
        tags$div(
          class = "ref",
          h5("Cailliez F (1983) The analytical solution of the additive constant problem. Psychometrika, 48, 305–308."),
          h5("Gower JC (1966) Some distance properties of latent root and vector methods used in multivariate analysis. Biometrika, 53, 325–338."),
          h5("Gower JC & Legendre P (1986) Metric and Euclidean properties of dissimilarity coefficients. Journal of Classification, 3, 5–48."),
          h5("Legendre P & Gallagher ED (2001) Ecologically meaningful transformations for ordination of species data. Oecologia, 129, 271–280."),
          h5("Legendre P & Legendre L (2012) Numerical Ecology. Vol. 24. Elsevier."),
          h5("Lingoes JC (1971) Some boundary conditions for a monotone analysis of symmetric matrices. Psychometrika, 36, 195–203."),
          h5("Paliy O & Shankar V. (2016) Application of multivariate statistical techniques in microbial ecology. Molecular ecology. 25(5):1032-57."),
          h5("Ramette A (2007) Multivariate analyses in microbial ecology. FEMS microbiology ecology. 62(2):142-60.")
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
                uiDistanceMethod(),
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
              condition = "output.pcoaReady == true",
              tags$hr(),
              tabsetPanel(
                id = "resultTabset",
                uiTabPanelDistanceMatrix(),
                
                tabPanel(
                  title = "Output",
                  tags$br(),
                  downloadButton("downloadPCoAButton", "Download PCoA")
                ),
                
                uiTabPanelEigenValues(),
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


