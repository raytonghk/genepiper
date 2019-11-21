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

shinyUI(
  fluidPage(
    useShinyjs(),
    theme = "../style.css",
    tags$script(type = "text/javascript", src = "../www/busy.js"),
    tags$script(type = "text/javascript", src = "../www/controls.js"),
    tags$div(class = "busy", p("Busy..."), img(src = "../www/hour-glass.gif")),
    tags$script(HTML("window.onload = function() { parent.postMessage('analysisDCALoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h4("Detrended Correspondence Analysis (DCA)"),
        tags$div(
          class = "desc",
          p("Detrended correspondence analysis (DCA) is an exploratory multivariate statistical technique developed to correct for three major problems of Correspondence Analysis (CA): (1) The arch effect, (2) compression at the ends of the gradient, and (3) rare species have an unduly large influence the analysis (Legendre and Gallagher 2001). Arch effect is a mathematical artefact where distribution of objects along the first canonical axis is also partially mirrored in the second axis, because CA axes are constrained to be uncorrelated but not necessarily unrelated of each other (Hill & Gauch 1980). Because of this problem, the second CA axis is an artifact and cannot easily be interpreted. Because of the second problem, the spacing of samples (and species) along the first axis is not necessarily related to the amount of change (or beta diversity) along the primary gradient. These problems are corrected in three phases: by Detrending, Rescaling and Downweighting."),
          p("Detrending is the process of removing the arch effect. DCA does this by dividing the first axis into segments, and then by centering the second axis on zero (This process is much better described in Gauch 1982, Pielou 1984, Digby and Kempton 1987, and Kent and Coker 1992). More specifically, it starts by running a standard ordination (CA or reciprocal averaging) on the data, to produce the initial horse-shoe curve in which the 1st ordination axis distorts into the 2nd axis. It then divides the first axis into segments (default = 26), and rescales each segment to have mean value of zero on the 2nd axis - this effectively squashes the curve flat. It also rescales the axis so that the ends are no longer compressed relative to the middle, so that 1 DCA unit approximates to the same rate of turnover all the way through the data: the rule of thumb is that 4 DCA units mean that there has been a total turnover in the community. Species scores are taken as species optima. Species are scaled so that site scores are their direct weighted averages (alpha = 1). Note that while often useful, the detrending procedure does not always lead to an ordination output that better reflects the observed environmental gradients (see Legendre & Legendre 2012, for an in-depth discussion). DCA were shown to approximate well the ecological niche model and the typically expected Gaussian distribution of ecological variables (ter Braak & Looman 1986; ter Braak 1987).  A downside of DCA is that it can be sensitive to the number of segments used in the detrending algorithm (Jackson and Somers 1991)."),
          a(target = "_blank", href="https://github.com/raytonghk/genepiper/wiki/15.-Detrended-Correspondence-Analysis", "see also our tutorial about DCA")
        ),
        tags$div(
          class = "ref",
          h5("Digby PGN & Kempton RA (1987) Population and Community Biology Series: Multivariate Analysis of Ecological Communities. Chapman and Hall, London."),
h5("Gauch HGJ (1982) Multivariate Analysis and Community Structure. Cambridge University Press, Cambridge, 298pp."),
h5("Hill MO & Gauch HG (1980) Detrended correspondence analysis: an improved ordination technique. Vegetatio 42, 47--58."),
h5("Jackson DA & Somers KM (1991) The ups and downs of detrended correspondence analysis. The American Naturalist 137:704-712."),
h5("Kent M & Coker P (1992)  Vegetation description and analysis: a practical approach. Belhaven Press, London."),
h5("Legendre P & Gallagher ED (2001) Ecologically meaningful transformations for ordination of species data. Oecologia 129:271-280."),
h5("Legendre P & Legendre LFJ (2012) Numerical ecology. Vol. 24. Elsevier."),
h5("Oksanen J & Minchin PR (1997) Instability of ordination results under changes in input data order: explanations and remedies. Journal of Vegetation Science 8, 447-454."),
h5("Pielou EC (1984) The Interpretation of Ecological Data: A Primer on Classification and Ordination. Wiley, New York."),
h5("ter Braak CJF & Prentice IC (1988) A theory of gradient analysis. Advances in Ecological Research, 18, 271–317."),
h5("ter Braak CJF & Looman CWN (1986) Weighted averaging, logistic regression and the gaussian response model. Vegetatio, 65,3–11."),
h5("ter Braak CJF (1987) Unimodal Models to Relate Species to Environment. Agricultural Mathematics Group, Wageningen.")
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
                    uiTabPanelPlotTaxaDotLabel(TAXA_DOT_SIZE, TAXA_LABEL_SIZE),
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
                    uiTabPanelPlotTaxa3dLabel(),
                    uiTabPanelPlotAxis3d(),
                    uiTabPanelGroup3d()
                  )
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.dcaReady == true",
              tags$hr(),
              tabsetPanel(
                id = "resultTabset",
                tabPanel(
                  title = "Output",
                  verbatimTextOutput("dcaOutput"),
                  downloadButton("downloadDCAButton", "Download DCA")
                ),
                uiTabPanelSampleTable(),
                uiTabPanelTaxaTable(),
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


