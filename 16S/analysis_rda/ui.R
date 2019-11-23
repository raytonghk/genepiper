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
    tags$script(HTML("window.onload = function() { parent.postMessage('analysisRDALoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h4("Redundancy Analysis (Linear)"),
        tags$div(
          class = "desc",
          p("ReDundancy Analysis (RDA)is considered as an extension of PCA in which the main axes (components) are constrained to be linear combinations of the environmental variables (Rao, 1964). Two tables are necessary: one contains response (dependent) variables (species presence/absence or abundance) and one contains explanatory (predictive/ independent) variables (such as environmental variables or experimental treatments measured in the same samples or sites). Multiple linear regressions are used to ‘explain’ variation between independent and dependent variables, and these calculations are performed within the iterative procedure to find the best ordination of the objects. The interest of such an approach is to represent not only the main patterns of species variation as much as they can be explained by the measured environmental variables but also to display correlation coefficients between each species and each environmental variable in the dataset. ‘Redundancy’ expresses how much of the variance in the set of response variables is explained by the set of explanatory variables. The fraction of the total variance observed in response variables that is explained by all the explanatory variables is a useful indication of how much variance in the species distribution, for example, is due to differences in environmental factors between sites."),
          p("The output of RDA is an ordination that is usually shown on a two-dimensional ‘triplot’, with constrained RDA dimensions used as axes. Each object is depicted by a point, and response variables are represented by arrows originating from the coordinate system origin, and explanatory variables by either arrows (quantitative variables) or points (categorical variables). Because triplots display a lot of data on a single plot, their interpretation is more challenging. On a distance triplot, distances between objects represent the between-object similarity; the angles between arrows of response variables and arrows of explanatory variables represent the found associations between those variables. Finally, the projection of an object onto an arrow approximates the value of the corresponding variable in this object. A detailed description of the interpretation of ordination diagrams can be found in ter Braak & Verdonschot (1995) and Smilauer & Leps (2014)."),          
          p("Summarized from Ramette 2007, Paliy & Shankar 2016. "),
          p("GenePiper utlizes the 'rda' function from 'vegan' package to plot RDA. User may specify a taxonomic rank for the species data and select the environmental variables for the constrained ordination. "),
          a(target = "_blank", href="https://github.com/raytonghk/genepiper/wiki/19.-Redundancy-Analysis-RDA/", "See our tutorial on RDA here.")
        ),
        tags$div(
          class = "ref",
          h5("Legendre P & Legendre L (2012) Numerical Ecology. 3rd English ed. Elsevier."),
          h5("McCune B (1997) Influence of noisy environmental data on canonical correspondence analysis. Ecology 78, 2617-2623."),
          h5("Paliy O & Shankar V (2016) Application of multivariate statistical techniques in microbial ecology. Molecular ecology. 25(5):1032-57."),
          h5("Palmer MW (1993) Putting things in even better order: The advantages of canonical correspondence analysis. Ecology 74,2215-2230."),
          h5("Ramette A (2007) Multivariate analyses in microbial ecology. FEMS microbiology ecology. 62(2):142-60."),
          h5("Rao CR (1964) The use and interpretation of principal component analysis in applied research. Sankhya A 26:
329–358."),
        h5("Smilauer P & Leps J (2014) Multivariate Analysis of Ecological Data Using CANOCO 5. Cambridge University Press, Cambridge."),
          h5("ter Braak CJF (1986) Canonical Correspondence Analysis: a new eigenvector technique for multivariate direct gradient analysis. Ecology 67, 1167-1179."),
          h5("ter Braak CJF & Verdonschot PFM (1995) Canonical correspondence analysis and related multivariate methods in aquatic ecology. Aquatic Sciences, 57, 255–289.")
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
                    title = "RDA",
                    textInput("rdaFormula", "Formula (Right Hand Side)", placeholder = "(Optional) e.g. A + B", width = "90%") %>%
                      shinyInput_label_embed(
                        shiny_iconlink() %>%
                          bs_embed_popover(title = "Table Y Design",
                                           placement = "left",
                                           html = "true",
                                           content = tags$p("For right hand side of the model formula, please refer to", tags$a(target = "_blank", href = "https://www.rdocumentation.org/packages/vegan/versions/2.4-2/topics/cca", "vegan::cca"), "."))
                      ),
                    actionButton("rdaButton", "Analysis"),
                    errorOutput("rdaMessage")
                  )
                )
              ),
              
              tags$div(
                class = "graphic-panel",
                conditionalPanel(
                  condition = "input.rdaTabset == 'Plot'",
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
                  title = "RDA",
                  tabsetPanel(
                    id = "rdaTabset",
                    tabPanel(
                      title = "Output",
                      verbatimTextOutput("rdaOutput"),
                      downloadButton("downloadRDAButton", "Download RDA")
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
                      plotOutput("rdaPlot", height = "800px"),
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
















































