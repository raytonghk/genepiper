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
TAXA_LINE_SIZE <- 1
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
    tags$script(HTML("window.onload = function() { parent.postMessage('analysisPCALoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h4("Principal Component Analysis (PCA)"),
        tags$div(
          class = "desc",
          p("Principal components analysis (PCA) is one of the most widely used and one of the oldest methods of ordination analyses (Pearson 1901). It is a statistical procedure that uses an orthogonal transformation to convert a set of observations of possibly correlated variables into a set of values of linearly uncorrelated synthetic variables called principal components. The number of distinct principal components is equal to the smaller of the number of original variables or the number of observations minus one. Each principal component (PC) is a linear combination of original variables calculated so that the first PC represents an axis in the multidimensional data space that would produce the largest dispersion of values along this component. Other principal components are calculated as orthogonal to the preceding components and similarly are positioned along the largest remaining scatter of the values. Thus, PCA creates a rotation of the original system of coordinates so that the new axes (principal components) are orthogonal to each other and correspond to the directions of largest variance in the data set. PCA is sensitive to the relative scaling of the original variables."),
          p("By definition, the first PC axis of the PCA output represents the largest gradient of variability in the data set, PC2 axis—the second largest, and so forth, until all data set variability has been accounted for. Each object can thus be given a new set of coordinates in the principal components space, and the distribution of objects in that space will correspond to the similarity of the variables’ scores in those objects.
"),
        p("Because PCA uses Euclidean distance to measure dissimilarity among objects, care should be taken when using PCA on a data set with many zeroes, as is often the case for data with long gradients. As described in detail by Legendre & Gallagher (2001), when run on such data sets, PCA can generate severe artefacts such as horseshoe visualisation effect (see Legendre & Legendre 2012; ter Braak & Smilauer 2015, for examples). With this artefact, objects at the edges of the environmental gradient actually appear close to each other in the ordination space (Novembre & Stephens 2008). While the horseshoe effect can be partially reduced by processing of the original data values through a chord or Hellinger transformation before running PCA (Legendre & Gallagher 2001), the use of correspondence analysis (CA) is usually advocated for such data sets (ter Braak & Smilauer 2015). "),
          p("Thus PCA should generally be used when the objects (sites or samples) cover very short gradients, i.e. when the same species are mostly identified everywhere in the study area (i.e., when samples mostly differ in species abundances), and when species linearly respond to environmental gradients. Because those conditions are often not met in ecological studies, other multivariate approaches have been progressively preferred over PCA such as correspondence analysis (CA) or multidimensional scaling (PCoA). "),
          p("PCA is successful when most of the variance is accounted for by the largest (generally the first two or three) components. The amount of variance accounted for by each principal component is given by its ‘eigenvalue.’ Eigenvalues derived from a PCA are generally considered to be significant when their values are larger than the average of all eigenvalues (Legendre & Legendre, 2012). The cumulative percentage of variance accounted for by the largest components indicates how much proportion of the total variance is depicted by the actual ordination. High absolute correlation values between the synthetic variables (principal components) and the original variables are useful to identify which variables mainly contribute to the variation in the data set, and this is referred to as the loading of the variables on a given axis. However, because the synthetic and original variables are linearly correlated (i.e. they are not independent), standard tests to determine the statistical significance of the correlations between them cannot be used."),
          p("Adopted from Ramette 2007, Paily & Shankar 2016"),
          p("GenePiper utilises the prcomp function from the stats package to plot PCA at a user-specified taxonomic rank. Options for indirect gradient analyses are provided in the graphical parameters: Group and Envfit, where metadata could be mapped into the ordination by the colour of the data points, or by fitting the environmental vectors as arrows and dots via the vegan::envfit function that overlay onto the ordination."),
          a(target = "_blank", href="https://github.com/raytonghk/genepiper/wiki/16.-Principal-Component-Analysis", "See our tutorial about PCA.")
        ),
        tags$div(
          class = "ref",
          h5("Anderson MJ (2001) A new method for non-parametric multivariate analysis of variance. Austral Ecology, 26: 32–46."),
          h5("Becker RA, Chambers JM & Wilks AR (1988) The New S Language. Wadsworth & Brooks/Cole."),                 
          h5("Excoffier L, Smouse PE & Quattro JM (1992) Analysis of molecular variance inferred from metric distances among DNA haplotypes: Application to human mitochondrial DNA restriction data. Genetics, 131:479–491."),
          h5("Legendre P &  Anderson MJ (1999) Distance-based redundancy analysis: Testing multispecies responses in multifactorial ecological experiments. Ecological Monographs, 69:1–24."),
          h5("Legendre P & Gallagher ED (2001) Ecologically meaningful transformations for ordination of species data. Oecologia 129:271-280."),
          h5("Legendre P & Legendre LFJ (2012) Numerical ecology. Vol. 24. Elsevier."),
          h5("Mardia KV, Kent JT & Bibby JM (1979) Multivariate Analysis, London: Academic Press."),
          h5("McArdle BH & Anderson MJ (2001) Fitting multivariate models to community data: A comment on distance-based redundancy analysis. Ecology, 82: 290–297."),
          h5("Novembre J & Stephens M (2008) Interpreting principal component analyses of spatial population genetic variation. Nature Genetics, 40, 646–649."),
          h5("Paliy O & Shankar V. (2016) Application of multivariate statistical techniques in microbial ecology. Molecular ecology. 25(5):1032-57."),
          h5("Pearson K (1901) On lines and planes of closest fit to systems of points in space. Philosophical Magazine, 2, 559–572."),
          h5("Ramette A (2007) Multivariate analyses in microbial ecology. FEMS microbiology ecology. 62(2):142-60."),
          h5("ter Braak CJF & Smilauer P (2015) Topics in constrained and unconstrained ordination. Plant Ecology, 216, 683–696."),
          h5("Venables WN & Ripley BD (2002) Modern Applied Statistics with S, Springer-Verlag."), 
          h5("Warton DI, Wright TW & Wang Y (2012) Distance-based multivariate analyses confound location and dispersion effects. Methods in Ecology and Evolution, 3, 89–101.")
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
                checkboxInput("scaleVariable", "Scale Variables?", FALSE),
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
                    uiTabPanelPlotTaxaLIneLabel(TAXA_LINE_SIZE, TAXA_LABEL_SIZE),
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
              condition = "output.pcaReady == true",
              tags$hr(),
              tabsetPanel(
                id = "resultTabset",
                tabPanel(
                  title = "Output",
                  verbatimTextOutput("pcaOutput"),
                  downloadButton("downloadPCAButton", "Download PCA")
                ),
                uiTabPanelSampleTable(),
                uiTabPanelTaxaTable("Loading"),
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
