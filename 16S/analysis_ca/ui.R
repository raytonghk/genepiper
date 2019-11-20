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
    tags$script(HTML("window.onload = function() { parent.postMessage('analysisCALoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h3("Correspondence Analysis"),
        tags$div(
          class = "desc",
          h4("Correspondence Analysis is an exploratory technique designed to find relationships (correspondence) between  samples and species from a table of counted data (or any dimensionally homogenous table) and to represent these relationships in an ordination space (Hill 1973, 1974). Noticeably, instead of maximising the amount of variance explained by the ordination, CA maximises the correspondence between species scores and sample scores. Several algorithms exist and the most commonly described one is reciprocal averaging, which consists of (1) assigning arbitrary numbers to all species in the table (these are the initial species scores), (2) for each sample, a sample score is then determined as a weighted average of all species scores (this thus takes into account the abundance of each species at the site and the previously determined species scores), (3) for each species, a new species score is then calculated as the weighted average of all the sample scores, (4) both species scores and sample scores are standardised again to obtain a mean of zero and a SD of one, and (5) steps 2 - 4 are repeated until species and site scores converge towards stable solutions in successive iterations (Hill, 1974). The overall table variance (inertia) based on Chi Square distances is decomposed into successive components that are uncorrelated to each other, as in the PCA or PCoA procedures. For each axis, the overall correspondence between species scores and sample scores is summarised by an eigenvalue, and the latter is thus equivalent to a correlation coefficient between species scores and sample scores (Gauch, 1982)."),
          h4("The technique is popular among ecologists because CA is particularly recommended when species display unimodal (bell shaped or Gaussian) relationships with environmental gradients (ter Braak, 1985), as it happens when a species favours specific values of a given environmental variable, which is revealed by a peak of abundance or presence when the optimal conditions are met (this can be visualised by plotting species abundance against the environmental parameter). The unimodal model that supports the concept of ecological niches has also been shown to be of the right order of complexity for the ordination of most ecological data (ter Braak & Prentice, 1988). Although examples of unimodal distributions along variables or environmental gradients exist with macroorganisms (ter Braak, 1985), the shape of the distribution of the abundance of microbial species along environmental parameters or gradients has not been extensively investigated (but see Ramette & Tiedje, 2007a, b). This may arise from the fact that, in microbial surveys, environmental sampling is mostly performed blindly in relation to environmental heterogeneity, and the abundance of target species is generally determined without systematically analysing associated environmental parameters. Finally, another important feature of CA for microbial ecologists is that the reciprocal averaging algorithm disregards species double absences because the relationships between rows and columns of the table are quantified using the Chi Square coefficient that excludes double absences (Legendre & Legendre, 2012)."),
          h4("Both samples and taxa are often jointly depicted in the ordination space, where the centre of inertia (centroid) of their scores corresponds to zero for all axes. Depending on the choice of the scaling type, either the ordination of rows (samples) or the columns (species) is meaningful, and can be interpreted as an approximation of the Chi Square distances between samples or species, respectively. Sample points that are close to each other are similar with regard to the pattern of relative frequencies across species. It is important to remember that in such joint plots, either distances between sample points or distances between species points can be interpreted, but not the distances between sample and species points. Indeed, these distances are not simple Euclidean distances computed from the relative row or column frequencies, but rather they are weighted distances. The proximity between sample and species points in the plot can thus be understood as a probability of species occurrence or of a high abundance in the samples in the vicinity of a species point."),
          h4("In scaling 2 (i.e. focus on species), species points found at the centre of the ordination space should be carefully checked with the raw data to clarify whether the species ordination really corresponds to the optimal abundance or occurrence of the species, or whether the species is just badly represented by the main axes, as it is the case when other axes are more appropriate to represent the species. Rare species contribute little to the total table inertia (i.e. they only play a minor role in the overall table variance) and are hence positioned at the edges of the plot, next to the sample(s) where they occur. In general, only the species points found away from the ordination centre and not close to the edges of the ordination have more chances to be related to the ordination axes, i.e. to contribute to the overall variance (Legendre & Legendre, 2012)."),
          h4("When the species composition of the samples progressively changes along the environmental gradient, sample positions may appear in the ordination plot as nonlinear configurations called ‘arch’ (Gauch, 1982) (or ‘horseshoe’ in the case of PCA), which may impair further ecological interpretation. In CA, the arch effect may be mathematically produced as a side-effect of the CA procedure that tries to obtain axes that both maximally separate species and that are uncorrelated to each other (ter Braak, 1987): when the first axis suffices to correctly order the samples and species, a second axis (uncorrelated with the former) can be obtained by folding the first axis in the middle and bringing its extremities together, thus resulting in an arch configuration. Further axes can be obtained by further dividing and folding the first axis into segments (Legendre & Legendre, 2012). To remove the arch effect in CA, a mathematical procedure, detrending, is used to flatten the distribution of the sites along the first CA axis without changing their ordination on that axis. The approach is then designated as Detrended Correspondence Analysis (DCA). GenePiper also provides analytical module for DCA.  For more information about the different detrending algorithms such as using segments or polynomials see ter Braak & Prentice, (1988) and Legendre & Legendre (2012). Some researchers have also argued that the arch effect may not be an artifact but an expected feature of the analysis, especially when species turnover is high along environmental gradients (James & McCulloch, 1990). In that case, if the samples are meaningfully positioned along the arch, the ordination should be accepted as a valid result. "),
          a(target = "_blank", href="https://github.com/raytonghk/genepiper/wiki/14.-Correspondence-Analysis", "see GenePiper Tutorial about CA")
        ),
        tags$div(
          class = "ref",
          h5("Gauch HGJ (1982) Multivariate Analysis and Community Structure. Cambridge University Press, Cambridge, 298pp."),
          h5("Hill MO (1973) Reciprocal averaging—eigenvector method of ordination. Journal of Ecology, 61, 237–244."),
          h5("Hill MO (1974) Correspondence analysis: a neglected multivariate method. Appl Statis 23: 340–354."),
          h5("Hill MO & Gauch HG (1980). Detrended correspondence analysis: an improved ordination technique. Vegetatio 42, 47--58."),
          h5("James FC & McCulloch CE (1990) Multivariate analysis in ecology and systematics: panacea or pandora’s box. Annu Rev Ecolog Syst 21: 129–166."),
          h5("Legendre P & Legendre LFJ. Numerical ecology. Vol. 24. Elsevier, 2012."),
          h5("Oksanen J & Minchin PR (1997). Instability of ordination results under changes in input data order: explanations and remedies. Journal of Vegetation Science 8, 447--454."),
          h5("Ramette A (2007) Multivariate analyses in microbial ecology. FEMS microbiology ecology. 62(2):142-60."),
          h5("Ramette A & Tiedje JM (2007a) Biogeography: an emerging cornerstone for understanding prokaryotic diversity, ecology and evolution. Microb Ecol 53: 197–207."),
          h5("Ramette A & Tiedje JM (2007b) Multiscale responses of microbial life to spatial distance and environmental heterogeneity in a patchy ecosystem. Proc Natl Acad Sci USA 104: 2761–2766."),
          h5("ter Braak CJF (1985) Correspondence analysis of incidence and abundance data: properties in terms of a unimodal response model. Biometrics 41: 859–873."),
          h5("ter Braak CJF & Prentice IC (1988) A theory of gradient analysis. Adv Ecol Res 18: 271–317.")
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
              condition = "output.caReady == true",
              tags$hr(),
              tabsetPanel(
                id = "resultTabset",
                tabPanel(
                  title = "Output",
                  verbatimTextOutput("caOutput"),
                  downloadButton("downloadCAButton", "Download CA")
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















































