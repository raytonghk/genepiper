library(shiny)
library(shinyjs)
library(bsplus)
source("../style.R")
source("../ui_additional.R")
source("../ui_panel_load_data.R")
source("../ui_panel_filter.R")
source("../ui_panel_parameter.R")
source("../ui_panel_graphic.R")

DOT_SIZE <- 1
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
    tags$script(HTML("window.onload = function() { parent.postMessage('analysisAlphaDiversityLoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
      class = "desc",
          p("Alpha diversity (α-diversity) together with beta and gamma diversity were described by R. H. Whittaker as measurements of species diversity in an ecosystem. Alpha diversity measurements are applied to samples from a particular communities that characterize both the species numbers and their relative importances. The species number measures the diversity proper, i.e. the richness of the community in terms of the number of species defined in the sample. In 16S rRNA gene amplicon sequencing, this species number refers to the number of ASVs (amplicon sequence variants) if one used DADA2, Deblur or Unoise pipelines or the number of OTUs (operational taxonomic units) if one used the traditional clustering by sequence similarities methods. The relative importances of species refer to the species productivities or how well they compete with other species in utilizing range of resources in a given community. Productivities of species are often hard to measure and thus this importance value is often "substituted" by the measurement of dominance of species, see Whittaker (1972) for details. Users should note that samples of different sizes (sequencing depth or the number of sequences retrieved) are not directly comparable with their alpha-diversity measurements. 
In this module, the most commonly reported alpha diversity measures are calculated including the Shannon, Simpson, and Fisher diversity indices and species richness estimates Chao1 and ACE using the phyloseq estimate_richness function. Users may choose to compute this statistics at the ASVs/OTUs or phylotypes level, which the ASVs/OTUs are agglomerated at the specified taxonomic rank by the phyloseq tax_glom function. 
"),
          a(target = "_blank", href="https://www.jstor.org/stable/1218190?seq=1#metadata_info_tab_contents", "Whittaker (1972) on Jstor")
        ),
        tags$div(
          class = "ref",
          h5("McMurdie PJ, Holmes S (2013) phyloseq: An R Package for Reproducible Interactive Analysis and Graphics of Microbiome Census Data. PLoS ONE 8(4): e61217. https://doi.org/10.1371/journal.pone.0061217"),
          h5("Whittaker RH (1972) Evolution and Measurement of Species Diversity. Taxon, 21(2/3): 213-251. doi:10.2307/1218190"),
	h5("Callahan BJ, McMurdie PJ, Holmes SP(2017) Exact sequence variants should replace operational taxonomic units in marker-gene data analysis. The ISME journal 11(12):2639-43. doi: 10.1038/ismej.2017.119"),
	h5("Reference manual of the vegan pacakge at https://cran.r-project.org/web/packages/vegan/vegan.pdf")

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
                uiAnalysisButton()
              ),
              
              tags$div(
                class = "graphic-panel",
                conditionalPanel(
                  condition = "input.resultTabset == 'Plot'",
                  h4("Graphic Parameters:"),
                  tabsetPanel(
                    id = "graphicTabset",
                    tabPanel(
                      title = "Plot",
                      selectInput("graphicGroupColumn", "Group Column", c("None")),
                      selectInput("plotIndex", "Plot Index", NULL),
                      conditionalPanel(
                        condition = "input.graphicGroupColumn != 'None'",
                        selectInput("plotType", "Plot Type",
                                    choices = list("Dot Plot" = "dot",
                                                   "Box Plot" = "box")
                        )
                      ),
                      conditionalPanel(
                        condition = "input.plotType != 'box'",
                        numericInput("dotSize", "Dot Size", DOT_SIZE)
                      )
                    ),
                    
                    uiTabPanelTitle(),
                    uiTabPanelXYAxis(X_AXIS_TEXT_SIZE, X_AXIS_TEXT_ANGLE, Y_AXIS_TEXT_SIZE, Y_AXIS_TEXT_ANGLE),
                    uiTabPanelLegend(LEGEND_TEXT_SIZE)
                  )
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.alphaTableReady == true",
              tags$hr(),
              tabsetPanel(
                id = "resultTabset",
                tabPanel(
                  title = "Table",
                  DT::dataTableOutput("alphaTable"),
                  downloadButton("downloadAlphaTableButton")
                ),
                
                tabPanel(
                  title = "Plot",
                  plotOutput("alphaPlot", height = "800px"),
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




























