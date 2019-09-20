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

DOT_SIZE <- 1
LINE_SIZE <- 1
LABEL_SIZE <- 3

shinyUI(
  fluidPage(
    useShinyjs(),
    theme = "../style.css",
    tags$script(type = "text/javascript", src = "../www/busy.js"),
    tags$script(type = "text/javascript", src = "../www/controls.js"),
    tags$div(class = "busy", p("Busy..."), img(src = "../www/hour-glass.gif")),
    tags$script(HTML("window.onload = function() { parent.postMessage('analysisCIALoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h4("Co-Inertia Analysis"),
        tags$div(
          class = "ref",
          h5("Dolédec, S. and Chessel, D. (1994) Co-inertia analysis: an alternative method for studying species-environment relationships. Freshwater Biology, 31, 277–294."),
          h5("Dray, S., Chessel, D. and J. Thioulouse (2003) Co-inertia analysis and the linking of the ecological data tables. Ecology, 84, 11, 3078–3089."),
          h5("Heo, M. & Gabriel, K.R. (1997) A permutation test of association between configurations by means of the RV coefficient. Communications in Statistics - Simulation and Computation, 27, 843-856.")
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
                  
                  tabPanel(
                    title = "Table Y",
                    selectInput("rankFilterY", "Filter Rank", NULL, width = "90%"),
                    conditionalPanel(
                      condition = "input.rankFilterY != output.taxRank",
                      checkboxGroupInput("rankFilterLabelY", "Filter Rank Labels", NULL)
                    ),
                    checkboxGroupInput("tableYTaxaLabels", "Select Labels", NULL),
                    selectInput("ordinateMethodY", "Ordination Method", width = "90%",
                                choices = list(
                                  "Principal Component Analysis" = "pca",
                                  "Correspondence Analysis" = "coa",
                                  "Principal Coordinates Analysis" = "pco",
                                  "Non Symmetric Correspondence Analysis" = "nsc"
                                )
                    ),
                    conditionalPanel(
                      condition = "input.ordinateMethodY == 'pca'",
                      checkboxInput("scaleY", "Scale Table Y?", FALSE)
                    ),
                    conditionalPanel(
                      condition = "input.ordinateMethodY == 'pco'",
                      selectInput("distanceMethodY", "Distance Method", width = "90%",
                                  choices = list("Bray-Curtis" = "bray",
                                                 "Gower" = "gower",
                                                 "Jaccard" = "jaccard",
                                                 "Kulczynski" = "kulczynski",
                                                 "Horn-Morisita" = "horn",
                                                 "Binomial" = "binomial",
                                                 "Cao" = "cao",
                                                 "Chao" = "chao")
                      )
                    ),
                    actionButton("analysisButtonY", "Analysis"),
                    errorOutput("tableYMessage")
                  ),
                  
                  tabPanel(
                    title = "Table X",
                    checkboxGroupInput("tableXMetaLabels", "Select Labels:", NULL),
                    selectInput("ordinateMethodX", "Ordination Method", NULL, width = "90%"),
                    conditionalPanel(
                      condition = "input.ordinateMethodX == 'pca'",
                      checkboxInput("scaleX", "Scale Table X?", FALSE)
                    ),
                    conditionalPanel(
                      condition = "input.ordinateMethodX == 'pco'",
                      selectInput("distanceMethodX", "Distance Method", width = "90%",
                                  choices = list("Bray-Curtis" = "bray",
                                                 "Gower" = "gower",
                                                 "Jaccard" = "jaccard",
                                                 "Kulczynski" = "kulczynski",
                                                 "Horn-Morisita" = "horn",
                                                 "Binomial" = "binomial",
                                                 "Cao" = "cao",
                                                 "Chao" = "chao")
                      )
                    ),
                    actionButton("analysisButtonX", "Analysis"),
                    errorOutput("tableXMessage")
                  ),
                  
                  tabPanel(
                    title = "Co-Inertia",
                    tags$br(),
                    actionButton("ciaButton", "Analysis")
                  )
                )
              ),
              
              tags$div(
                class = "graphic-panel",
                conditionalPanel(
                  condition = "input.ciaTabset == 'Plot'",
                  h4("Graphic Parameters:"),
                  tabsetPanel(
                    id = "graphicTabset",
                    tabPanel(
                      title = "Type",
                      selectInput("plotType", "Plot Type", 
                                  choices = list(
                                    "Full Plot" = "full",
                                    "Coinertia Plot and Canonical Weight Only" = "ciaWeight",
                                    "Coinertia Plot Only" = "cia",
                                    "Axes Plot Only" = "axes",
                                    "Canonical Weights Plot Only" = "weight",
                                    "Eigenvalues Plot Only" = "eigen"
                                  )
                      )
                    ),
                    uiTabPanelGroup(NULL, checkboxInput("plotLegend", "Plot Legend?", FALSE)),
                    uiTabPanelSize(DOT_SIZE, LINE_SIZE, LABEL_SIZE)
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
                  title = "Table Y",
                  tabsetPanel(
                    id = "tableYTabset",
                    tabPanel(
                      title = "Table",
                      DT::dataTableOutput("tableY")
                    ),
                    
                    tabPanel(
                      title = "Output",
                      verbatimTextOutput("tableYOutput"),
                      downloadButton("downloadTableYDudi", "Download")
                    )
                  )
                ),
                
                tabPanel(
                  title = "Table X",
                  tabsetPanel(
                    id = "tableXTabset",
                    tabPanel(
                      title = "Table",
                      DT::dataTableOutput("tableX")
                    ),
                    
                    tabPanel(
                      title = "Output",
                      verbatimTextOutput("tableXOutput"),
                      downloadButton("downloadTableXDudi", "Download")
                    )
                  )
                ),
                
                tabPanel(
                  title = "Co-Inertia",
                  tabsetPanel(
                    id = "ciaTabset",
                    tabPanel(
                      title = "Output",
                      verbatimTextOutput("ciaOutput"),
                      downloadButton("downloadCIAButton", "Download")
                    ),
                    
                    tabPanel(
                      title = "Plot",
                      tags$div(
                        style = "display: flex; height: 800px; width: auto; background: #FFF; overflow: auto; justify-content: center;",
                        plotOutput("ciaPlot", height = "800px", width = "800px")
                      ),
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



































