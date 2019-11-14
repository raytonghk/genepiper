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

shinyUI(
  fluidPage(
    useShinyjs(),
    theme = "../style.css",
    tags$script(type = "text/javascript", src = "../www/busy.js"),
    tags$script(type = "text/javascript", src = "../www/controls.js"),
    tags$div(class = "busy", p("Busy..."), img(src = "../www/hour-glass.gif")),
    tags$script(HTML("window.onload = function() { parent.postMessage('analysisNetworkLoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h4("Network Analysis"),
        tags$div(
          class = "desc",
          p("Weighted correlation network analysis, also known as weighted gene co-expression network analysis (WGCNA), is a widely used data mining method especially for studying biological networks based on pairwise correlations between variables. While it can be applied to most high-dimensional data sets, it has been most widely used in genomic applications. It allows one to define modules (clusters), intramodular hubs, and network nodes with regard to module membership, to study the relationships between co-expression modules, and to compare the network topology of different networks (differential network analysis). WGCNA can be used as a data reduction technique (related to oblique factor analysis), as a clustering method (fuzzy clustering), as a feature selection method (e.g. as gene screening method), as a framework for integrating complementary (genomic) data (based on weighted correlations between quantitative variables), and as a data exploratory technique. Although WGCNA incorporates traditional data exploratory techniques, its intuitive network language and analysis framework transcend any standard analysis technique. Since it uses network methodology and is well suited for integrating complementary genomic data sets, it can be interpreted as systems biologic or systems genetic data analysis method. By selecting intramodular hubs in consensus modules, WGCNA also gives rise to network based meta analysis techniques."),
          a(target = "_blank", href = "https://en.wikipedia.org/wiki/Weighted_correlation_network_analysis", "From Wikipedia")
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
                selectInput("corrTarget", "Correlation Target", c("Taxa", "Samples"), width = "90%"),
                conditionalPanel(
                  condition = "input.corrTarget == 'Taxa'",
                  selectInput("groupColumn", "Group Column (At least 5 samples for each group)", NULL, width = "90%")
                ),
                selectInput("corrMethod", "Correlation Method", width = "90%",
                            choices = c("Pearson", "Spearman", "SparCC", "SpiecEasi")),
                conditionalPanel(
                  condition = "input.corrMethod == 'SparCC'",
                  numericInput("bootstrapNum", "Number Of Bootstraps:", 10, width = "90%")
                ),
                conditionalPanel(
                  condition = "input.corrMethod == 'SpiecEasi'",
                  selectInput("seMethod", "SpiecEasi Method", c("glasso", "mb"), width = "90%")
                ),
                tags$div(
                  tags$div(
                    class = "column-left",
                    numericInput("filterDataNumber", "Data Number Filter", 3, 3, width = "80%")
                  ),
                  
                  tags$div(
                    class = "column-right",
                    conditionalPanel(
                      condition = "input.corrMethod != 'SpiecEasi'",
                      numericInput("filterCorr", "Correlation Filter", 0.8, 0, 1, width = "80%")
                    )
                  ),
                  
                  tags$div(
                    class = "column-left",
                    conditionalPanel(
                      condition = "input.corrMethod != 'SpiecEasi'",
                      numericInput("filterPVal", "P Value Filter", 0.05, 0, 1, width = "80%")
                    )
                  )
                ),
                uiAnalysisButton()
              ),
              
              tags$div(
                class = "graphic-panel",
                conditionalPanel(
                  condition = "output.igListReady",
                  h4("Graphic Parameters:"),
                  tabsetPanel(
                    tabPanel(
                      title = "Plot",
                      selectInput("plotSelect", "Select Plot", NULL)
                    ),
                    
                    tabPanel(
                      title = "Label",
                      conditionalPanel(
                        condition = "output.nameTypeDisplay",
                        selectInput("nameType", "Name Type", c("ID", "Taxa Name"))
                      ),
                      numericInput("labelSize", "Label Size:", 20)
                    ),
                    
                    tabPanel(
                      title = "Vertex",
                      selectInput("nodeColor", "Nodes Color Column", NULL),
                      selectInput("nodeSize", "Nodes Size Column", NULL),
                      numericInput("nodeSizeScale", "Nodes Size Scale Factor", 1)
                    ),
                    
                    tabPanel(
                      title = "Edge",
                      selectInput("edgeColor", "Edges Color", c("None", "Pos/Neg")),
                      selectInput("edgeSize", "Edges Width", c("None", "Correlation")),
                      numericInput("edgeSizeScale", "Edges Size Scale Factor", 1)
                    )
                  )
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.igListReady",
              tags$hr(),
              visNetwork::visNetworkOutput("plotNetwork", height = "800px"),
              tags$div(
                class = "column-left",
                tags$div(
                  class = "column-left",
                  downloadButton("downloadIgButton", "Download IGraph")
                ),
                tags$div(
                  class = "column-right",
                  actionButton("downloadDialogButton", "Download Image")
                )
              )
            )
          )
        )
      )
    )
  )
)
