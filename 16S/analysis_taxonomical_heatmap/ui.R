library(shiny)
library(shinyjs)
library(bsplus)
source("../style.R")
source("../ui_additional.R")
source("../ui_panel_load_data.R")
source("../ui_panel_filter.R")
source("../ui_panel_parameter.R")
source("../ui_panel_graphic.R")

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
    tags$script(HTML("window.onload = function() { parent.postMessage('analysisTaxonomicalHeatmapLoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h4("Taxonomical Heatmap"),
        tags$div(
          class = "ref",
          h5("McMurdie PJ, Holmes S (2013) phyloseq: An R Package for Reproducible Interactive Analysis and Graphics of Microbiome Census Data. PLoS ONE 8(4): e61217. https://doi.org/10.1371/journal.pone.0061217")
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
                
                tags$div(
                  tags$div(
                    class = "column-left",
                    style = "width: 35%",
                    selectInput("sampleOrderBy", "Sample Order By", width = "90%",
                                choices = c("Name", "Group", "Hierarchical Clustering")
                    )
                  ),
                  tags$div(
                    class = "column-right",
                    style = "width: 55%",
                    conditionalPanel(
                      condition = "input.sampleOrderBy == \'Group\'",
                      selectInput("sampleGroupColumn", "Group Column", NULL)
                    ),
                    conditionalPanel(
                      condition = "input.sampleOrderBy == \'Hierarchical Clustering\'",
                      tags$div(
                        class = "column-left",
                        style = "width: 45%",
                        selectInput("sampleClusterMethod", "Clustering Method", width = "90%",
                                    choices = c("complete", "average", "single", "ward.D", "ward.D2", "mcquitty", "median","centroid"))
                      ),
                      tags$div(
                        class = "column-right",
                        style = "width: 45%",
                        selectInput("sampleDistanceMethod", "Distance Method", width = "90%",
                                    choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
                        )
                      )
                    )
                  )
                ),
                
                tags$div(
                  tags$div(
                    class = "column-left",
                    style = "width: 35%",
                    selectInput("taxaOrderBy", "Taxa Order By", width = "90%",
                                choices = c("Name", "Hierarchical Clustering")
                    )
                  ),
                  tags$div(
                    class = "column-right",
                    style = "width: 55%",
                    conditionalPanel(
                      condition = "input.taxaOrderBy == \'Hierarchical Clustering\'",
                      tags$div(
                        class = "column-left",
                        style = "width: 45%",
                        selectInput("taxaClusterMethod", "Clustering Method", width = "90%",
                                    choices = c("complete", "average", "single", "ward.D", "ward.D2", "mcquitty", "median","centroid")
                        )
                      ),
                      tags$div(
                        class = "column-right",
                        style = "width: 45%",
                        selectInput("taxaDistanceMethod", "Distance Method", width = "90%",
                                    choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
                        )
                      )
                    )
                  )
                ),
                
                tags$div(
                  tags$div(
                    class = "column-left",
                    style = "width: auto; margin-right: 2%",
                    h5("Display:", style = "font-weight: bold;")
                  ),
                  tags$div(
                    class = "column-right",
                    style = "width: 30%",
                    selectInput("displayWhich", NULL, list("Top Abundance" = "top", "Least Abundance" = "least"))
                  ),
                  tags$div(
                    class = "column-right",
                    style = "width: 30%",
                    numericInput("displayNumber", NULL, 20)
                  )
                ),
                
                tags$div(
                  tags$div(
                    class = "column-left",
                    style = "width: auto; margin-right: 2%",
                    h5("Rescale Taxa?", style = "font-weight: bold")
                  ),
                  tags$div(
                    class = "column-right",
                    style = "width: 40%",
                    selectInput("rescaleMethod", NULL, c("None", "Normalization", "Standardization"))
                  )
                ),
                
                uiPlotButton()
              ),
              
              tags$div(
                class = "graphic-panel",
                h4("Graphic Parameters:"),
                tabsetPanel(
                  id = "graphicTabset",
                  uiTabPanelTitle(),
                  uiTabPanelXYAxis(X_AXIS_TEXT_SIZE, X_AXIS_TEXT_ANGLE, Y_AXIS_TEXT_SIZE, Y_AXIS_TEXT_ANGLE),
                  uiTabPanelLegend(LEGEND_TEXT_SIZE),

                  tabPanel(
                    title = "Color",
                    selectInput("colorScale", "Color Scale", c("Default", "Heat Color", "Green Red", "Viridis", "Green", "Red"))
                  )
                )
              )
            ),
            conditionalPanel(
              condition = "output.plotTableReady == true",
              tags$hr(),
              plotOutput("heatmap", height = "800px"),
              actionButton("downloadDialogButton", "Download")
            )
          )
        )
      )
    )
  )
)












































