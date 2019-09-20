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
        class = "desc-box",
        h4("Alpha diversity"),
        tags$div(
          class = "desc",
          p("In ecology, alpha diversity (α-diversity) is the mean species diversity in sites or habitats at a local scale. The term was introduced by R. H. Whittaker together with the terms beta diversity (β-diversity) and gamma diversity (γ-diversity). Whittaker's idea was that the total species diversity in a landscape (gamma diversity) is determined by two different things, the mean species diversity in sites or habitats at a more local scale (alpha diversity) and the differentiation among those habitats (beta diversity)."),
          a(target = "_blank", href="https://en.wikipedia.org/wiki/Alpha_diversity", "From Wikipedia")
        ),
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




























