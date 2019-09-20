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

OBJECT_DOT_SIZE <- 1
OBJECT_LABEL_SIZE <- 5
VARIABLE_LINE_SIZE <- 1
VARIABLE_LABEL_SIZE <- 5
X_AXIS_TEXT_SIZE <- 10
X_AXIS_TEXT_ANGLE <- 0
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
    tags$script(HTML("window.onload = function() { parent.postMessage('analysisCCorALoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h4("Canonical Correlation Analysis"),
        tags$div(
          class = "desc",
          p("In statistics, canonical-correlation analysis (CCA) is a way of inferring information from cross-covariance matrices. If we have two vectors X = (X1, ..., Xn) and Y = (Y1, ..., Ym) of random variables, and there are correlations among the variables, then canonical-correlation analysis will find linear combinations of the Xi and Yj which have maximum correlation with each other.[1] T. R. Knapp notes that 'virtually all of the commonly encountered parametric tests of significance can be treated as special cases of canonical-correlation analysis, which is the general procedure for investigating the relationships between two sets of variables.' The method was first introduced by Harold Hotelling in 1936."),
          a(target = "_blank", href = "https://en.wikipedia.org/wiki/Canonical_correlation", "From Wikipedia")
        ),
        tags$div(
          class = "ref",
          h5("Hotelling, H. 1936. Relations between two sets of variates. Biometrika 28: 321-377."),
          h5("Legendre, P. 2005. Species associations: the Kendall coefficient of concordance revisited. Journal of Agricultural, Biological, and Environmental Statistics 10: 226-245.")
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
                    selectInput("tableYType", "Table Y Contains", width = "90%", 
                                choices = list("Taxa Only" = "taxa", "Metadata Only" = "meta", "Taxa + Metadata" = "both")
                    ),
                    conditionalPanel(
                      condition = "input.tableYType != 'meta'",
                      h5("Taxa:", style = "font-weight: bold;"),
                      selectInput("rankFilterY", "Filter Rank", NULL, width = "90%"),
                      conditionalPanel(
                        condition = "input.rankFilterY != output.taxRank",
                        checkboxGroupInput("rankFilterLabelY", "Filter Rank Labels", NULL)
                      ),
                      checkboxGroupInput("tableYTaxaLabels", "Select Labels", NULL)
                    ),
                    conditionalPanel(
                      condition = "input.tableYType != 'taxa'",
                      h5("Metadata:", style = "font-weight: bold;"),
                      checkboxGroupInput("tableYMetaLabels", "Select Labels:", NULL)
                    ),
                    errorOutput("tableYMessage")
                  ),
                  
                  tabPanel(
                    title = "Table X",
                    selectInput("tableXType", "Table X Contains", width = "90%", 
                                choices = list("Taxa Only" = "taxa", "Metadata Only" = "meta", "Taxa + Metadata" = "both")
                    ),
                    conditionalPanel(
                      condition = "input.tableXType != 'meta'",
                      h5("Taxa:", style = "font-weight: bold;"),
                      selectInput("rankFilterX", "Filter Rank", NULL, width = "90%"),
                      conditionalPanel(
                        condition = "input.rankFilterX != output.taxRank",
                        checkboxGroupInput("rankFilterLabelX", "Filter Rank Labels", NULL)
                      ),
                      checkboxGroupInput("tableXTaxaLabels", "Select Labels", NULL)
                    ),
                    conditionalPanel(
                      condition = "input.tableXType != 'taxa'",
                      h5("Metadata:", style = "font-weight: bold;"),
                      checkboxGroupInput("tableXMetaLabels", "Select Labels:", NULL)
                    ),
                    errorOutput("tableXMessage")
                  ),
                  
                  tabPanel(
                    title = "CCorA",
                    tags$div(
                      tags$div(
                        class = "column-left",
                        checkboxInput("standardiseTableY", "Standardise Table Y?", FALSE)
                      ),
                      tags$div(
                        class = "column-right",
                        checkboxInput("standardiseTableX", "Standardise Table X?", FALSE)
                      )
                    ),
                    actionButton("ccoraButton", "Analysis"),
                    errorOutput("ccoraMessage")
                  )
                )
              ),
              
              tags$div(
                class = "graphic-panel",
                conditionalPanel(
                  condition = "input.ccoraTabset == 'Plot'",
                  h4("Graphic Parameters:"),
                  tabsetPanel(
                    id = "graphicTabset",
                    tabPanel(
                      title = "Type",
                      selectInput("plotType", "Plot Type", c("Layout", "Objects", "Variables", "Biplots", "Variables Overlay")
                      )
                    ),
                    uiTabPanelPlotAxis(),
                    
                    tabPanel(
                      title = "Object",
                      numericInput("objectDotSize", "Dot Size", OBJECT_DOT_SIZE),
                      checkboxInput("labelObject", "Label Object?", FALSE),
                      conditionalPanel(
                        condition = "input.labelObject == true",
                        numericInput("objectLabelSize", "Label Size", OBJECT_LABEL_SIZE)
                      )
                    ),
                    
                    tabPanel(
                      title = "Variable",
                      numericInput("variableLineSize", "Line Width", VARIABLE_LINE_SIZE),
                      numericInput("variableLabelSize", "Label Size", VARIABLE_LABEL_SIZE)
                    ),
                    uiTabPanelXYAxis(X_AXIS_TEXT_SIZE, X_AXIS_TEXT_ANGLE, Y_AXIS_TEXT_SIZE, Y_AXIS_TEXT_ANGLE)
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
                  DT::dataTableOutput("tableY")
                ),
                
                tabPanel(
                  title = "Table X",
                  DT::dataTableOutput("tableX")
                ),
                
                tabPanel(
                  title = "CCorA",
                  tabsetPanel(
                    id = "ccoraTabset",
                    tabPanel(
                      title = "Output",
                      verbatimTextOutput("ccoraOutput"),
                      downloadButton("downloadCCorAButton", "Download CCorA")
                    ),
                    
                    tabPanel(
                      title = "Objects",
                      tabsetPanel(
                        tabPanel(
                          title = "Table Y",
                          DT::dataTableOutput("tableYObjects")
                        ),
                        
                        tabPanel(
                          title = "Table X",
                          DT::dataTableOutput("tableXObjects")
                        )
                      )
                    ),
                    
                    tabPanel(
                      title = "Variables",
                      tabsetPanel(
                        tabPanel(
                          title = "Table Y",
                          tabsetPanel(
                            tabPanel(
                              title = "Y",
                              DT::dataTableOutput("variablesTableYY")
                            ),
                            
                            tabPanel(
                              title = "X",
                              DT::dataTableOutput("variablesTableYX")
                            )
                          )
                        ),
                        
                        tabPanel(
                          title = "Table X",
                          tabsetPanel(
                            tabPanel(
                              title = "Y",
                              DT::dataTableOutput("variablesTableXY")
                            ),
                            
                            tabPanel(
                              title = "X",
                              DT::dataTableOutput("variablesTableXX")
                            )
                          )
                        )
                      )
                    ),
                    
                    tabPanel(
                      title = "Plot",
                      plotOutput("ccoraPlot", height = "800px"),
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






































