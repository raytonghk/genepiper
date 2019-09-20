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

X_AXIS_TEXT_SIZE <- 10
X_AXIS_TEXT_ANGLE <- 0
Y_AXIS_TEXT_SIZE <- 10
Y_AXIS_TEXT_ANGLE <- 0
DOT_SIZE <- 3
LINE_SIZE <- 1

shinyUI(
  fluidPage(
    useShinyjs(),
    theme = "../style.css",
    tags$script(type = "text/javascript", src = "../www/busy.js"),
    tags$script(type = "text/javascript", src = "../www/controls.js"),
    tags$div(class = "busy", p("Busy..."), img(src = "../www/hour-glass.gif")),
    tags$script(HTML("window.onload = function() { parent.postMessage('analysisProcrustesLoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h4("Procrustes Analysis"),
        tags$div(
          class = "desc",
          h5("In statistics, Procrustes analysis is a form of statistical shape analysis used to analyse the distribution of a set of shapes. The name Procrustes (Greek: Προκρούστης) refers to a bandit from Greek mythology who made his victims fit his bed either by stretching their limbs or cutting them off."),
          a(target = "_blank", href = "https://en.wikipedia.org/wiki/Procrustes_analysis", "From Wikipedia")
        ),
        tags$div(
          class = "ref",
          h5("Mardia, K.V., Kent, J.T. and Bibby, J.M. (1979). Multivariate Analysis. Academic Press."),
          h5("Peres-Neto, P.R. and Jackson, D.A. (2001). How well do multivariate data sets match? The advantages of a Procrustean superimposition approach over the Mantel test. Oecologia 129: 169-178.")
        )
      ),
      
      column(
        width = 4,
        wellPanel(
          class = "left-column",
          h4("Target Matrix:"),
          selectInput("targetFileType", "File Type", list("Comma-separated file" = "csv",
                                                          "Tab-separated file" = "tsv",
                                                          "RDS file" = "rds")),
          fileInput("targetFilepath", "File Path"),
          errorOutput("targetFilepathMessage")
        ),
        
        wellPanel(
          class = "left-column",
          h4("Matrix To Be Rotated:"),
          selectInput("rotatedFileType", "File Type", list("Comma-separated file" = "csv",
                                                           "Tab-separated file" = "tsv",
                                                           "RDS file" = "rds")),
          fileInput("rotatedFilepath", "File Path"),
          errorOutput("rotatedFilepathMessage")
        )
      ),
      
      column(
        width = 8,
        conditionalPanel(
          condition = "output.dataReady",
          wellPanel(
            class = "middle-column",
            tags$div(
              tags$div(
                class = "parameter-panel",
                h4("Parameters:"),
                errorOutput("procMessage"),
                conditionalPanel(
                  condition = "output.dataValidated",
                  checkboxInput("scale", "Scale XY axes to fit (uncheck for scale Y axis only)?", TRUE),
                  checkboxInput("symmetric", "Use symmetric Procrustes statistic?", FALSE),
                  tags$div(
                    tags$div(
                      class = "column-left",
                      h5("Target Matrix:"),
                      checkboxGroupInput("targetTableAxes", "Select Axes:", NULL),
                      errorOutput("targetTableAxesMessage")
                    ),
                    tags$div(
                      class = "column-right",
                      h5("Matrix To Be Rotated:"),
                      checkboxGroupInput("rotatedTableAxes", "Select Axes:", NULL),
                      errorOutput("rotatedTableAxesMessage")
                    )
                  ),
                  uiAnalysisButton()
                )
              ),
              
              tags$div(
                class = "graphic-panel",
                conditionalPanel(
                  condition = "input.resultTabset == 'Plot'",
                  h4("Graphic Parameters:"),
                  tabsetPanel(
                    id = "graphicTabset",
                    uiTabPanelTitle(),
                    uiTabPanelXYAxis(X_AXIS_TEXT_SIZE, X_AXIS_TEXT_ANGLE, Y_AXIS_TEXT_SIZE, Y_AXIS_TEXT_ANGLE),
                    uiTabPanelDot(DOT_SIZE),
                    uiTabPanelLine(LINE_SIZE)
                  )
                )
              )
            ),
            
            conditionalPanel(
              condition = "output.procReady",
              tags$hr(),
              tabsetPanel(
                id = "resultTabset",
                tabPanel(
                  title = "Output",
                  verbatimTextOutput("procOutput"),
                  downloadButton("downloadProcButton")
                ),
                
                tabPanel(
                  title = "Protest",
                  verbatimTextOutput("protestOutput"),
                  errorOutput("protestMessage"),
                  downloadButton("downloadProtestButton")
                ),
                
                tabPanel(
                  title = "Mantel",
                  selectInput("corrMethod", "Correlation Method", c("Pearson", "Spearman", "Kendall")) %>%
                    shinyInput_label_embed(
                      shiny_iconlink() %>%
                        bs_embed_popover(title = "Mantel Test",
                                         placement = "left",
                                         html = "true",
                                         content = tags$p("For choosing the corrlation method, please refer to", tags$a(target = "_blank", href = "https://www.rdocumentation.org/packages/vegan/versions/2.4-2/topics/mantel", "vegan::mantel"), "."))
                    ),
                  verbatimTextOutput("mantelOutput"),
                  errorOutput("mantelMessage"),
                  downloadButton("downloadMantelButton")
                ),
                
                tabPanel(
                  title = "Plot",
                  plotOutput("procPlot", height = "800px"),
                  errorOutput("plotMessage"),
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