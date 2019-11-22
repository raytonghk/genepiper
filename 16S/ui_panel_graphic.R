uiTabPanelDot <- function(DOT_SIZE, id = NULL, step = 1) {
  tabPanel(
    title = "Dot",
    numericInput(paste0("dotSize", id), "Dot Size", DOT_SIZE, step = step)
  )
}

uiTabPanelEnvfit <- function(ENVFIT_VECTOR_LINE_SIZE, ENVFIT_VECTOR_LABEL_SIZE, ENVFIT_FACTOR_DOT_SIZE, 
                             ENVFIT_FACTOR_LABEL_SIZE) {
  tabPanel(
    title = "Envfit",
    tags$br(),
    tags$p("Fit sample data to the ordination by",
           tags$a(target = "_blank", href = "https://www.rdocumentation.org/packages/vegan/versions/2.4-2/topics/envfit", "vegan::envfit"),
           "."),
    tags$div(
      class = "fixed-height-block",
      verbatimTextOutput("envfitOutput")
    ),
    tags$br(),
    errorOutput("envfitMessage"),
    conditionalPanel(
      condition = "output.envfitReady == true",
      downloadButton("downloadEnvfit", "Download Envfit"),
      checkboxInput("plotEnvfit", "Plot Envfit?", FALSE),
      conditionalPanel(
        condition = "input.plotEnvfit == true",
        tabsetPanel(
          tabPanel(
            title = "Vector",
            checkboxGroupInput("envfitVectorLabel", "Vectors:", NULL),
            numericInput("envfitVectorLineSize", "Line Width", value = ENVFIT_VECTOR_LINE_SIZE, width = "45%"),
            numericInput("envfitVectorLabelSize", "Label Size", value = ENVFIT_VECTOR_LABEL_SIZE, width = "45%")
          ),
          tabPanel(
            title = "Factor",
            tags$div(id = "envfitFactor"),
            numericInput("envfitFactorDotSize", "Dot Size", value = ENVFIT_FACTOR_DOT_SIZE, width = "45%"),
            numericInput("envfitFactorLabelSize", "Label Size", value = ENVFIT_FACTOR_LABEL_SIZE, width = "45%")
          )
        )
      )
    )
  )
}

uiTabPanelGroup <- function(..., choices = c("None"), id = NULL) {
  tabPanel(
    title = "Group",
    selectInput(paste0("graphicGroupColumn", id), "Group Column", choices), ...
  )
}

uiTabPanelGroup3d <- function() {
  uiTabPanelGroup(choices = NULL, id = "3d")
}

uiTabPanelGroupOrdination <- function(SPIDER_LINE_SIZE, SPIDER_LABEL_SIZE, ELLIPSE_LINE_SIZE, ELLIPSE_SIGNIF, id = NULL) {
  tabPanel(
    title = "Group",
    selectInput(paste0("graphicGroupColumn", id), "Group Column", NULL),
    conditionalPanel(
      condition = paste0("input.graphicGroupColumn", id, " != 'None'"),
      checkboxInput(paste0("plotConvexHull", id), "Plot Convex Hull?", FALSE),
      uiConditionalPlotSpider(SPIDER_LINE_SIZE, SPIDER_LABEL_SIZE, id),
      uiConditionalPlotEllipse(ELLIPSE_LINE_SIZE, ELLIPSE_SIGNIF, id)
    )
  )
}

uiConditionalPlotSpider <- function(SPIDER_LINE_SIZE, SPIDER_LABEL_SIZE, id = NULL) {
  tags$div(
    checkboxInput(paste0("plotSpider", id), "Plot Spider?", FALSE),
    conditionalPanel(
      condition = paste0("input.plotSpider", id, " == true"),
      numericInput(paste0("spiderLineSize", id), "Spider Line Width", SPIDER_LINE_SIZE),
      numericInput(paste0("spiderLabelSize", id), "Spider Label Size", SPIDER_LABEL_SIZE)
    )
  )
}

uiConditionalPlotEllipse <- function(ELLIPSE_LINE_SIZE, ELLIPSE_SIGNIF, id = NULL) {
  tags$div(
    checkboxInput(paste0("plotEllipse", id), "Plot Ellipse?", FALSE),
    conditionalPanel(
      condition = paste0("input.plotEllipse", id, " == true"),
      numericInput(paste0("ellipseLineSize", id), "Ellipse Line Width", ELLIPSE_LINE_SIZE),
      selectInput(paste0("ellipseType", id), "Ellipse Plot Type", list("Standard Deviation" = "sd", "Standard Error" = "se")),
      numericInput(paste0("ellipseSignif", id), "Significant Level", ELLIPSE_SIGNIF, 0, 1)
    )
  )
}

uiTabPanelLabel <- function(LABEL_SIZE, id = NULL) {
  tabPanel(
    title = "Label",
    numericInput(paste0("labelSize", id), "Label Size", LABEL_SIZE)
  )
}

uiTabPanelLegend <- function(LEGEND_TEXT_SIZE, display = TRUE, id = NULL) {
  tabPanel(
    title = "Legend",
    numericInput(paste0("legendSize", id), "Legend Font Size", LEGEND_TEXT_SIZE),
    checkboxInput(paste0("displayLegend", id), "Display Legend?", display)
  )
}

uiTabPanelLine <- function(LINE_WIDTH, id = NULL) {
  tabPanel(
    title = "Line",
    numericInput(paste0("lineSize", id), "Line Width", LINE_WIDTH)
  )
}

uiTabPanelPlotAxis <- function(id = NULL) {
  tabPanel(
    title = "Plot Axis",
    checkboxGroupInput(paste0("plotAxis", id), "Plot Axis", NULL),
    errorOutput(paste0("plotAxis", id, "Message"))
  )
}

uiTabPanelPlotAxis2d <- function() {
  uiTabPanelPlotAxis("2d")
}

uiTabPanelPlotAxis3d <- function() {
  uiTabPanelPlotAxis("3d")
}

uiTabPanelPlotSample3dLabel <- function() {
  tabPanel(
    title = "Sample",
    checkboxInput("plotSample3d", "Plot Samples?", TRUE),
    conditionalPanel(
      condition = "input.plotSample3d == true",
      checkboxInput("labelSample3d", "Label Samples?", FALSE)
    )
  )
}

uiTabPanelPlotSampleDotLabel <- function(SAMPLE_DOT_SIZE, SAMPLE_LABEL_SIZE, id = NULL) {
  tabPanel(
    title = "Sample",
    uiConditionalPlotSampleDotSize(SAMPLE_DOT_SIZE,
                                   id = id,
                                   uiConditionalLabelSampleSize(SAMPLE_LABEL_SIZE, id = id))
  )
}

uiConditionalPlotSampleDotSize <- function(SAMPLE_DOT_SIZE, ..., id = NULL, value = TRUE) {
  tags$div(
    checkboxInput(paste0("plotSample", id), "Plot Samples?", value),
    conditionalPanel(
      condition = paste0("input.plotSample", id, " == true"),
      numericInput(paste0("sampleDotSize", id), "Dot Size", SAMPLE_DOT_SIZE),
      ...
    )
  )
}

uiConditionalLabelSampleSize <- function(SAMPLE_LABEL_SIZE, ..., id = NULL, value = FALSE) {
  tags$div(
    checkboxInput(paste0("labelSample", id), "Label Samples?", value),
    conditionalPanel(
      condition = paste0("input.labelSample", id, " == true"),
      numericInput(paste0("sampleLabelSize", id), "Label Size", SAMPLE_LABEL_SIZE),
      ...
    )
  )
}

uiTabPanelPlotTaxa3dLabel <- function() {
  tabPanel(
    title = "Taxa",
    checkboxInput("plotTaxa3d", "Plot Taxa?", FALSE),
    conditionalPanel(
      condition = "input.plotTaxa3d == true",
      checkboxInput("labelTaxa3d", "Label Taxa?", FALSE)
    )
  )
}

uiTabPanelPlotTaxaDotLabel <- function(TAXA_DOT_SIZE, TAXA_LABEL_SIZE, id = NULL) {
  tabPanel(
    title = "Taxa",
    uiConditionalPlotTaxaDotSize(TAXA_DOT_SIZE,
                                 id = id,
                                 uiConditionalLabelTaxaSize(TAXA_LABEL_SIZE, id = id))
  )
}

uiConditionalPlotTaxaDotSize <- function(TAXA_DOT_SIZE, ..., id = NULL, value = FALSE) {
  tags$div(
    checkboxInput(paste0("plotTaxa", id), "Plot Taxa?", value),
    conditionalPanel(
      condition = paste0("input.plotTaxa", id, " == true"),
      numericInput(paste0("taxaDotSize", id), "Dot Size", TAXA_DOT_SIZE),
      ...
    )
  )
}

uiConditionalLabelTaxaSize <- function(TAXA_LABEL_SIZE, ..., id = NULL, value = FALSE) {
  tags$div(
    checkboxInput(paste0("labelTaxa", id), "Label Taxa?", value),
    conditionalPanel(
      condition = paste0("input.labelTaxa", id, " == true"),
      numericInput(paste0("taxaLabelSize", id), "Label Size", TAXA_LABEL_SIZE),
      ...
    )
  )
}


uiTabPanelPlotTaxaLIneLabel <- function(TAXA_LINE_SIZE, TAXA_LABEL_SIZE) {
  tabPanel(
    title = "Loading",
    checkboxInput("plotTaxa", "Plot Loading?", FALSE),
    conditionalPanel(
      condition = "input.plotTaxa == true",
      numericInput("taxaLineSize", "Line Width", TAXA_LINE_SIZE),
      checkboxInput("labelTaxa", "Label Loading?", FALSE),
      conditionalPanel(
        condition = "input.labelTaxa == true",
        numericInput("taxaLabelSize", "Label Size", TAXA_LABEL_SIZE)
      )
    )
  )
}

uiTabPanelSize <- function(DOT_SIZE, LINE_SIZE, LABEL_SIZE, id = NULL) {
  tabPanel(
    title = "Size",
    numericInput(paste0("dotSize", id), "Dot Size", DOT_SIZE),
    numericInput(paste0("lineSize", id), "Line Width", LINE_SIZE),
    numericInput(paste0("labelSize", id), "Label Size", LABEL_SIZE)
  )
}

uiTabPanelTitle <- function(id = NULL) {
  tabPanel(
    title = "Title",
    textInput(paste0("title", id), "Title")
  )
}

uiTabPanelXYAxis <- function(X_AXIS_TEXT_SIZE, X_AXIS_TEXT_ANGLE, Y_AXIS_TEXT_SIZE, Y_AXIS_TEXT_ANGLE, id = NULL) {
  tabPanel(
    title = "Axis",
    numericInput(paste0("xAxisTextSize", id), "X-Axis Label Size", X_AXIS_TEXT_SIZE),
    numericInput(paste0("xAxisTextAngle", id), "X-Axis Label Angle", X_AXIS_TEXT_ANGLE),
    numericInput(paste0("yAxisTextSize", id), "Y-Axis Label Size", Y_AXIS_TEXT_SIZE),
    numericInput(paste0("yAxisTextAngle", id), "Y-Axis Label Angle", Y_AXIS_TEXT_ANGLE)
  )
}









