library(shiny)
library(shinyjs)
source("../style.R")
source("../ui_additional.R")

shinyUI(
  fluidPage(
    useShinyjs(),
    theme = "../style.css",
    tags$script(type = "text/javascript", src = "../www/busy.js"),
    tags$script(type = "text/javascript", src = "../www/controls.js"),
    tags$div(class = "busy", p("Busy..."), img(src = "../www/hour-glass.gif")),
    tags$script(HTML("window.onload = function() { parent.postMessage('helperDada2PipelineLoaded', '*') }")),
    eval(parse(text = style())),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h5("DADA2 Pipeline"),
        tags$div(
          class = "ref",
          h5("Callahan, B., McMurdie, P., Rosen, M. et al. (2016) DADA2: High-resolution sample inference from Illumina amplicon data. Nat Methods 13: 581–583")
        )
      )
    )
  )
)