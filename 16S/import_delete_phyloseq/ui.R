library(shiny)
library(shinyjs)
library(bsplus)
source("../style.R")
source("../ui_additional.R")
source("../ui_panel_load_data.R")

shinyUI(
  fluidPage(
    useShinyjs(),
    theme = "../style.css",
    tags$script(type = "text/javascript", src = "../www/busy.js"),
    tags$script(type = "text/javascript", src = "../www/controls.js"),
    tags$div(class = "busy", p("Busy..."), img(src = "../www/hour-glass.gif")),
    tags$script(HTML("window.onload = function() { parent.postMessage('importDeletePhyloseqLoaded', '*') }")),
    eval(parse(text = style())),
    use_bs_popover(),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h5("Delete phyloseq is a helper function to delete the stored phyloseq dataset in GenePiper.")
      ),
      
      column(
        width = 4,
        uiPanelLoadData()
      ),
      
      column(
        width = 4,
        wellPanel(
          class = "middle-column",
          h4("Delete:"),
          actionButton("deleteDialogButton", "Delete"),
          errorOutput("deleteMessage")
        )
      )
    )
  )
)
