library(shiny)
library(shinyjs)
source("../style.R")
source("../ui_additional.R")

SUB_MODEL <- c("JC", "F81", "K80", "HKY", "SYM", "GTR")

shinyUI(
  fluidPage(
    useShinyjs(),
    theme = "../style.css",
    tags$script(type = "text/javascript", src = "../www/busy.js"),
    tags$script(type = "text/javascript", src = "../www/controls.js"),
    tags$div(class = "busy", p("Busy..."), img(src = "../www/hour-glass.gif")),
    tags$script(HTML("window.onload = function() { parent.postMessage('helperBuildMlTreeLoaded', '*') }")),
    eval(parse(text = style())),
    
    fluidRow(
      wellPanel(
        class = "desc-box",
        h5("Build ML tree function uses an aligned sequence file to build a ML tree. It bases on the 'modelTest' function in 'phangorn' package. The output file is in Newick format."),
        tags$div(
          class = "ref",
          h5("Burnham, K. P. and Anderson, D. R (2002) Model selection and multimodel inference: a practical information-theoretic approach. 2nd ed. Springer, New York"),
          h5("Posada, D. and Crandall, K.A. (1998) MODELTEST: testing the model of DNA substitution. Bioinformatics 14(9): 817-818"),
          h5("Posada, D. (2008) jModelTest: Phylogenetic Model Averaging. Molecular Biology and Evolution 25: 1253-1256"),
          h5("Darriba D., Taboada G.L., Doallo R and Posada D. (2011) ProtTest 3: fast selection of best-fit models of protein evolution. . Bioinformatics 27: 1164-1165")
        )
      ),
      
      column(
        width = 4,
        wellPanel(
          class = "left-column",
          h4("Sequence File:"),
          selectInput("fileType", "Sequence File Type", list(Fasta = "fasta",
                                                             Phylip = "phylip",
                                                             Nexus = "nexus")),
          fileInput("filepath", "Sequence File Path"),
          errorOutput("filepathMessage")
        ),
        
        wellPanel(
          class = "left-column",
          h4("Build Trees:"),
          checkboxGroupInput("subModel", "Substitution Model", SUB_MODEL, SUB_MODEL),
          checkboxInput("doGammaModel", "Do Gamma Model Test?", TRUE),
          checkboxInput("doInvarSites", "Do Invariant Sites Test?", TRUE),
          errorOutput("buildTreesMessage"),
          actionButton("buildTreesButton", "Build Trees")
        )
      ),
      
      column(
        width = 8,
        wellPanel(
          class = "middle-column",
          h4("Tree Models:"),
          DT::dataTableOutput("treeModelsTable")
        ),
        
        wellPanel(
          class = "middle-column",
          h4("Tree:"),
          verbatimTextOutput("treeDesc"),
          downloadButton("downloadTreeButton"),
          plotOutput("treePlot", height = "auto")
        )
      )
    )
  )
)
