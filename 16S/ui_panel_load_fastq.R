uiPanelLoadPairedFastq <- function() {
  tags$div(
    wellPanel(
      class = "left-column",
      tags$div(
        tags$div(
          class = "pull-right",
          shiny_iconlink() %>%
            bs_embed_popover(title = "Load Fastq Files",
                             placement = "left",
                             html = "true",
                             content = "Load fastq files from shared folder.")
        ),
        h4("Load Forward Fastq Files"),
        textInput("forwardFilterString", "Filter Pattern", ".fastq"),
        checkboxGroupInput("forwardFastq", "Files:"),
        actionButton("forwardSelectAllButton", "Select All"),
        actionButton("forwardCleanAllButton", "Clean All"),
        tags$div(
          class = "pull-right",
          actionButton("forwardConfirmButton", "Confirm")
        )
      )
    ),
    
    wellPanel(
      class = "left-column",
      tags$div(
        tags$div(
          class = "pull-right",
          shiny_iconlink() %>%
            bs_embed_popover(title = "Load Fastq Files",
                             placement = "left",
                             html = "true",
                             content = "Load fastq files from shared folder.")
        ),
        h4("Load Reverse Fastq Files"),
        textInput("reverseFilterString", "Filter Pattern", ".fastq"),
        checkboxGroupInput("reverseFastq", "Files:"),
        errorOutput("reverseFastqMessage"),
        actionButton("reverseSelectAllButton", "Select All"),
        actionButton("reverseCleanAllButton", "Clean All"),
        tags$div(
          class = "pull-right",
          actionButton("reverseConfirmButton", "Confirm")
        )
      )
    )
  )
}