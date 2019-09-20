downloadImageDialog <- function(id = NULL, height = 6, width = 6) {
  modalDialog(
    h4("Download Image"),
    textInput(paste0("imageFileName", id), "Save File Name", "image"),
    tags$div(
      tags$div(
        style = "display:inline-block; width: 45%",
        numericInput(paste0("imageHeight", id), "Height (in inch)", height)
      ),
      tags$div(
        style = "display:inline-block; vertical-align: top; width: 45%",
        numericInput(paste0("imageWidth", id), "Width (in inch)", width)
      )
    ),
    footer = tagList(
      modalButton("Cancel"),
      downloadButton(paste0("imageDownloadButton", id))
    )
  )
}

downloadImageDialogWithoutDim <- function(id = NULL) {
  modalDialog(
    h4("Download Image"),
    textInput(paste0("imageFileName", id), "Save File Name", "image"),
    footer = tagList(
      modalButton("Cancel"),
      downloadButton(paste0("imageDownloadButton", id))
    )
  )
}