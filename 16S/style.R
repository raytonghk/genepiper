style <- function(containsShinyOptionsGroupStyle = TRUE, shinyOptionsGroupColumnNumber = 2) {
  styleText <- c()
  if(containsShinyOptionsGroupStyle) {
    styleText <- c(styleText, shinyOptionsGroupCssText(shinyOptionsGroupColumnNumber))
  }
}

shinyOptionsGroupCssText <- function(shinyOptionsGroupColumnNumber) {
  paste0('tags$style("div.shiny-options-group {max-height: 200px; width: auto; overflow-y: scroll;',
         '-webkit-column-count: ', shinyOptionsGroupColumnNumber, ';',
         '-moz-column-count: ', shinyOptionsGroupColumnNumber, ';',
         'column-count: ', shinyOptionsGroupColumnNumber, ';',
         '-webkit-column-fill: balance; -moz-column-fill: balance; column-fill: balance; margin-top: 0px;}")')
  # 'tags$style("div.shiny-options-group {max-height: 200px; column-fill: balance}")'
}