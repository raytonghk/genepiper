### Graphic Group Column
observe({
  req(vals$filteredPhyloseq)
  updateSelectInput(session, "graphicGroupColumn", choices = c("None", characterVariableNames(vals$filteredPhyloseq)))
})

observe({
  req(vals$filteredPhyloseq)
  updateSelectInput(session, "graphicGroupColumn3d", choices = c("None", characterVariableNames(vals$filteredPhyloseq)))
})

# protect crash from plot too much taxa label
observe({
  req(vals$modifiedPhyloseq, input$plotTaxa == TRUE)

  if(ntaxa(vals$modifiedPhyloseq) > 1000) {
    disable("labelTaxa")
  } else {
    enable("labelTaxa")
  }
})

observe({
  req(vals$modifiedPhyloseq, input$plotTaxa3d == TRUE)
  
  if(ntaxa(vals$modifiedPhyloseq) > 1000) {
    disable("labelTaxa3d")
  } else {
    enable("labelTaxa3d")
  }
})
