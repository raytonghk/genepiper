### Graphic Group Column
observe({
  req(vals$filteredPhyloseq)
  updateSelectInput(session, "graphicGroupColumn", choices = c("None", characterVariableNames(vals$filteredPhyloseq)))
})

observe({
  req(vals$filteredPhyloseq)
  updateSelectInput(session, "graphicGroupColumn3d", choices = c("None", characterVariableNames(vals$filteredPhyloseq)))
})