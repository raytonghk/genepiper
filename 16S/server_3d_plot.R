plot3dTaxa <- function(pl, taxaTable, axis) {
  if(input$labelTaxa3d) {
    pl %>%
      plotly::add_text(x = formula(paste0("~", axis[1])), y = formula(paste0("~", axis[2])), z = formula(paste0("~", axis[3])), 
                       text = rownames(taxaTable), font.size = 1, data = taxaTable)
  } else {
    pl %>%
      plotly::add_markers(x = formula(paste0("~", axis[1])), y = formula(paste0("~", axis[2])), z = formula(paste0("~", axis[3])), size = 1, data = taxaTable)
  }
}

plot3dSampleWithoutGroup <- function(pl, sampleTable, axis) {
  if(input$labelSample3d) {
    pl %>%
      plotly::add_text(x = formula(paste0("~", axis[1])), y = formula(paste0("~", axis[2])), z = formula(paste0("~", axis[3])),
                       text = rownames(sampleTable), data = sampleTable)
  } else {
    pl %>%
      plotly::add_markers(x = formula(paste0("~", axis[1])), y = formula(paste0("~", axis[2])), z = formula(paste0("~", axis[3])), data = sampleTable)
  }
}

plot3dSampleWithGroup <- function(pl, sampleTable, axis, phyloseq, groupColumn) {
  if(input$labelSample3d) {
    pl %>%
      plotly::add_text(x = formula(paste0("~", axis[1])), y = formula(paste0("~", axis[2])), z = formula(paste0("~", axis[3])),
                       text = rownames(sampleTable), color = get_variable(phyloseq, groupColumn), data = sampleTable)
  } else {
    pl %>%
      plotly::add_markers(x = formula(paste0("~", axis[1])), y = formula(paste0("~", axis[2])), z = formula(paste0("~", axis[3])),
                          color = get_variable(phyloseq, groupColumn), data = sampleTable)
  }
}