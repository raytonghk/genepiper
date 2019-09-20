### Taxonomic Rank
serverTaxRank <- function(id = NULL, choices = c("OTU", rev(rank_names(vals$filteredPhyloseq)))) {
  observe({
    req(vals$filteredPhyloseq)
    updateSelectInput(session, paste0("taxRank", id), choices = choices)
  })
}


### Group Column Without None
# observe({
#   req(vals$filteredPhyloseq)
#   updateSelectInput(session, "groupColumnWithoutNone", choices = characterVariableNames(vals$filteredPhyloseq))
# })

### Distance Method
serverDistanceMethod <- function(id = NULL, choices = list("Bray-Curtis" = "bray", "Gower" = "gower", "Jaccard" = "jaccard", 
                                                           "Kulczynski" = "kulczynski", "Horn-Morisita" = "horn", 
                                                           "Binomial" = "binomial", "Cao" = "cao", "Chao" = "chao"), 
                                 choicesWithTree = list("UniFrac" = "unifrac", "Weighted UniFrac" = "wunifrac")) {
  observe({
    req(vals$filteredPhyloseq)
    choices <- choices
    if(!is.null(vals$filteredPhyloseq@phy_tree)) {
      choices <- c(choicesWithTree, choices)
    }
    updateSelectInput(session, paste0("distanceMethod", id), choices = choices)
  })
}

distanceMatrix <- function(phyloseq, distanceMethod) {
  if(distanceMethod %in% c("unifrac", "wunifrac")) {
    phyloseq::distance(phyloseq, distanceMethod)
  } else {
    otuDataFrame(phyloseq) %>%
      t() %>%
      vegan::vegdist(distanceMethod)
  }
}

### GroupColumnWithLabel
serverGroupColumnWithLabel <- function(id = NULL, minSelect = 2, maxSelect = NULL) {
  observe({
    req(vals$filteredPhyloseq)
    updateSelectInput(session, paste0("groupColumn", id), choices = characterVariableNames(vals$filteredPhyloseq))
  })

  observe({
    req(vals$filteredPhyloseq, input[[paste0("groupColumn", id)]])
    choices <- levels(groupFactor(vals$filteredPhyloseq, input[[paste0("groupColumn", id)]]))
    updateCheckboxGroupInput(session, paste0("groupLabels", id), choices = choices, selected = choices)
  })

  observe({
    output[[paste0("groupLabelMessage", id)]] <- renderText(NULL)
    req(input[[paste0("groupLabels", id)]])
    if(length(input[[paste0("groupLabels", id)]]) < minSelect) {
      output[[paste0("groupLabelMessage", id)]] <- renderText(HTML(paste0("Please select at least ", minSelect, " labels.")))
    }
    if((!is.null(maxSelect)) && length(input[[paste0("groupLabels", id)]]) > maxSelect) {
      output[[paste0("groupLabelMessage", id)]] <- renderText(HTML(paste0("Please select not more than ", maxSelect, " labels.")))
    }
  })
}



