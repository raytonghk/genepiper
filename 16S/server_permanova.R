observe({
  req(vals$modifiedPhyloseq)
  updateSelectInput(session, "permanovaGroupColumn", choices = characterVariableNames(vals$filteredPhyloseq))
})

observe({
  req(vals$modifiedPhyloseq)
  choices <- list(
    "Bray-Curtis" = "bray",
    "Gower" = "gower",
    "Jaccard" = "jaccard",
    "Kulczynski" = "kulczynski",
    "Horn-Morisita" = "horn",
    "Binomial" = "binomial",
    "Cao" = "cao",
    "Chao" = "chao"
  )
  if(!is.null(vals$filteredPhyloseq@phy_tree)) {
    choices <- c(
      list(
        "UniFrac" = "unifrac",
        "Weighted UniFrac" = "wunifrac"
      ),
      choices
    )
  }
  updateSelectInput(session, "permanovaDistanceMethod", choices = choices)
})

observe({
  vals$permanovaMessage <- NULL
  vals$adonis <- NULL
  tryCatch(
    {
      req(vals$modifiedPhyloseq, input$permanovaGroupColumn, input$permanovaDistanceMethod)
      distanceMatrix <- distanceForAdonis(vals$modifiedPhyloseq, input$permanovaDistanceMethod)
      group <- get_variable(vals$modifiedPhyloseq, input$permanovaGroupColumn)
      vals$adonis <- vegan::adonis(distanceMatrix ~ group)
    },
    error = function(e) {
      vals$permanovaMessage <- "Error: Permanova error."
    }
  )
})

distanceForAdonis <- function(phyloseq, distanceMethod) {
  if(distanceMethod %in% c("unifrac", "wunifrac")) {
    distance(phyloseq, distanceMethod)
  } else {
    otuDataFrame(phyloseq) %>%
      t() %>%
      vegan::vegdist(distanceMethod)
  }
}

output$permanovaMessage <- renderText(HTML(vals$permanovaMessage))

output$permanovaOutput <- renderPrint(vals$adonis)








































