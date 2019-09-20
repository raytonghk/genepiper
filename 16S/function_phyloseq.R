addRankToTable <- function(tble, phyloseq, rank) {
  table %>%
    left_join(otuRankTable(phyloseq, rank))
}

addGroupToTable <- function(table, phyloseq, groupColumn) {
  table %>%
    left_join(sampleGroupTable(phyloseq, groupColumn))
}

agglomerateTaxa <- function(phyloseq, taxRank) {
  if(taxRank != "OTU") {
    phyloseq <- tax_glom(phyloseq, taxRank)
  }
  phyloseq
}

characterVariableNames <- function(phyloseq) {
  get_variable(phyloseq) %>%
    select_if(is.character) %>%
    colnames()
}

convertDataFrameColumnToFactor <- function(df, columns) {
  for(i in columns) {
    df[[i]] <- as.factor(df[[i]])
  }
  df
}

formatTableWithTaxRank <- function(tbl, phyloseq, taxRank) {
  if(taxRank == "OTU") {
    tbl <- tbl %>%
      mutate(Species = otuNamedTaxa(phyloseq, "Species")[OTU]) %>%
      select(OTU, Species, everything())
  }
  tbl
}

getGroupFactorColor <- function(phyloseq, groupColumn, order = TRUE) {
  groupFactor <- groupFactor(phyloseq, groupColumn, order)
  scales::hue_pal()(nlevels(groupFactor))[groupFactor]
}

getGroupFactorLevelColor <- function(phyloseq, groupColumn) {
  groupFactor <- groupFactor(phyloseq, groupColumn)
  scales::hue_pal()(nlevels(groupFactor))
}

groupFactor <- function(phyloseq, groupColumn, order = TRUE) {
  variable <- get_variable(phyloseq, groupColumn)
  if(order) {
    variable <- variable[order(variable)]
  }
  factor(variable)
}

logicalVariableNames <- function(phyloseq) {
  get_variable(phyloseq) %>%
    select_if(is.logical) %>%
    colnames()
}

numericVariableNames <- function(phyloseq) {
  get_variable(phyloseq) %>%
    select_if(is.numeric) %>%
    colnames()
}

otuDataFrame <- function(phyloseq) {
  data.frame(get_taxa(phyloseq))
}

otuDataFrameWithTaxaRowname <- function(phyloseq, rank) {
  otuTable <- otuDataFrame(phyloseq)
  if(rank != "OTU") {
    `rownames<-`(otuTable, otuNamedTaxa(phyloseq, rank)[rownames(otuTable)])
  } else {
    otuTable
  }
}

otuNamedTaxa <- function(phyloseq, rank) {
  tax_table(phyloseq) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    {`names<-`(.[[rank]], rownames(.))}
}

otuPlotTable <- function(phyloseq) {
  otuDataFrame(phyloseq) %>%
    rownames_to_column("OTU") %>%
    gather("Sample", "Value", -OTU)
}

otuRankTable <- function(phyloseq, rank) {
  tax_table(phyloseq) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    rownames_to_column("OTU") %>%
    select(one_of(c("OTU", rank)))
}

rankPlotTable <- function(phyloseq, rank) {
  otuPlotTable <- otuPlotTable(phyloseq)
  if(rank != "OTU") {
    otuPlotTable <- otuPlotTable %>%
      left_join(otuRankTable(phyloseq, rank)) %>%
      select(-OTU)
  }
  otuPlotTable 
}

transformCount <- function(phyloseq, abundanceType) {
  switch(abundanceType,
         "Raw Count" = phyloseq,
         "Rarefied Count" = rarefy_even_depth(phyloseq),
         "Relative Abundance" = transform_sample_counts(phyloseq, function(x) {
           x / sum(x)
         })
  )
}

sampleNamedVariable <- function(phyloseq, variableName) {
  `names<-`(get_variable(phyloseq, variableName), sample_names(phyloseq))
}

sampleGroupTable <- function(phyloseq, groupColumn) {
  get_variable(phyloseq) %>%
    rownames_to_column("Sample") %>%
    select(one_of(c("Sample", groupColumn)))
} 

subsetTaxaPhyloseq <- function(phyloseq, rank, filterLabel) {
  paste0("subset_taxa(phyloseq, ", rank, " %in% c(", paste(shQuote(filterLabel), collapse = ","), "))") %>%
    parse(text = .) %>%
    eval()
}

taxaNames <- function(phyloseq, rank) {
  if(rank == "OTU") {
    taxa_names(phyloseq)
  } else {
    get_taxa_unique(phyloseq, rank)
  }
}

variableNames <- function(phyloseq) {
  colnames(get_variable(phyloseq))
}

variableType <- function(phyloseq, column) {
  try(
    {
      class(get_variable(phyloseq, column))
    }, silent = TRUE
  )
}

