PROJECT_ROOT_PATH <- "/srv/shiny-server/data/"

projectNames <- function() {
  unique(c("Default", unlist(getProjects())))
}

getProjects <- function() {
  list.dirs(getProjectPath(), FALSE)
}

getProjectPath <- function(project = NULL) {
  paste0(PROJECT_ROOT_PATH, project)
}

otuDetails <- function(df) {
  req(df)
  cat("OTU table summary:\n")
  cat(paste0("Number of OTUs: ", nrow(df), "\n"))
  cat(paste0("Number of samples: ", ncol(df) - 1, "\n"))
}

formatOtuTable <- function(df) {
  df %>%
    as.data.frame() %>%
    column_to_rownames(colnames(.)[1]) %>%
    `rownames<-`(make.names(rownames(.))) %>%
    `colnames<-`(make.names(colnames(.)))
}

validateOtuTable <- function(df) {
  df <- as.data.frame(df)
  ncol(df) > 3 && is.character(df[, 1]) && is.numeric(as.matrix(df[, -1])) && all(!is.na(as.matrix(df[, -1])))
}

taxDetails <- function(df) {
  req(df)
  cat("Taxonomy table summary:\n")
  cat(paste0("Number of taxa: ", nrow(df), "\n"))
  cat(paste0("Number of taxonomic ranks:", ncol(df) - 1, "\n"))
}

formatTaxTable <- function(df) {
  colnames(df)[1] <- "OTU_ID"
  df %>%
    group_by(OTU_ID) %>%
    gather(rank, value, -OTU_ID) %>%
    filter(!is.na(value)) %>%
    summarise(tax = list(value)) %>%
    rowwise() %>%
    mutate(tax = list(formatTaxa(tax))) %>%
    mutate(tax = paste(unlist(tax), collapse = ",")) %>%
    separate(tax, c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"), sep = ",") %>%
    mutate(OTU_ID = make.names(OTU_ID)) %>%
    as.data.frame() %>%
    column_to_rownames("OTU_ID")
}

formatTaxa <- function(x) {
  taxa <- unlist(x)
  if(length(taxa) < 6) {
    len <- length(taxa)
    taxa <- c(taxa, rep(taxa[len], 6 - len))
  }
  if(length(taxa) == 6) {
    taxa <- c(taxa, paste(taxa[6], "sp."))
  } else {
    taxa[7] <- paste(taxa[6], taxa[7])
  }
  taxa
}

validateTaxTable <- function(df) {
  df <- as.data.frame(df)
  ncol(df) %in% 2:8 && is.character(as.matrix(df))
}

sampleDetails <- function(df) {
  req(df)
  cat("Sample data summary:\n")
  cat(paste0("Number of samples: ", nrow(df), "\n"))
  cat(paste0("Number of features: ", ncol(df), "\n"))
}

formatSampleData <- function(df) {
  df %>%
    as.data.frame() %>%
    column_to_rownames(colnames(.)[1]) %>% 
    `rownames<-`(make.names(rownames(.))) %>%
    `colnames<-`(make.names(colnames(.)))
}

validateSamData <- function(df) {
  df <- as.data.frame(df)
  ncol(df) >= 2 && is.character(df[, 1])
}

formatTree <- function(tree) {
  tree$tip.label <- make.names(tree$tip.label)
  tree
}


phyloseqWithTree <- function(otuTable, taxTable, sampleData, tree) {
  phyloseq(otu_table(t(as.matrix(otuTable)), taxa_are_rows = TRUE), 
           tax_table(as.matrix(taxTable)),
           sam_data(sampleData),
           phy_tree(tree))
}

phyloseqWithoutTree <- function(otuTable, taxTable, sampleData) {
  phyloseq(otu_table(t(as.matrix(otuTable)), taxa_are_rows = TRUE), 
           tax_table(as.matrix(taxTable)),
           sam_data(sampleData))
}

formatPhyloseq <- function(phyloseq) {
  otuTable <- formatPhyloseqOtuTable(phyloseq)
  taxTable <- formatPhyloseqTaxTable(phyloseq)
  samData <- formatPhyloseqSamData(phyloseq)
  if(is.null(phyloseq@phy_tree)) {
    phyloseqWithoutTree(t(otuTable), taxTable, samData)
  } else {
    tree <- formatTree(phy_tree(phyloseq))
    phyloseqWithTree(t(otuTable), taxTable, samData, tree)
  }
}

formatPhyloseqOtuTable <- function(phyloseq) {
  if(phyloseq@otu_table@taxa_are_rows) {
    otuTab <- otu_table(phyloseq)
  } else {
    otuTab <- t(otu_table(phyloseq))
  }
  
 otuTab %>%
    as.data.frame() %>%
    `rownames<-`(make.names(rownames(.))) %>%
    `colnames<-`(make.names(colnames(.)))
}

formatPhyloseqTaxTable <- function(phyloseq) {
  tax_table(phyloseq) %>%
    as("matrix") %>%
    as.data.frame() %>%
    rownames_to_column("OTU_ID") %>%
    formatTaxTable()
}

formatPhyloseqSamData <- function(phyloseq) {
  sample_data(phyloseq) %>%
    as.data.frame() %>%
    `rownames<-`(make.names(rownames(.))) %>%
    `colnames<-`(make.names(colnames(.)))
}


























