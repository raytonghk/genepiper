getIgList <- function(corrList, method) {
  map(corrList, ~{
    if(method == "SpiecEasi") {
      req(vals$dfList)
      ig <- SpiecEasi::adj2igraph(SpiecEasi::getRefit(.x))
      igraph::vertex_attr(ig)$name <- colnames(vals$dfList[[1]])[as.numeric(igraph::vertex_attr(ig)$name)]
      igraph::edge_attr(ig)$r <- igraph::edge_attr(ig)$weight
      igraph::delete_vertices(ig, igraph::degree(ig) == 0)
    } else {
      igraph::graph_from_data_frame(.x, directed = FALSE)
    }
  })
} 

getCorrList <- function(dfList, method) {
  if(method %in% c("Pearson", "Spearman")) {
    getRCorrList(dfList, method)
  } else if(method == "SparCC") {
    getSpCorrList(dfList)
  } else if(method == "SpiecEasi") {
    getSECorrList(dfList)
  }
}

getSECorrList <- function(dfList) {
  req(input$seMethod)
  map(dfList, ~{
    library(SpiecEasi)
    .x %>%
      as.matrix() %>%
      spiec.easi(method = input$seMethod)
  })
}

getSpCorrList <- function(dfList) {
  req(input$bootstrapNum)
  map(dfList, ~{
    sp <- .x %>%
      select_if(any_vars(sum(. > 0) >= input$filterDataNumber)) %>%
      as.matrix() %>%
      SpiecEasi::sparccboot(R = input$bootstrapNum) %>%
      SpiecEasi::pval.sparccboot()
    
    rMat <- pMat <- matrix(nrow = ncol(.x), ncol = ncol(.x), dimnames = list(colnames(.x), colnames(.x)))
    rMat[upper.tri(rMat)] <- sp$cors
    pMat[upper.tri(pMat)] <- sp$pvals
    map2(list(rMat, pMat), c("r", "P"), ~{
      .x %>%
        as.data.frame() %>%
        rownames_to_column("Var1") %>%
        gather("Var2", !!as.name(.y), -Var1)
    }) %>%
      reduce(left_join) %>%
      filter(abs(r) >= input$filterCorr) %>%
      filter(P <= input$filterPVal) %>%
      select(-P) %>%
      filterRepeatPair()
  })
}

getRCorrList <- function(dfList, method) {
  map(dfList, ~{
    .x %>%
      select_if(any_vars(sum(. > 0) >= input$filterDataNumber)) %>%
      as.matrix() %>%
      Hmisc::rcorr(type = tolower(method)) %>%
      .[c("r", "P")] %>%
      map2(c("r", "P"), ~{
        .x %>%
          as.data.frame() %>%
          rownames_to_column("Var1") %>%
          gather("Var2", !!as.name(.y), -Var1)
      }) %>%
      reduce(left_join) %>%
      filter(abs(r) >= input$filterCorr) %>%
      filter(P <= input$filterPVal) %>%
      select(-P) %>%
      filterRepeatPair()
  })
}

filterRepeatPair <- function(df) {
  df %>%
    filter(Var1 != Var2) %>%
    rowwise() %>%
    mutate(comb = paste(c(Var1, Var2)[order(c(Var1, Var2))], collapse = "_")) %>%
    distinct(comb, .keep_all = TRUE) %>%
    select(-comb) %>%
    ungroup()
}

getDFList <- function(phyloseq, target) {
  if(target == "Samples") {
    getSampleDFList(phyloseq)
  } else {
    req(input$groupColumn)
    getGroupDFList(phyloseq, input$groupColumn)
  }
}

getSampleDFList <- function(phyloseq) {
  get_taxa(phyloseq) %>%
    as.data.frame() %>%
    list(Sample = .)
}

getGroupDFList <- function(phyloseq, group) {
  df <- get_taxa(phyloseq) %>%
    t() %>%
    as.data.frame()
  if(group == "None") {
    list(Taxa = df)
  } else {
    df %>%
      mutate(group = get_variable(phyloseq, group)) %>%
      group_by(group) %>%
      nest() %>%
      {`names<-`(.$data, .$group)}
  }
}

igEdgeSize <- function(ig) {
  req(input$edgeSize, input$edgeSizeScale)
  if(input$edgeSize != "None") {
    vars <- abs(igraph::edge_attr(ig)$r)
    vars <- (vars - min(vars))/max(vars)
    igraph::edge_attr(ig)$width <- ((vars + 0.1) * input$edgeSizeScale)
  }
  ig
}

igEdgeColor <- function(ig) {
  req(input$edgeColor)
  if(input$edgeColor == "None") {
    vars <- rep("black", length(igraph::edge_attr(ig)$r))
  } else {
    vars <- c("red", "green")[(igraph::edge_attr(ig)$r > 0) + 1]
  }
  igraph::edge_attr(ig)$color <- vars
  ig
}

igNodeSize <- function(ig) {
  req(input$nodeSize, vals$dfList, input$plotSelect, input$nodeSizeScale)
  if(input$nodeSize != "None") {
    if(input$nodeSize == "Abundance") {
      vars <- colSums(vals$dfList[[input$plotSelect]][, igraph::vertex_attr(ig)$name])
    } else {
      vars <- sampleNamedVariable(vals$filteredPhyloseq, input$nodeSize)[igraph::vertex_attr(ig)$name]
    }
    igraph::vertex_attr(ig)[[input$nodeColor]] <- vars
    igraph::vertex_attr(ig)$size <- (log(vars + 1) * input$nodeSizeScale)
  }
  ig
}

igNodeColor <- function(ig) {
  req(input$nodeColor, vals$corrTarget, vals$filteredPhyloseq)
  if(input$nodeColor != "None") {
    if(vals$corrTarget == "Samples") {
      vars <- sampleNamedVariable(vals$filteredPhyloseq, input$nodeColor)[igraph::vertex_attr(ig)$name]
    } else {
      vars <- otuNamedTaxa(vals$filteredPhyloseq, input$nodeColor)[igraph::vertex_attr(ig)$name]
    }
    igraph::vertex_attr(ig)[[input$nodeColor]] <- vars
    igraph::vertex_attr(ig)$color <- scales::hue_pal()(nlevels(factor(vars)))[as.numeric(factor(vars))]
  }
  ig
}

igNameType <- function(ig) {
  req(vals$corrTarget, vals$filteredPhyloseq, vals$taxRank)
  if(vals$corrTarget == "Taxa" && vals$taxRank != "OTU" && input$nameType != "ID") {
    igraph::vertex_attr(ig)$name <- otuNamedTaxa(vals$filteredPhyloseq, vals$taxRank)[igraph::vertex_attr(ig)$name]
  }
  ig
}