factorPlotTableRankBySum <- function(plotTable, rank) {
  plotTable %>%
    group_by(!!as.name(rank)) %>%
    mutate(Sum = sum(Value)) %>%
    ungroup() %>%
    arrange(Sum) %>%
    mutate(!!as.name(rank) := factor(!!as.name(rank), levels = unique(!!as.name(rank))))
}

factorPlotTableSampleByGroup <- function(plotTable, groupColumn) {
  plotTable %>%
    arrange(!!as.name(groupColumn)) %>%
    mutate(Sample = factor(Sample, levels = unique(Sample)))
}

formatSquareGgAspectRatio <- function(gg) {
  xlim <- ggplot_build(gg)$layout$panel_params[[1]]$x.range
  ylim <- ggplot_build(gg)$layout$panel_params[[1]]$y.range
  gg +
    coord_fixed(diff(xlim) / diff(ylim), xlim = xlim, ylim = ylim)
}

formatSquareGg <- function(gg, format = TRUE, legend = TRUE, title = TRUE) {
  xlim <- ggplot_build(gg)$layout$panel_params[[1]]$x.range
  ylim <- ggplot_build(gg)$layout$panel_params[[1]]$y.range
  maxDiff <- max(diff(xlim), diff(ylim))
  if(diff(xlim) > diff(ylim)) {
    ylim <- c(mean(ylim) - maxDiff / 2, mean(ylim) + maxDiff / 2)
  } else {
    xlim <- c(mean(xlim) - maxDiff / 2, mean(xlim) + maxDiff / 2)
  }
  if(format) {
    gg <- formatGg(gg, legend = legend, title = title)
  }
  gg +
    coord_fixed(1, xlim, ylim)
}

formatGg <- function(gg, legend = TRUE, title = TRUE) {
  if(legend) {
    try(
      {
        if(input$displayLegend == FALSE) {
          gg <- gg +
            guides(color = FALSE, fill = FALSE)
        }
      }, silent = TRUE
    )
  }
  
  if(title) {
    if(input$title != "") {
      gg <- gg +
        ggtitle(input$title)
    }
  }
  
  gg +
    theme(axis.text.x = element_text(size = input$xAxisTextSize,
                                     angle = input$xAxisTextAngle,
                                     hjust = ifelse(input$xAxisTextAngle != 0, 1, 0.5)),
          axis.title.x = element_text(size = input$xAxisTextSize + 1),
          axis.text.y = element_text(size = input$yAxisTextSize,
                                     angle = input$yAxisTextAngle),
          axis.title.y = element_text(size = input$yAxisTextSize + 1),
          legend.text = element_text(size = input$legendSize),
          legend.title = element_text(size = input$legendSize + 1))
}

plotArrowLine <- function(gg, table, axis, label, lineSize, labelSize, color = "black", labelColor = "black") {
  gg <- gg +
    geom_segment(aes_string(x = 0, y = 0, xend = axis[1], yend = axis[2]), color = color, arrow = arrow(), size = lineSize, data = table)
  if(label) {
    textTable <- nudgeCoordinateReferToOrigin(table, axis)
    gg <- gg+
      geom_text(aes(x = !!as.name(axis[1]), y = !!as.name(axis[2]), label = rownames(textTable)), color = labelColor, size = labelSize, data = textTable)
  }
  gg
}

plotArrowLineTaxa <- function(gg, taxaTable, axis, labelTaxa, lineSize, labelSize, color = "blue", labelColor = "blue") {
  plotArrowLine(gg, taxaTable, axis, labelTaxa, lineSize, labelSize, color, labelColor)
}

plotDotTaxa <- function(gg, taxaTable, axis, label, dotSize, labelSize) {
  gg <- gg +
    geom_point(aes_string(x = axis[1], y = axis[2]), shape = 3, color = "red", size = dotSize, data = taxaTable)
  if(label) {
    gg <- labelDotTaxa(gg, taxaTable, axis, labelSize)
  }
  gg
}

labelDotTaxa <- function(gg, taxaTable, axis, labelSize) {
  taxaTable <- rownames_to_column(taxaTable, "Taxa")
  gg +
    ggrepel::geom_text_repel(aes_string(x = axis[1], y = axis[2], label = "Taxa"), color = "red", size = labelSize, data = taxaTable)
}

plotDotSampleWithoutGroup <- function(gg, sampleTable, axis, label, dotSize, labelSize, color = "black", labelColor = "black") {
  gg <- gg +
    geom_point(aes_string(x = axis[1], y = axis[2]), size = dotSize, color = color, data = sampleTable)
  if(label) {
    gg <- labelDotSampleWithoutGroup(gg, sampleTable, axis, labelSize, color = labelColor)
  }
  gg
}

labelDotSampleWithoutGroup <- function(gg, sampleTable, axis, labelSize, color = "black") {
  sampleTable <- rownames_to_column(sampleTable, "Sample")
  gg +
    ggrepel::geom_text_repel(aes_string(x = axis[1], y = axis[2], label = "Sample"), size = labelSize, color = color, data = sampleTable)
}

sampleTableWithGroup <- function(sampleTable, phyloseq, groupColumn) {
  rownames_to_column(sampleTable, "Sample") %>%
    addGroupToTable(phyloseq, groupColumn)
}

plotDotSampleWithGroup <- function(gg, sampleTable, axis, groupColumn, label, dotSize, labelSize) {
  gg <- gg +
    geom_point(aes_string(x = axis[1], y = axis[2], color = groupColumn), size = dotSize, data = sampleTable)
  if(label) {
    gg <- labelDotSampleWithGroup(gg, sampleTable, axis, groupColumn, labelSize)
  }
  gg
}

labelDotSampleWithGroup <- function(gg, sampleTable, axis, groupColumn, labelSize) {
  gg +
    ggrepel::geom_text_repel(aes_string(x = axis[1], y = axis[2], label = "Sample", color = groupColumn), size = labelSize, data = sampleTable)
}

plotConvexHull <- function(gg, sampleTable, axis, groupColumn) {
  chullTable <- sampleTable %>%
    group_by(!!as.name(groupColumn)) %>%
    summarise(chull = list(data.frame(!!as.name(axis[1]), !!as.name(axis[2])) %>%
    {.[chull(.),]})) %>%
    unnest(chull)
  gg +
    geom_polygon(aes_string(x = axis[1], y = axis[2], group = groupColumn, fill = groupColumn), alpha = 0.5, data = chullTable)
}

plotSpider <- function(gg, sampleTable, axis, groupColumn, lineSize, labelSize) {
  spiderTable <- sampleTable %>%
    group_by(!!as.name(groupColumn)) %>%
    mutate(meanX := mean(!!as.name(axis[1])), meanY := mean(!!as.name(axis[2]))) %>%
    ungroup()
  gg +
    geom_segment(aes_string(x = "meanX", y = "meanY", xend = axis[1], yend = axis[2], color = groupColumn), size = lineSize, data = spiderTable) +
    geom_text(aes_string(x = "meanX", y = "meanY", label = groupColumn), color = "black", size = labelSize, data = spiderTable)
}

plotEllipse <- function(gg, sampleTable, axis, groupColumn, type, signif, lineSize) {
  ellipseTable <- sampleTable %>%
    group_by(!!as.name(groupColumn)) %>%
    summarise(df = list(data.frame(!!as.name(axis[1]), !!as.name(axis[2])))) %>%
    rowwise() %>%
    mutate(mat = list(cov.wt(df, rep(1, nrow(df))))) %>%
    mutate(center = list(mat$center), cov = list(mat$cov), wt = list(mat$wt)) %>%
    mutate(cov = ifelse(type == "se", list(cov * sum(wt ^ 2)), list(cov))) %>%
    mutate(ellip = list(vegan:::veganCovEllipse(cov, center, sqrt(qchisq(1 - signif, 2))))) %>%
    mutate(ellip = list(data.frame(ellip))) %>%
    unnest(ellip)
  gg +
    geom_path(aes_string(x = axis[1], y = axis[2], group = groupColumn, color = groupColumn), size = lineSize, data = ellipseTable)
}

plotEnvfit <- function(gg, envfit, sampleTable, axis, factorLabel, vectorLabel, factorDotSize, factorLabelSize, vectorLineSize, vectorLabelSize) {
  if(length(factorLabel) > 0) {
    gg <- plotEnvfitFactor(gg, envfit, sampleTable, axis, factorLabel, factorDotSize, factorLabelSize)
  }
  if(length(vectorLabel) > 0) {
    gg <- plotEnvfitVector(gg, envfit, sampleTable, axis, vectorLabel, vectorLineSize, vectorLabelSize)
  }
  gg
}

plotEnvfitFactor <- function(gg, envfit, sampleTable, axis, factorLabel, factorDotSize, factorLabelSize) {
  factorTable <- vegan::scores(envfit, display = "factor") %>%
    as.data.frame() %>%
    fitCoordinate(sampleTable, axis) %>%
    rownames_to_column("Factor") %>%
    filter(Factor %in% factorLabel)
  gg +
    geom_point(aes_string(x = axis[1], y = axis[2]), color = "green3", size = factorDotSize, data = factorTable) +
    ggrepel::geom_text_repel(aes_string(x = axis[1], y = axis[2], label = "Factor"), color = "green3", size = factorLabelSize, data = factorTable)
}

plotEnvfitVector <- function(gg, envfit, sampleTable, axis, vectorLabel, vectorLineSize, vectorLabelSize) {
  vectorTable <- vegan::scores(envfit, display = "vector") %>%
    as.data.frame() %>%
    fitCoordinate(sampleTable, axis) %>%
    rownames_to_column("Vector") %>%
    filter(Vector %in% vectorLabel)
  vectorLabelTable <- vectorTable %>%
    as.data.frame() %>%
    nudgeCoordinateReferToOrigin(axis)
  gg +
    geom_segment(aes_string(x = 0, y = 0, xend = axis[1], yend = axis[2]), color = "green3", size = vectorLineSize, arrow = arrow(), data = vectorTable) +
    geom_text(aes_string(x = axis[1], y = axis[2], label = "Vector"), color = "green3", size = vectorLabelSize, data = vectorLabelTable)
}

fitCoordinate <- function(df, refDf, axis) {
  ratio <- getCoordinateRatio(df, refDf, axis)
  df[[axis[1]]] <- df[[axis[1]]] * ratio
  df[[axis[2]]] <- df[[axis[2]]] * ratio
  df
}

getCoordinateRatio <- function(df, refDf, axis) {
  refRange <- c(range(refDf[, axis[1]]), range(refDf[, axis[2]]))
  dfRange <- c(range(df[, axis[1]]), range(df[, axis[2]]))
  ratio <- refRange / dfRange
  min(ratio[is.finite(ratio) & ratio > 0])
}

nudgeCoordinateReferToOrigin <- function(df, axis, nudgeRatio = 10) {
  df$length <- sqrt(df[, axis[1]] ^ 2 + df[, axis[2]] ^ 2)
  df$angle <- atan2(df[, axis[2]], df[, axis[1]])
  nudgeLength <- max(df$length) / nudgeRatio
  df[, axis[1]] <- cos(df$angle) * (df$length + nudgeLength)
  df[, axis[2]] <- sin(df$angle) * (df$length + nudgeLength)
  df
}

factorColumnBySum <- function(table, factorColumn, valueColumn) {
  table %>%
    group_by(!!as.name(factorColumn)) %>%
    mutate(sum := sum(!!as.name(valueColumn))) %>%
    ungroup() %>%
    arrange(sum) %>%
    mutate(!!as.name(factorColumn) := factor(!!as.name(factorColumn), levels = unique(!!as.name(factorColumn)))) %>%
    select(-sum)
}
  
filterTaxaByAbundance <- function(longTable, displayFilter, displayNumber, rank) {
  if(displayFilter == "top") {
    longTable <- longTable %>%
      group_by(!!sym(rank)) %>%
      mutate(sum = sum(Value)) %>%
      ungroup() %>%
      mutate(rank = dense_rank(desc(sum))) %>%
      filter(rank <= displayNumber) %>%
      select(-sum, -rank)
  }
  longTable
}

filterTaxaByPrevalence <- function(longTable, prevalence, rank) {
  longTable %>%
    group_by(!!sym(rank)) %>%
    mutate(preval = (sum(Value > 0) / length(Value)) * 100) %>%
    ungroup() %>%
    filter(preval >= prevalence) %>%
    select(-preval)
}
  
  
  
  
  
  
  
  
  
  






