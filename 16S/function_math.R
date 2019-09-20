getRichness <- function(community) {
  nlevels(factor(community))
}

getShannon <- function(community) {
  vegan::diversity(table(community), index = "shannon")
}

getInverseSimpson <- function(community) {
  vegan::diversity(table(community), index = "invsimpson")
}

getCoverage <- function(community, countAsRare) {
  counts <- table(community)
  1 - (length(counts[counts %in% (1 : countAsRare)]) / sum(counts))
}

