#
# R Library for BrainStars http://brainstars.org/
#
# Itoshi NIKAIDO <dritoshi@gmail.com>
# Copyright (C) 2009. Itoshi NIKAIDO. GPL 2
#

getBrainStarsExpression <- function(gene) {
  base.url    <- "http://brainstars.org/probeset/"
  url.options <- "/exprvalues.csv"
  url         <- paste(base.url, gene, url.options, sep="")

  expr <- read.csv(url, row.names=1)
  return(expr)
}

getBrainStarsFigure <- function(gene, url.option) {
  require("RCurl")

  base.url    <- "http://brainstars.org/probeset/"
  url.option.list <- c("exprgraph", "exprmap", "switchgraph", "switchhist", "switchmap")

  if (sum(url.option.list == url.option) != 1) {
    error.meg <- "Specify figure type."
    error.meg <- paste(error.meg, paste(url.option.list, collapse=", "))
    stop(warn=error.meg)
  }

  cat("Downloading...\n")
  url         <- paste(base.url, gene, "/", url.option, ".png", sep="")
  file.name   <- paste(gene, ".", url.option, ".png", sep="")
  plot <- getBinaryURL(url)
  cat("Done.\n")
  
  writeBin(plot, file.name)
  invisible(plot)
}

getBrainStarsSearch <- function(keyword) {
  require("RCurl")

  base.url    <- "http://brainstars.org/search/"

#  if (sum(keyword) != 1) {
#    error.meg <- "Input keyword type."
#    error.meg <- paste(error.meg, paste(keyword, collapse=", "))
#    stop(warn=error.meg)
#  }

  cat("Downloading...\n")
  url         <- paste(base.url, keyword, sep="")
  cat("Done.\n")
  results <- getURL(url)
  results <- unlist(strsplit(results, "\n"))
  return(results)
  
#  writeBin(plot, file.name)
#  invisible(plot)
}

getBrainStarsMarker <- function(keyword) {
  require("RCurl")

  base.url    <- "http://brainstars.org/marker/"
  json.option <- "?content-type=application/json"
  
  cat("Downloading...\n")
  url         <- paste(base.url, keyword, json.option, sep="")
  cat(url, "\n")
  cat("Done.\n")
  results <- getURL(url)
  #results <- unlist(strsplit(results, "\n"))
  return(results)
}
