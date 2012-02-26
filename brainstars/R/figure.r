## List API

#' Get plot of gene expression profile by one gene
#'
#' This function get plot of gene expression profile from BrainStars
#'
#' @usage getBrainStarsFigure(query, fig.type, fig.format)
#' @param query id Affymetrix GeneChip ID (Mouse Genome 430 2.0 array)
#' @param fig.type "exprmap", "exprgraph", "switchhist", "switchgraph",
#'  "switchmap", "multihist", "multigraph", "multimap"
#' @param fig.format "png" or "pdf"
#' @return A "raw" vector of figure image. The figure image file is saved current directory.
#'
#' @export
#' @importMethodsFrom RJSONIO fromJSON
#' @examples
#' getBrainStarsFigure("1439627_at", "exprmap",     "png")
#' getBrainStarsFigure("1439627_at", "exprgraph",   "png")
#'
#' getBrainStarsFigure("1439627_at", "switchhist",  "pdf")
#' getBrainStarsFigure("1439627_at", "switchgraph", "pdf")
#' getBrainStarsFigure("1439627_at", "switchmap",   "pdf")
#'
#' getBrainStarsFigure("1439627_at", "multihist",  "png")
#' getBrainStarsFigure("1439627_at", "multigraph", "png")
#' getBrainStarsFigure("1439627_at", "multimap",   "png")
getBrainStarsFigure <- function(query, fig.type, fig.format) {
  api.name = "probeset"
  res = new("BrainStars", query=query, api.name=api.name)
  o <- fromJSON(res@response)

  # Figrue Image Format
  image.format.value <- ""
  if (fig.format == "png") {
    image.format.value <- "imagehref"    
  } else if (fig.format == "pdf") {
    image.format.value <- "pdfhref"
  } else {
    stop("Unknown figure format.\n")
  }

  # Figrue Types
  url <-  ""
  if        (fig.type == "exprmap") {
    url <- o$resources$exprmap[image.format.value]
  } else if (fig.type == "exprgraph") {
    url <- o$resources$exprgraph[image.format.value]
    
  } else if (fig.type == "switchhist") {
    url <- o$resources$switchhist[image.format.value]    
  } else if (fig.type == "switchgraph") {
    url <- o$resources$switchgraph[image.format.value]
  } else if (fig.type == "switchmap") {
    url <- o$resources$switchmap[image.format.value]

  } else if (fig.type == "multihist") {
    url <- o$resources$multihist[image.format.value]    
  } else if (fig.type == "multigraph") {
    url <- o$resources$multigraph[image.format.value]
  } else if (fig.type == "multimap") {
    url <- o$resources$multimap[image.format.value]    
  } else {
    stop("Unknown figure type.\n")    
  }    

  image.file <- paste(query, fig.type, fig.format, sep=".")
  #cat("Download figure file from", url,  "\n")
  cat("Save figure file at", image.file, "\n")

  fig <- getBinaryURL(url)
  writeBin(fig, image.file)
  invisible(fig)  
}
