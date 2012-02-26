\name{getBrainStarsFigure}
\alias{getBrainStarsFigure}
\title{Get plot of gene expression profile by one gene}
\usage{
  getBrainStarsFigure(query, fig.type, fig.format)
}
\arguments{
  \item{query}{id Affymetrix GeneChip ID (Mouse Genome 430
  2.0 array)}

  \item{fig.type}{"exprmap", "exprgraph", "switchhist",
  "switchgraph", "switchmap", "multihist", "multigraph",
  "multimap"}

  \item{fig.format}{"png" or "pdf"}
}
\value{
  A "raw" vector of figure image. The figure image file is
  saved current directory.
}
\description{
  This function get plot of gene expression profile from
  BrainStars
}
\examples{
getBrainStarsFigure("1439627_at", "exprmap",     "png")
getBrainStarsFigure("1439627_at", "exprgraph",   "png")

getBrainStarsFigure("1439627_at", "switchhist",  "pdf")
getBrainStarsFigure("1439627_at", "switchgraph", "pdf")
getBrainStarsFigure("1439627_at", "switchmap",   "pdf")

getBrainStarsFigure("1439627_at", "multihist",  "png")
getBrainStarsFigure("1439627_at", "multigraph", "png")
getBrainStarsFigure("1439627_at", "multimap",   "png")
}

