\name{getBrainStarsFigure}
\alias{getBrainStarsFigure}
\title{Get barplot of gene expression profile by one gene}
\usage{
  getBrainStarsFigure(id, fig.type)
}
\arguments{
  \item{id}{Affymetrix GeneChip ID (Mouse Genome 430 2.0
  array)}

  \item{fig.type}{Figure type}
}
\description{
  This function get a barplotof gene expression profile
  from BrainStars
}
\examples{
getBrainStarsFigure("1439627_at", "exprgraph")
getBrainStarsFigure("1439627_at", "exprmap")
getBrainStarsFigure("1439627_at", "switchgraph")
getBrainStarsFigure("1439627_at", "switchhist")
getBrainStarsFigure("1439627_at", "switchmap")
}
\keyword{bio}

