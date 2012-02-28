\name{getBrainStarsExpression}
\alias{getBrainStarsExpression}
\title{Retreive a list of expression profile from BrainStars}
\usage{
  getBrainStarsExpression(queries)
}
\arguments{
  \item{queries}{vector of Affymetrix GeneChip IDs (Mouse
  Genome 430 2.0 array)}
}
\value{
  An object of ExpressionSet is returned.
}
\description{
  This function queries a list of gene expression profile
  from BrainStars
}
\examples{
mk.genes <- getBrainStarsMarker("low/SCN/all")
mk.ids   <- probeSetIDs(mk.genes)
mk.eset  <- getBrainStarsExpression(mk.ids)
}

