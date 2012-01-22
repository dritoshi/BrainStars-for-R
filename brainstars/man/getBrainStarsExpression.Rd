\name{getBrainStarsExpression}
\alias{getBrainStarsExpression}
\title{Get raw expression profile by one gene}
\usage{
  getBrainStarsExpression(id)
}
\arguments{
  \item{id}{Affymetrix GeneChip ID (Mouse Genome 430 2.0
  array)}
}
\description{
  This function get a gene expression profile from
  BrainStars
}
\examples{
my.eset <- getBrainStarsExpression("1439627_at")
}
\keyword{bio}

