\name{getBrainStarsEntry}
\alias{getBrainStarsEntry}
\title{Get an entry information in JSON format}
\usage{
  getBrainStarsEntry(query)
}
\arguments{
  \item{query}{Affymetrix GeneChip ID (Mouse Genome 430 2.0
  array)}
}
\value{
  A character vector of entry API response in JSON.
}
\description{
  This function get entry information (annotation,
  expressions, links) in JSON format from BrainStars using
  by Entry API.
}
\examples{
my.entry <- getBrainStarsEntry("1439627_at")
}

