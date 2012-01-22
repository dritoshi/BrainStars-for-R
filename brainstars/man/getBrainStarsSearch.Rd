\name{getBrainStarsSearch}
\alias{getBrainStarsSearch}
\title{Keyword search}
\usage{
  getBrainStarsSearch(keyword)
}
\arguments{
  \item{keyword}{keyword}
}
\description{
  This function search by keyword in BrainStars
}
\examples{
recep.list  <- getBrainStarsSearch("receptor/10,5")
recep.count <- getBrainStarsSearch("receptor/count")
}
\keyword{bio}

