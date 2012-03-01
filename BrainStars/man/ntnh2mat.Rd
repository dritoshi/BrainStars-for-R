\name{ntnh2mat}
\alias{ntnh2mat}
\title{Transform results of ntnh's json to matrix}
\usage{
  count(rjsonio)
}
\arguments{
  \item{rjsonio}{result of List or Search API with type
  "ntnh"}
}
\value{
  A numeric vector of a number of hits
}
\description{
  This function extract a number of hits from result of
  List or Search count API
}
\examples{
mk.genes <- getBrainStars(query = "high/SCN/ME/all", type = "ntnh", output = "json")
mk.ntnh.mat <- ntnh2mat(mk.genes)
}

