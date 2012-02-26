\name{getBrainStarsGeneFamCat}
\alias{getBrainStarsGeneFamCat}
\title{Search gene family or categories from BrainStars}
\usage{
  getBrainStarsGeneFamCat(query)
}
\arguments{
  \item{query}{keyword}
}
\value{
  A character vector of list API response in JSON.
}
\description{
  This function queries a list of genes by gene family or
  categories name from BrainStars using by List API.
}
\details{
  List API is for retrieving genes in specific gene family
  or categories.

  Keyword search format:
  (category)/(keyword)/(offset),(limit)

  Count of entries search format:
  (category)/(keyword)/count

  (category): gene family / category name \itemize{ \item
  "tf": transcription factors \item "transmem":
  transmembrane genes \item "channel": channel genes \item
  "gpcr": GPCR genes \item "adhesion": cell adhesion genes
  \item "excellmat": extracellular matrix genes \item
  "structural": structural protein genes \item
  "neurogenesis": neurogenesis related genes \item "hox":
  homeobox genes \item "nucrcpt": nuclear receptor genes
  \item "ntnh": neurotransmitter/neurohormone genes \item
  "axon": axon guidance genes \item "fox": forkhead genes }

  (keyword): keyword. If no keyword search required, make
  this omited or blank.

  output: If the result has at least one hit entries, a
  list of entry IDs is returned in RJSONIN format. If not,
  404 Not found error code is returned. "offset,limit" can
  be used to retrieve a part of hit entries. If
  "offset,limit" is not given, all hits are returned.
}
\examples{
gfc.genes1 <- getBrainStarsGeneFamCat("tf//count")
gfc.genes2 <- getBrainStarsGeneFamCat("tf/terminal/all")
gfc.genes3 <- getBrainStarsGeneFamCat("tf/terminal/count")
}

