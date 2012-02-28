\docType{package}
\name{BrainStars}
\alias{brainstars}
\alias{BrainStars}
\alias{BrainStars-package}
\alias{package-brainstars}
\title{BrainStars API client for R}
\description{
  This R package can search and get gene expression data
  and plots from BrainStars (or B*,
  \url{http://brainstars.org/}).
}
\details{
  BrainStars is a quantitative expression database of the
  adult mouse brain. The database has genome-wide
  expression profile at 51 adult mouse CNS regions.

  For 51 CNS regions, slices (0.5-mm thick) of mouse brain
  were cut on a Mouse Brain Matrix, frozen, and the
  specific regions were punched out bilaterally with a
  microdissecting needle (gauge 0.5 mm) under a
  stereomicroscope. For each region, we took samples every
  4 hours, starting at ZT0 (Zeitgaber time 0; the time of
  lights on), for 24 hours (6 time-point samples for each
  region), and we pooled the samples from the different
  time points. We independently sampled each region twice
  (n=2).

  These samples were purified their RNA, and measured with
  Affymetrix GeneChip Mouse Genome 430 2.0 arrays.
  Expression values were then summarized with the RMA
  method. After several analysis with the expression data,
  the data and analysis results were stored in the
  BrainStars database.

  BrainStars database has a REST API to query gene
  expression data and some kind of figures written by Dr.
  Takeya Kasukawa. This package is wrapper for BrainStars
  REST API in R. BrainStars data, images and texts
  (excluding ABA data and images) are licensed under a
  Creative Commons Attribution 2.1 Japan License.
}
\references{
  Takeya Kasukawa*, Koh-hei Masumoto*, Itoshi Nikaido*,
  Mamoru Nagano, Kenichiro D. Uno, Kaori Tsujino, Carina
  Hanashima, Yasufumi Shigeyoshi, and Hiroki R. Ueda:
  Quantitative Expression Profile of Distinct Functional
  Regions in the Adult Mouse Brain, PLoS ONE 6(8), e23228,
  2011. (doi:10.1371/journal.pone.0023228) *equally
  contributed
}

