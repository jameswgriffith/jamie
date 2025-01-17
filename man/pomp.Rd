% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pomp.R
\name{pomp}
\alias{pomp}
\title{pomp}
\usage{
pomp(raw_scores, min_possible, max_possible)
}
\arguments{
\item{raw_scores}{A vector of numeric questionnaire scores.}

\item{min_possible}{The minimum possible value of the questionnaire.}

\item{max_possible}{The maximum possible value of the questionnaire.}
}
\value{
Percent of Maximum Possible Scores
}
\description{
Takes a questionnaire score and rescales it to a 0-100% metric.\cr
A score of 0% is the minimum possible value of the scale; a score of 100%
is the maximum possible value of the scale.\cr\cr
This function is based on the work of Patricia Cohen et al. (1999)
Cohen, P., Cohen, J., Aiken, L. S., & West, S. G. (1999).
The problem of units and the circumstance for POMP.
Multivariate behavioral research, 34(3), 315-346.
}
\section{Key Reference}{
 \url{https://doi.org/10.1207/S15327906MBR3403_2}
}

\examples{
\dontrun{

pomp(test_scores, 10, 60)

}
}
