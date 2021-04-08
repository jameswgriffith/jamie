#' pomp
#'
#' @description Takes a questionnaire score and rescales it to a 0-100% metric.\cr
#' A score of 0% is the minimum possible value of the scale; a score of 100%
#' is the maximum possible value of the scale.\cr\cr
#' This function is based on the work of Patricia Cohen et al. (1999)
#' Cohen, P., Cohen, J., Aiken, L. S., & West, S. G. (1999).
#' The problem of units and the circumstance for POMP.
#' Multivariate behavioral research, 34(3), 315-346.
#'
#' @section Key Reference: \url{https://doi.org/10.1207/S15327906MBR3403_2}
#'
#' @param raw_scores A vector of numeric questionnaire scores.
#' @param min_possible The minimum possible value of the questionnaire.
#' @param max_possible The maximum possible value of the questionnaire.
#'
#' @return Percent of Maximum Possible Scores
#' @export
#'
#' @examples
#' \dontrun{
#'
#' pomp(test_scores, 10, 60)
#'
#' }
pomp <- function(raw_scores, min_possible, max_possible) {
  # Handle some errors
  if(!is.vector(raw_scores) || !is.numeric(raw_scores)) {
    stop("raw_scores must be a vector of numeric data. Please try again")
  }
  if(min_possible >= max_possible) {
    stop("min_possible must be less than max_possible")
  }
  if(min(raw_scores, na.rm = TRUE) < min_possible) {
    stop("Some elements of raw_scores are less than min_possible.")
  }
  if(max(raw_scores, na.rm = TRUE) > max_possible) {
    stop("Some elements of raw_scores are more than max_possible.")
  }
  # Done handling errors. Now calculate and return POMP scores:
  (raw_scores - min_possible) / (max_possible - min_possible) * 100
}
