#' nmi_matrix
#'
#' @description This function populates a matrix with
#' normalised mutual information (NMI) for each pair of
#' columns in the dataframe x.
#'
#' @details The input must be a dataframe or coercible to a dataframe.
#'
#' @param x A dataframe (e.g., questionnaire items)
#'
#' @param ... Additional arguments to be passed to NMI as part of the
#' aricode package.
#'
#' @return A matrix[i, j] with NMI for each pair of columns.
#'
#' @export
#'
#' @importFrom aricode NMI
#'
#' @examples
#'\dontrun{
#' nmi_matrix(RRS))
#' }
nmi_matrix <- function(x, ...) {

  # Check for errors in the input
  if(!is.data.frame(x) & !is.matrix(x)) {
    stop("x must be a dataframe or a matrix. Please try again")
  }

  x <- as.data.frame(x)

  col_pairs_to_matrix(x, FUN = "NMI", ...)

}
