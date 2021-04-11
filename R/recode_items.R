# Updated 11 April 2021

#' recode_items
#'
#' @description This function is used to recode items
#' (e.g., 1 = 4, 2 = 3, 3 = 2, 4 = 1).
#'
#' @details This function is mostly used inside scoring algorithms.
#' Input can be vectors, matrices, or dataframes.
#'
#' @param items Items to be recoded.
#'
#' @param original A vector containing the original coding of the variable
#' (e.g., 1, 2, 3, 4).
#'
#' @param recoded A vector containing the recoding of the variable
#' (e.g., 4, 3, 2, 1). "recoded" and "original" must be the same length.
#'
#' @return Recoded data are returned (usually questionnaire items).
#'
#' @export
#'
#' @examples
#'\dontrun{
#'
#' recode_items(1:4, 1:4, c("A", "B", "C", "D"))
#'
#' }
recode_items <- function(items, original, recoded) {

  # Check for errors in the input
  if(is.null(original)) {
    stop("original is required. Please try again")
  }

  if(is.null(recoded)) {
    stop("recoded is required. Please try again")
  }

  if(length(original) != length(recoded)) {
    stop("original and recoded must be equal in length. Please try again")
  }

  # Check data type of items
  items_type <- "default" # Set default type for anything
                          # except a dataframe or matrix
  if(is.data.frame(items)) items_type <- "dataframe"
  if(is.matrix(items)) items_type <- "matrix"

  if(items_type == "dataframe") {
    recoded_items <- recode_items_in_df(items,
                                        original = original,
                                        recoded = recoded)
  }

  if(items_type == "matrix") {
    recoded_items <- recode_items_in_matrix(items,
                                            original = original,
                                            recoded = recoded)
  }

  if(items_type == "default") {

    # Find indices for items in original
    i <- match(items, original)

    recoded_items <- recoded[i]
  }

  # Return recoded items
  recoded_items

}
