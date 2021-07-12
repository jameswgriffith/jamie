#' A function to collapse questionnaire items before item response theory(IRT)
#' analyses, polychoric correlation analyses, or other situations in which
#' items are to be treated as an ordered category, but have have one or more
#' sparse response categories (e.g., fewer that 5 responses in a particular
#' category).
#'
#' @description This function takes a vector (v) of response category data, looks
#' for response categories that are sparse (e.g., with fewer that 5 responses
#' in the data set). Starting with the least frequent responses, the function
#' will merge them to an adjacent category. The function will merge the 
#' whichever adjacent response category has fewer responses; in the case of
#' ties the function will follow a user-specified preference (right or left).
#' The parameters "prefer" and "merge_first" may result in slightly different
#' patterns of merging depending on the pattern of sparse response categories.
#' 
#' @details NA is not considered a response type. It is not recommended to 
#' recode NA into a response type for this function.
#'
#' @param v A vector of questionnaire response data. These are assumed to be
#' ordered numeric data. v should be passed to collapse() as numeric data rather
#' than as factors or character data. collapse() will attempt to sort and merge
#' whatever data passed to it though.
#' 
#' @param min_f The minimum frequency desired for response category data.
#' Any response categories that have a frequency count less than min_f will
#' be merged with an adjacent response category. The default value is 5. If it
#' is set to a higher number, then each category will have more data, but
#' collapse() will be more likely to merge. A lower number will mean that
#' collapse() is less likely to merge, but fewer observations will be in each
#' resultant category.
#' 
#' @param prefer In cases where a sparse response category has a left and right
#' adjacent response category of equal frequency, does the user prefer to merge
#' with the category on the left or on the right? This is only relevant for
#' response categories in the middle (e.g., for 1, 2, 3, 4, 5, this is only
#' relevant for 2, 3, and 4).
#' 
#' @param merge_first collapse() begins by merging the least frequent response
#' category. If there is more than one response category with the lowest
#' frequency count, does the user prefer starting with the leftmost or
#' rightmost one?
#' 
#' @param warn_me collapse() will provide warnings about the collapsing, 
#' especially when it is important to check the results, or to tell the
#' user if there is a potential problem with the data. The warnings can
#' be turned off using this parameter.
#'
#' @return A vector of merged response data.
#' If the function is unable to collapse the data, then the original data
#' are returned (usually with a warning if there is potentially a problem
#' with the data).
#'  
#' @export
#'
#' @examples
#' \dontrun{
#'
#' sparse_vector <- c(1, 1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 4, 4, 5, 5)
#' collapse(sparse_vector)
#' 
#' table(sparse_vector)
#' table(collapse(sparse_vector))
#'
#' }
collapse <- function(v,
                     min_f = 5,
                     prefer = c("left", "right"),
                     merge_first = c("leftmost", "rightmost"),
                     warn_me = TRUE) {
  
  prefer <- match.arg(prefer, c("left", "right"))
  merge_first <- match.arg(merge_first, c("leftmost", "rightmost"))
  
  # Check for errors
  if (!is.vector(v)) stop("v must be a vector")
  
  if (min_f < 0) stop("min_f must be an integer of 0 or greater. Please try again")
  
  if (all(is.na(v))) {
    if(warn_me) warning("I am unable to collapse an empty vector.")
    return(v)
  }
  
  if (length(v) < 2 * min_f) {
    if(warn_me) warning("There are not enough observations to collapse.")
    return(v)
  }
  
  if (length(table(v)) <= 2 ) {
    if(warn_me) warning("I can only collapse if three or more response types are present.")
    return(v)
  }
  
  if (!any_sparse_in_v(v, min_f = min_f)) return(v)
  
  if(warn_me && n_sparse(v) >= 2 && n_sparse(v) < n_resp_cats(v)) {
    warning("You have more than two sparse response types. Check the collapsed results carefully.")
  }
  
  if(warn_me && n_sparse(v) == n_resp_cats(v)) {
    warning("All of your response types are sparse. Check the collapsed results carefully.")
  }
  
  if(warn_me && !is.numeric(v)) {
    warning("This function is recommended for numeric data.\n",
            "If your data are sortable, then you can try, but check the results carefully.")
  }
  
  # Create a frequency table of v's response options
  tbl <- table(sort(v))
  
  # Find index of least frequent response type 
  # First change arguments
  if(merge_first == "leftmost") recoded_merge_1st <- "min"
  if(merge_first == "rightmost") recoded_merge_1st <- "max"
  
  index <- i_freq_min(tbl, recoded_merge_1st)
  
  merge_to <- merge_right_or_left(v, i = index, prefer = prefer)
  
  v <- if (merge_to == "right") {
    merge_w_right(v, index)
  } else if (merge_to == "left") {
    merge_w_left(v, index)
  }
  
  collapse(v,
           min_f = min_f,
           prefer = prefer,
           merge_first = merge_first,
           warn_me = FALSE)
  
}


# Internals ---------------------------------------------------------------

#' A internal function to check whether a vector v contains any sparse responses.
#'
#' @description This function checks the vector v for any sparse responses.
#' It will return TRUE or FALSE. If v contains only NA, then NA will be
#' returned by the function.
#' 
#' @note This is intended as an internal function to
#' the collapse() function. Use with care for other purposes.
#'
#' @param v A vector of questionnaire response data.
#' 
#' @param min_f The minimum frequency desired for response category data.
#' Any response categories that have a frequency count less than min_f will
#' be considered by sparse (i.e., have potentially too little data).
#'
#' @return TRUE or FALSE depending on whether sparse responses are present.
#' 
#' @seealso collapse()
#'
#' @examples
#' \dontrun{
#'
#' sparse_vector <- c(1, 1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 4, 4, 5, 5)
#' any_sparse_in_v(sparse_vector)
#'
#' }
any_sparse_in_v <- function(v, min_f = 5) {
  # Check for errors in the input
  if(!is.vector(v)) stop("v must be a vector. Please try again.")
  if(min_f < 0) stop("min_f must be an integer of 0 or greater. Please try again")
  
  # Return output
  if(all(is.na(v))) NA
  else any(table(v) < 5)
}

#' An internal function to return an index from among sparse response
#' categories. This function tells collapse() which response category
#' to start with when merging.
#'
#' @description This function looks at a table, finds indices of the
#' least frequent response category (or categories), and returns one
#' as a starting point for collapse().
#' 
#' @note This is intended as an internal function to
#' the collapse() function. Use with care for other purposes.
#'
#' @param tbl A table, usually one that was made from a vector of response
#' categories.
#' 
#' @param return_which In situations in which more than one response category
#' is the least frequent (i.e., with the lowest frequency count), should the 
#' function return the minimum response (i.e., the leftmost category if sorted in
#' ascending order) or the maximum response (i.e., the rightmost category if
#' sorted in ascending order). The default is "min" for "minimum" or the
#' "rightmost" response option. This parameter is only relevant when more than
#' one response category is tied for being the least frequent.
#'
#' @return A single numeric index in tbl that tells collapse() which response
#' category to start with.
#' 
#' @seealso collapse()
#'
#' @examples
#' \dontrun{
#'
#' sparse_vector <- c(1, 1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 4, 4, 5, 5)
#' i_freq_min(table(sparse_vector))
#' table(sparse_vector) |> i_freq_min()
#'
#' }
i_freq_min <- function(tbl, return_which = c("min", "max")) {
  # Check for errors
  if(!is.table(tbl)) stop("tbl must be a table.")

  return_which <- match.arg(return_which, c("min", "max"))
  
  least_freq_resps <- as.numeric(which(tbl == min(tbl)))
  
  do.call(return_which, list(least_freq_resps))
  
}

#' An internal function to that takes a response category found in a 
#' sorted table of v,  at index i in this table, and merges it with
#' the adjacent category to the left at i - 1.
#'
#' @description After sorting the response of v and putting them into a table,
#' this function looks at the category in said table at position i and merges
#' it with the category to the left at position i - 1.
#' 
#' @note This is intended as an internal function to
#' the collapse() function. Use with care for other purposes.
#'
#' @param v A vector of response categories.
#' 
#' @param i An index of the to-be-collapsed category. i must range from 2 to
#' the maximum number of response categories. For example, for response 
#' categories 1, 2, 3, 4, and 5, "1" is not possible because there is no place
#' to the left. Thus, i can be 2, 3, 4, or 5.
#'
#' @return A vector of collapsed response data.
#' 
#' @seealso collapse()
#' @seealso merge_w_right()
#'
#' @examples
#' \dontrun{
#'
#' sparse_vector <- c(1, 1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 4, 4, 5, 5)
#' merge_w_left(sparse_vector, 2)
#' 
#' }
merge_w_left <- function(v, i) {
  # Check for errors
  if(!is.vector(v)) stop("v must be a vector.")
  
  # Sort the unique values of v and put into a table
  sorted_tbl_v <- sort(unique(v))
  
  # Check for errors
  if(i <= 1)
    stop("i must be between 2 and ", length(sorted_tbl_v))
  
  if(i > length(sorted_tbl_v))
    stop("i must be between 2 and ", length(sorted_tbl_v))
  
  # Merge with element on the left
  v[which(v == sorted_tbl_v[i])] <- sorted_tbl_v[i - 1]
  
  # Return v after it has been collapsed
  v
}

#' An internal function to that takes a response category found in a 
#' sorted table of v,  at index i in this table, and merges it with
#' the adjacent category to the right at i + 1.
#'
#' @description After sorting the response of v and putting them into a table,
#' this function looks at the category in said table at position i and merges
#' it with the category to the right at position i + 1.
#' 
#' @note This is intended as an internal function to
#' the collapse() function. Use with care for other purposes.
#'
#' @param v A vector of response categories.
#' 
#' @param i An index of the to-be-collapsed category. i must range from 1 to
#' the maximum number of response categories minus 1. For example, for response 
#' categories 1, 2, 3, 4, and 5, "5" is not possible because there is no place
#' to the right. Thus, i can be 1, 2, 3, or 4.
#'
#' @return A vector of collapsed response data.
#' 
#' @seealso collapse()
#' @seealso merge_w_left()
#'
#' @examples
#' \dontrun{
#'
#' sparse_vector <- c(1, 1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 4, 4, 5, 5)
#' merge_w_right(sparse_vector, 2)
#' 
#' }
merge_w_right <- function(v, i) {
  # Check for errors
  if(!is.vector(v)) stop("v must be a vector.")
  
  # Sort the unique values of v and put into a table
  sorted_tbl_v <- sort(unique(v))
  
  # Check for errors
  if(i < 1)
    stop("i must be between 1 and ", length(sorted_tbl_v) - 1)
  
  if(i >= length(sorted_tbl_v))
    stop("i must be between 1 and ", length(sorted_tbl_v) - 1)
  
  # Merge with element on the right
  v[which(v == sorted_tbl_v[i])] <- sorted_tbl_v[i + 1]
  
  # Return v after it has been collapsed
  v
  
}

#' An internal function to that tells collapse whether to collapse with the 
#' category to the left or to the right, depending on which adjacent category
#' is smaller, or based on user preference in case of a tie.
#'
#' @description An internal function to that tells collapse whether to collapse
#' with the category to the left or to the right, depending on which adjacent 
#' category is smaller, or based on user preference in case of a tie.
#' 
#' @note This is intended as an internal function to
#' the collapse() function. Use with care for other purposes.
#'
#' @param v A vector of response categories.
#' 
#' @param i An index based on a table of sorted (ascending) response
#' categories of v. i must range from 1 to the maximum number of response
#' categories. For example, for response categories 1, 2, 3, 4, and 5, 
#' i can be 1, 2, 3, 4, or 5.
#' 
#' @param prefer A user supplies preference of whether to merge left or right
#' in the case of a tie (i.e., equal response counts at i - 1 and i + 1.
#'
#' @return "left" or "right"; this tells collapse which direction to merge to.
#' 
#' @seealso collapse()
#' @seealso merge_w_left()
#' @seealso merge_w_right()
#'
#' @examples
#' \dontrun{
#'
#' sparse_vector <- c(1, 1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 4, 4, 5, 5)
#' merge_right_or_left(sparse_vector, 2, "left")
#' merge_right_or_left(sparse_vector, 2, "right")
#' 
#' }
merge_right_or_left <- function(v, i, prefer = c("left", "right")) {
  
  prefer <- match.arg(prefer, c("left", "right"))
  
  # Check for errors
  if(!is.vector(v)) stop("v must be a vector.")
  
  # Sort the unique values of v and put into a table
  sorted_tbl_v <- sort(unique(v))
  
  # Check for other errors
  if(i < 1 || i > max(sorted_tbl_v)) {
    stop("i out of range.\n",
         "The range for index i is from 1 to ", max(sorted_tbl_v))
  }
  
  # Sort v itself and put into a table
  tbl <- table(sort(v))
  
  merge_to <- prefer
  
  if (i == 1) {
    merge_to <- "right"
  } else if (i == length(sorted_tbl_v)) {
    merge_to <- "left"
  } else if (tbl[i - 1] == tbl[i + 1] && prefer == "right") {
    merge_to <- "right"
  } else if(tbl[i - 1] == tbl[i + 1] && prefer == "left") {
    merge_to <- "left"
  } else if(tbl[i - 1] < tbl[i + 1]) {
    merge_to <- "left"
  } else if(tbl[i - 1] > tbl[i + 1]) {
    merge_to <- "right"
  }
  
  # Return output
  merge_to

}

#' An internal function that returns indices of sparse response categories.
#'
#' @description An internal function that returns the indices (i) of sparse
#' response categories, given a table (tbl).
#' 
#' @note This is intended as an internal function to
#' the collapse() function. Use with care for other purposes.
#'
#' @param tbl A table, usually made from a vector of self-report items.
#' 
#' @param min_f The threshold for non-sparse frequencies. Any category less
#' than min_f will be flagged as sparse.
#' 
#' @return Numerical indices indicating where in the table a sparse category
#' is found.
#'
#' @seealso collapse()
#'
#' @examples
#' \dontrun{
#'
#' sparse_vector <- c(1, 1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 4, 4, 5, 5, 6, 6, 6)
#' i_sparse(table(sparse_vector))
#' i_sparse(table(sparse_vector, 3))
#' 
#' # By coercion
#' i_sparse(sparse_vector)
#' 
#' }
i_sparse <- function(tbl, min_f = 5) {
  # If tbl is not a table, then coerce it to one
  if(!is.table(tbl)) {
    
    # If not a vector, stop and print error message.
    if(!is.vector(tbl)) stop("tbl must be a table. Vectors will be coerced to tables.")
    
    tbl <- table(sort(tbl))
    
  }
  
  # Check for other errors
  if(min_f < 0) stop("min_f must be an integer of 0 or greater. Please try again")
  
  # Return output
  which(tbl < min_f)
  
}

#' An internal function that returns the number of sparse response categories.
#'
#' @description An internal function that returns the number of sparse
#' response categories in a vector v.
#' 
#' @note This is intended as an internal function to
#' the collapse() function. Use with care for other purposes.
#'
#' @param v A vector, usually of self-report items, and usually numeric.
#' 
#' @param min_f The threshold for non-sparse frequencies. Any category less
#' than min_f will be flagged as sparse.
#' 
#' @return The number of sparse responses (i.e., < min_f) in v.
#'
#' @seealso collapse()
#' @seealso n_resp_cats()
#'
#' @examples
#' \dontrun{
#'
#' sparse_vector <- c(1, 1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 4, 4, 5, 5, 6, 6, 6)
#' n_sparse(sparse_vector)
#' 
#' }
n_sparse <- function(v, min_f = 5) {
  # Check for errors
  if (!is.vector(v)) stop("v must be a vector.")
  if (min_f < 0) stop("min_f must be an integer of 0 or greater. Please try again")
  
  tbl <- table(sort(v))

  # Return output
  sum(tbl < min_f)
  
}

#' An internal function that returns the number of possible response categories.
#'
#' @description An internal function that returns the number of possible
#' response categories in a vector v.
#' 
#' @note This is intended as an internal function to
#' the collapse() function. Use with care for other purposes.
#'
#' @param v A vector, usually of self-report items, and usually numeric.
#' 
#' @return The number of response categories in v. Note that NA is not counted
#' as a response category
#'
#' @seealso collapse()
#' @seealso n_sparse()
#'
#' @examples
#' \dontrun{
#'
#' sparse_vector <- c(1, 1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 4, 4, 5, 5, 6, 6, 6)
#' n_resp_cats(sparse_vector)
#' 
#' }
n_resp_cats <- function(v) {
  # Check for errors
  if (!is.vector(v)) stop("v must be a vector.")
  
  # Remove NA values from v
  v <- v[!is.na(v)]
  
  length(unique(v))
  
}