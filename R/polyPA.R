#' polyPA
#'
#' @description Conducts a parallel analysis using the method of Lubbe (2019).
#' The code in this function is adapted from the key reference (see below).
#' This function relies on the psych package to calculate the polychoric correlations.
#'
#' @section Key Reference: \url{https://doi.org/10.1037/met0000171}
#'
#' @param data Categorical input of questionnaire items
#' @param replications The number of replications in the simulation
#' @param prc The percentile for obtaining the reference eigenvalues
#'
#' @return Empirical and reference eigenvalues
#' @export
#'
#' @examples
#' \dontrun{
#'
#' pa_results <- polyPA(hl[1:10], 500, .99)
#'
#' }
#' @importFrom psych polychoric
#' @importFrom stats quantile rnorm
polyPA <- function(data,
                   replications = 1000,
                   prc = 0.99) {

  # Check input
  if(replications < 1){
    stop("replications should be a positive integer; I recommend 500 or larger.")
  }

  if(replications < 500){
    warning("replications should be a large number; I recommend 500 or larger.")
  }

  if(prc < 0 | prc > 1){
    stop("prc (for percentile) should be between 0 and 1; I recommend .99, but other options are .5 or .95.")
  }

  p <- ncol(data)
  n <- nrow(data)

    #1. Calculate UCPs
  ucp <- list()
  for(j in seq_along(p)) {
    freq <- table(data[, j])
    ucp[[j]] <- c(0, cumsum(freq) / sum(freq))
  }
  ev <- matrix(, replications, p)
  for(i in seq_along(replications)) {

    #2. Generate Random Samples
    x <- y <- matrix(rnorm(n * p), n, p)
    for(j in seq_along(p)) {

      #3. Discretization of Random Variables
      splt <- quantile(x[, j], probs = ucp[[j]])
      for(k in 1:(length(splt) - 1))
        y[which(x[, j] >= splt[k] & x[, j] <= splt[k + 1]), j] <- k
    }

    #4. Calculate Polychoric Correlations
    R <- psych::polychoric(y)$rho

    #5. Calculate Eigenvalues
    ev[i, ] <- eigen(R)$values
  }
  list(sample_ev = eigen(polychoric(data)$rho)$values,
       reference_ev = apply(ev, 2, quantile, probs = prc))
}

# Parallel Analysis With Categorical Variables: Impact of Category
# Probability Proportions on Dimensionality Assessment Accuracy
# Dirk Lubbe
# Justus-Liebig-Universität Giessen

# Lubbe, D. (2019). Parallel analysis with categorical variables:
# Impact of category probability proportions on dimensionality
# assessment accuracy. Psychological methods, 24(3), 339.
