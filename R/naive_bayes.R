#' Title Naive Bayes model implementation
#'
#' @param X data matrix which stores observations as rows and
#' features as columns
#'
#' @param y column vector that stores each observation's class labels
#'
#' @return
#' @export
#'
#' @examples
#'
naive_bayes <- function(X, y) {
  n <- dim(X)[1]
  d <- dim(X)[2]
  levels <- sort(unique(y)[, 1])
  k <- length(levels)

  prior <- rep(0, k)
  for (i in 1:k) {
    prior[i] <- sum(y == levels[i]) / n
  }

  summaries <- array(rep(1, d * k * 2), dim = c(k, d, 2))
  for (i in 1:k) {
    X_k <- X[which(y == (i - 1)), ]
    summaries[i, , 1] <- apply(X_k, 2, mean)
    summaries[i, , 2] <- apply(X_k, 2, sd)
  }
  nb <- list(X = X, y = y, summaries = summaries, prior = prior)
  class(nb) <- "naive_bayes"
  return(nb)
}

#' Title Predict method for Naive Bayes model (uses Gaussian assumption)
#'
#' @param obj model obtained by fitting Naive Bayes model
#' @param x_new test data matrix with observations as rows and features as columns
#'
#' @return predicted class labels
#' @export
#'
#' @examples
#'
predict.naive_bayes <- function(obj, x_new) {
  prior <- obj$prior
  summaries <- obj$summaries
  n <- dim(x_new)[1]
  d <- dim(x_new)[2]
  k <- dim(summaries)[1]

  result <- matrix(rep(0, n * k), nrow = n)
  for (obs in 1:n) {
    for (class in 1:k) {
      post <- log(prior[class])
      for (feat in 1:d) {
        mu <- summaries[class, feat, 1]
        sd <- summaries[class, feat, 2]
        cond <- dnorm(x_new[obs, feat], mu, sd, log = TRUE)
        post <- post + cond
      }
      result[obs, class] <- post
    }
  }

  pred <- apply(result, 1, which.max)
  return(matrix((pred - 1), ncol = 1))
}
