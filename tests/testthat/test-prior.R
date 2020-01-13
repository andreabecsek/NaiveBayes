context('class priors')

test_that("prior is calculated correctly",{
  # some random data
  n <- 20
  X <- matrix(rnorm(2*n),ncol=2)
  # class labels
  prior <- c(0.7,0.3)
  count <- prior*20
  y <- matrix(c(rep(0,count[1]),rep(1,count[2])),ncol=1)
  # fit model
  model <- naive_bayes(X,y)
  expect_equal(prior,model$prior)
})
