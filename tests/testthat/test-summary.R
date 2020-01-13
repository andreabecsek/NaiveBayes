context("Summaries")
library(MASS)

test_that("summary is calculated correctly",{
  # some random data
  # observations in class 1
  mu_1 <- c(40,-80)
  sd_1 <- c(1,1)
  C1 <- mvrnorm(60,mu_1, diag(sd_1^2))
  # observations in class 2
  mu_2 <- c(-50,60)
  sd_2 <- c(1,1)
  C2 <- mvrnorm(40,mu_2, diag(sd_2^2))
  # full data matrix
  X <- rbind(C1,C2)
  y <- matrix(c(rep(0,60),rep(1,40)),ncol=1)
  # fit model
  model <- naive_bayes(X,y)
  summaries <- round(model$summaries)
  expect_equal(summaries[1,,1],mu_1)
  expect_equal(summaries[2,,1],mu_2)
  expect_equal(summaries[1,,2],sd_1)
  expect_equal(summaries[2,,2],sd_2)
})
