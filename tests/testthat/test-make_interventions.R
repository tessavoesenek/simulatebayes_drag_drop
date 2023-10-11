test_that("ncol is ok", {
  id <- as.integer(c(1))
  Interventie <- c(1)
  K1 <- c(0.4)
  df <- data.frame(id, Interventie, K1)

  result <- make_interventions(data = df, -0.1, -0.04, 2)

  expected <- 9
  actual <- ncol(result$interventions)

  expect_equal(actual, expected)
})

test_that("nrows are ok", {
  id <- as.integer(c(1,2,3))
  Interventie <- c(1,1,0)
  K1 <- c(0.4, 0.4, 0.37)
  df <- data.frame(id, Interventie, K1)

  result <- make_interventions(data = df, -0.1, -0.04, 3)

  expected <- 108
  actual <- nrow(result$interventions)

  expect_equal(actual, expected)
})
