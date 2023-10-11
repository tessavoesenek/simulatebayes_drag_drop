load("test_interventions.rda")
result <- run_regressions(test_interventions,0)

test_that("output is list", {
  expect_type(result, "list")
})
