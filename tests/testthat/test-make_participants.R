test_that("length is ok", {
  actual_output <- make_participants(2,4)
  expect_length(actual_output$Interventie, 2+4)
})

test_that("weird values are ok", {
  actual_output <- make_participants(200,4)
  expect_length(actual_output$Interventie, 200+4)
})
