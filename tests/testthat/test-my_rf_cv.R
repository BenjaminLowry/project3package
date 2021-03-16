test_that("my_rf_cv returns a numeric mean square error value", {
  rf_5 <- my_rf_cv(5)
  expect_true(is.numeric(rf_5))
  expect_true(rf_5 >= 0)
})

test_that("my_rf_cv throws error when input is non-numeric", {
  expect_error(my_rf_cv("five"))
})

test_that("my_rf_cv throws error when input is negative", {
  expect_error(my_rf_cv(-2))
})

test_that("my_rf_cv throws error when input is zero", {
  expect_error(my_rf_cv(0))
})
