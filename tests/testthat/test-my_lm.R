x <- c(0.5, 0.8, 1.5, 0.5, 0.2, 3.5)
y <- c(1.0, 1.2, 0.1, 0.2, 1.4, 7.2)
z <- c(3.0, 2.4, 1.8, 0.2, 2.3, 2.6)
test_df <- cbind(data.frame(x), data.frame(y), data.frame(z))
my_lm_test <- my_lm(z ~ x*y, test_df)
lm_expected <- summary(lm(z ~ x*y, test_df))

test_that("my_lm calculates correct variable coefficient estimates", {
  expect_equal(my_lm_test$Estimate, unname(lm_expected$coefficients[,1]))
})

test_that("my_lm calculates correct variable coefficient std error", {
  expect_equal(my_lm_test$`Std. Error`, unname(lm_expected$coefficients[,2]))
})

test_that("my_lm calculates correct variable t statistics", {
  expect_equal(my_lm_test$`t value`, unname(lm_expected$coefficients[,3]))
})

test_that("my_lm calculates correct variable p value", {
  expect_equal(my_lm_test$`Pr(>|t|)`, unname(lm_expected$coefficients[,4]))
})

test_that("my_lm throws error if invalid formula is provided", {
  expect_error(my_lm("formula", test_df))
})

test_that("my_lm throws error if invalid data is provided", {
  expect_error(my_lm(z ~ x*y, 3))
})
