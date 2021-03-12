#
# Two-sided Tests
#
two_sided_test <- my_t.test(c(0.5, 1.0, 1.5, 2.0), "two.sided", 1.0)

test_that("my_t.test returns correct two-sided t test test stat", {
  expect_true(abs(two_sided_test$test_stat - 0.7746) < 0.001)
})

test_that("my_t.test returns correct two-sided t test df", {
  expect_equal(two_sided_test$df, 3)
})

test_that("my_t.test returns correct two-sided t test alternative", {
  expect_match(two_sided_test$alternative, "two.sided")
})

test_that("my_t.test returns correct two-sided t test p value", {
  expect_true(abs(two_sided_test$p_val - 0.4950) < 0.001)
})

#
# One-sided Tests (Alternative: Less)
#
one_sided_test_less <- my_t.test(c(1.7, 2.1, 4.8, 3.2, 4.5), "less", 2.2)

test_that("my_t.test returns correct two-sided t test test stat", {
  expect_true(abs(one_sided_test_less$test_stat - 1.7092) < 0.001)
})

test_that("my_t.test returns correct two-sided t test df", {
  expect_equal(one_sided_test_less$df, 4)
})

test_that("my_t.test returns correct two-sided t test alternative", {
  expect_match(one_sided_test_less$alternative, "less")
})

test_that("my_t.test returns correct two-sided t test p value", {
  expect_true(abs(one_sided_test_less$p_val - 0.9187) < 0.001)
})

#
# One-sided Tests (Alternative: Greater)
#
one_sided_test_less <- my_t.test(c(1.7, 4.1, 4.8, 5.2, 2.5), "greater", 3.7)

test_that("my_t.test returns correct two-sided t test test stat", {
  expect_true(abs(one_sided_test_less$test_stat - (-0.0595)) < 0.001)
})

test_that("my_t.test returns correct two-sided t test df", {
  expect_equal(one_sided_test_less$df, 4)
})

test_that("my_t.test returns correct two-sided t test alternative", {
  expect_match(one_sided_test_less$alternative, "greater")
})

test_that("my_t.test returns correct two-sided t test p value", {
  expect_true(abs(one_sided_test_less$p_val - 0.5223) < 0.001)
})

#
# Improper Input Tests
#
test_that("my_t.test throws error without numeric vector of sample data", {
  expect_error(my_t.test("a vector", "less", 4.0))
})

test_that("my_t.test throws error with invalidate alternative string", {
  expect_error(my_t.test(c(2.0, 4.0, 2.0, 3.0), "one.sided", 4.0))
})

test_that("my_t.test throws error with non-numeric null hypothesis mean", {
  expect_error(my_t.test(c(2.0, 4.0, 2.0, 3.0), "less", "4.0"))
})
