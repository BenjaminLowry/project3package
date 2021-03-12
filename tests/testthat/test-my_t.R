#
# Two-sided Tests
#
two_sided_test <- my_t.test(c(0.5, 1.0, 1.5, 2.0), "two.sided", 1.0)
two_sides_expected <- t.test(c(0.5, 1.0, 1.5, 2.0), alternative = "two.sided",
                             mu = 1.0)

test_that("my_t.test returns correct two-sided t test test stat", {
  expect_equal(two_sided_test$test_stat, two_sides_expected$statistic[["t"]])
})

test_that("my_t.test returns correct two-sided t test df", {
  expect_equal(two_sided_test$df, two_sides_expected$parameter[["df"]])
})

test_that("my_t.test returns correct two-sided t test alternative", {
  expect_match(two_sided_test$alternative, two_sides_expected$alternative)
})

test_that("my_t.test returns correct two-sided t test p value", {
  expect_equal(two_sided_test$p_val, two_sides_expected$p.value)
})

#
# One-sided Tests (Alternative: Less)
#
one_sided_test_less <- my_t.test(c(1.7, 2.1, 4.8, 3.2, 4.5), "less", 2.2)
one_sided_test_less_expected <- t.test(c(1.7, 2.1, 4.8, 3.2, 4.5),
                                       alternative = "less", mu = 2.2)

test_that("my_t.test returns correct two-sided t test test stat", {
  expect_equal(one_sided_test_less$test_stat,
               one_sided_test_less_expected$statistic[["t"]])
})

test_that("my_t.test returns correct two-sided t test df", {
  expect_equal(one_sided_test_less$df,
               one_sided_test_less_expected$parameter[["df"]])
})

test_that("my_t.test returns correct two-sided t test alternative", {
  expect_match(one_sided_test_less$alternative,
               one_sided_test_less_expected$alternative)
})

test_that("my_t.test returns correct two-sided t test p value", {
  expect_equal(one_sided_test_less$p_val, one_sided_test_less_expected$p.value)
})

#
# One-sided Tests (Alternative: Greater)
#
one_sided_test_greater <- my_t.test(c(1.7, 4.1, 4.8, 5.2, 2.5), "greater", 3.7)
one_sided_test_greater_expected <- t.test(c(1.7, 4.1, 4.8, 5.2, 2.5),
                                          alternative = "greater", mu = 3.7)


test_that("my_t.test returns correct two-sided t test test stat", {
  expect_equal(one_sided_test_greater$test_stat,
               one_sided_test_greater_expected$statistic[["t"]])
})

test_that("my_t.test returns correct two-sided t test df", {
  expect_equal(one_sided_test_greater$df,
               one_sided_test_greater_expected$parameter[["df"]])
})

test_that("my_t.test returns correct two-sided t test alternative", {
  expect_match(one_sided_test_greater$alternative,
               one_sided_test_greater_expected$alternative)
})

test_that("my_t.test returns correct two-sided t test p value", {
  expect_equal(one_sided_test_greater$p_val,
               one_sided_test_greater_expected$p.value)
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
