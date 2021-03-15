height <- c(1.87, 1.45, 1.67, 1.82, 1.91, 1.58)
weight <- c(210, 140, 165, 185, 205, 130)
gender <- c("Male", "Female", "Female", "Male", "Male", "Male")
df <- cbind(data.frame(height), data.frame(weight), data.frame(gender))
my_knn_res <- my_knn_cv(df[,1:2], df[,3], 1, 2)

test_that("my_knn_cv 1-nearest-neighbor has zero misclassification rate", {
  expect_equal(my_knn_res$cv_err, 0)
})

test_that("my_knn_cv returns a numeric classification vector", {
  expect_true(is.numeric(my_knn_res$class))
})

my_knn_res_3 <- my_knn_cv(df[,1:2], df[,3], 3, 2)

test_that("my_knn_cv 3-nearest-neighbor has misclassification rate of 0 to 1", {
  expect_true(my_knn_res_3$cv_err >= 0)
  expect_true(my_knn_res_3$cv_err <= 1)
})

test_that("my_knn_cv throws error if training data is not numeric data frame", {
  expect_error(my_knn_cv(7, df[,3], 1, 2))
  expect_error(my_knn_cv(data.frame("a", "b", "c", "d", "e", "f"), df[,3], 1, 2))
})

test_that("my_knn_cv throws error if class data is not a data frame", {
  expect_error(my_knn_cv(df[,1:2], "classification", 1, 2))
})

test_that("my_knn_cv throws error if number of neighbors is not numeric", {
  expect_error(my_knn_cv(df[,1:2], df[,3], "one", 2))
})

test_that("my_knn_cv throws error if number of folds is not numeric", {
  expect_error(my_knn_cv(df[,1:2], df[,3], 1, "two"))
})
