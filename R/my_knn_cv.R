#' K-Nearest-Neighbors Cross Validation
#'
#' Performs k-nearest-neighbors cross validation on dataset to predict
#' classifications of the data.
#'
#' @param train Data frame containing data to train the model.
#' @param cl Data frame containing true classification values for the data.
#' @param k_nn Numeric denoting how many nearest neighbors to use in the model.
#' @param k_cv Numeric denoting number of folds to use in cross validation.
#' @keywords prediction
#'
#' @return List containing "class": vector of classification predictions made
#'   by the model, "cv_err": numeric containing the average misclasification
#'   rate from 0 to 1.
#'
#' @examples
#' height <- c(1.87, 1.45, 1.67, 1.82, 1.91, 1.58)
#' weight <- c(210, 140, 165, 185, 205, 130)
#' gender <- c("Male", "Female", "Female", "Male", "Male", "Male")
#' df <- cbind(data.frame(height), data.frame(weight), data.frame(gender))
#' my_knn_cv(df[,1:2], df[,3], 1, 2)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  if (!is.data.frame(train)) {
    stop("Training data must be a data frame")
  } else if (all(!sapply(train, is.numeric))) {
    stop("Training data must be numeric")
  } else if (!is.numeric(k_cv)) {
    stop("Number of folds must be numeric")
  }
  fold <- sample(rep(1:k_cv, length = nrow(train)))
  train_fold <- train %>% dplyr::mutate("split" = fold)

  class <- vector(length = length(train))
  num_incorrect <- 0
  for (i in 1:k_cv) {
    data_train <- train_fold %>% dplyr::filter(split != i)
    data_test <- train_fold %>% dplyr::filter(split == i)

    class[fold == i] <- class::knn(data_train, data_test, cl[fold != i], k_nn)

    num_incorrect <- num_incorrect +
      length(which(suppressWarnings(as.numeric(cl[fold == i])) != class[fold == i]))
  }
  print(class)
  print(cl)

  cv_err <- num_incorrect / nrow(train)
  return(list("class" = class, "cv_err" = cv_err))
}
