#' Linear fit generator
#'
#' Performs a linear fit on data given a formula.
#'
#' @param formula Formula class object to fit \code{data} to.
#' @param data Data frame that will be fit onto \code{formula}.
#' @keywords inference
#'
#' @return Table of coefficients generated through the linear fit. Contains the
#'   estimates of the coefficients of the variables provided in the formula
#'   (and the intercept), the standard error of these estimates, the
#'   corresponding t values, and the p values of these t values for each
#'   coefficient.
#'
#' @examples
#' x <- c(0.5, 0.8, 1.5, 0.5, 0.2)
#' y <- c(1.0, 1.2, 0.1, 0.2, 1.4)
#' df <- cbind(data.frame(y), data.frame(x))
#' my_lm(y ~ x, df)
#'
#' @export
my_lm <- function(formula, data) {
  X <- stats::model.matrix(formula, data)
  model_frame <- stats::model.frame(formula, data)
  Y <- stats::model.response(model_frame)

  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
  df <- nrow(data) - (length(stats::terms(formula)) + 1)

  # Sigma hat squared
  shs <- sum(((Y - X %*% beta_hat) ^ 2) / df)

  # Standard error
  se <- sqrt(diag(shs * solve(t(X) %*% X)))

  t <- beta_hat / se

  # Pr(>|t|)
  pr_gt_abs_t <- 2 * stats::pt(abs(t), df, lower.tail = FALSE)

  coeff_data <- cbind(beta_hat, se, t, pr_gt_abs_t)
  coeff_table <- data.frame(coeff_data)
  colnames(coeff_table) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  return(coeff_table)
}
