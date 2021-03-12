#' Statistics t test
#'
#' This function performs a t test given data and a hypothesis.
#'
#' @param x Vector of numerics representing sample data.
#' @param alternative String containing type of alternative hypothesis with
#'   one of the following values "two.sided", "less", or "greater".
#' @param mu Numeric containing null hypothesis mean value.
#' @keywords inference
#'
#' @return List containing "test_stat": the t statistic calculated, "df": the
#'   degrees of freedom, "alternative": the value of \code{alternative},
#'   "p_val": the p-value calculated from the t test.
#'
#' @examples
#' my_t.test(c(0.5, 0.6, 0, 1.5, 4), "two.sided", 2)
#' my_t.test(c(1.0, 1.0), "less", 0)
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  if (!is.numeric(x)) {
    stop("Sample data must be a numeric vector")
  } else if (!is.numeric(mu)) {
    stop("Null hypothesis mean must be numeric")
  }

  n <- length(x)
  mu_hat <- mean(x)
  se <- stats::sd(x) / sqrt(n)
  test_stat <- (mu_hat - mu) / se
  df <- n - 1

  # Perform different type of t test based on value of `alternative`
  if (alternative == "two.sided") {
    p_val <- 2 * stats::pt(abs(test_stat), df, lower.tail = FALSE)
  } else if (alternative == "less") {
    p_val <- stats::pt(test_stat, df, lower.tail = TRUE)
  } else if (alternative == "greater") {
    p_val <- stats::pt(test_stat, df, lower.tail = FALSE)
  } else {
    stop('Alternative parameter must be "two.sided", "less", or "greater"')
  }

  return(list(test_stat = test_stat, df = df, alternative = alternative,
              p_val = p_val))
}
