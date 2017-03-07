#' @title data.table runif
#'
#' @description
#' Generates random variables for multiple hyperparameters in data.table format.
#'
#' @param n number of observations
#' @param name variable names
#' @param lower lower bounds
#' @param upper upper bounds
#' @return a data.table of hyperparameters
#' @importFrom stats runif
#' @importFrom data.table data.table setnames
#' @importFrom foreach foreach %do%
#' @importFrom magrittr %>% %T>%
#' @keywords internal
#' @export

DT_runif <- function(n, name, lower, upper) {
  foreach(i = seq_along(lower), .combine = "cbind") %do% {
    runif(n, min = lower[i], max = upper[i]) %>%
      pmin(., upper[i] - sqrt(.Machine$double.eps)) %>%
      pmax(., lower[i] + sqrt(.Machine$double.eps))
  } %>%
    data.table(.) %T>%
    setnames(., old = names(.), new = name)
}
utils::globalVariables(c("i", "."))

#' @title data.table Top_K
#'
#' @description
#' Select Top K rows from data.table based on certain variable.
#'
#' @param DT data.table
#' @param Value_Var variable for selecting Top K row(s)
#' @param K number of K
#' @param maximize maximizing indicator
#' @return a data.table of hyperparameters
#' @importFrom magrittr %>% extract
#' @keywords internal
#' @export

DT_Top_K <- function(DT, Value_Var, K, maximize) {
  DT %>%
    extract(., i = order(get(Value_Var), decreasing = maximize)) %>%
    extract(., i = seq_len(K))
}
utils::globalVariables(c("."))
