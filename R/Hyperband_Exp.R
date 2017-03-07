#' @title Hyperband Experiment
#'
#' @description
#' Hyperband algorithm experiment function.
#'
#' @param R Resource parameter, the maximum amount of resource that can be allocated to a single hyperparameter configuration.
#' @param R_unit Resource unit, the minimum amount of computation where different hyperparameter configurations start to separate.
#' @param eta an input that controls the proportion of configurations discarded in each round of SuccessiveHalving.
#' @examples
#' Hyperband_Exp(R = 81, R_unit = 1, eta = 3)
#' @importFrom magrittr %>%
#' @export

Hyperband_Exp <- function(R, R_unit, eta = 3) {
  # Preparation
  R_Integer_Ind <- ifelse(class(R_unit) == "integer", TRUE, FALSE)
  # Initialization
  s_max <- floor(log(R, base = eta))
  B <- (s_max + 1) * R
  # Outer Loop
  for (s in s_max:0) {
    n <- ceiling((B * eta^s) / (R * (s + 1)))
    r <- R * eta^(-s)
    cat(paste("Bracket ", s_max - s + 1, ", initializing with ", n, " random configuration(s)", sep = ""), "\n")
    # Inner Loop
    for (i in 0:s) {
      n_i <- floor(n * eta^(-i))
      r_i <- ((r * eta^i) * R_unit) %>% {
        ifelse(R_Integer_Ind, round(.), .)
      }
      k <- max(floor(n_i / eta), 1)
      cat(paste("SuccessiveHalving Round ", i + 1, ", #Configuration = ", n_i, ", #Resource = ", r_i, ", Top_K = ", k, sep = ""), "\n")
    }
  }
}
