#' @title Hyperband algorithm
#'
#' @description
#' Hyperband algorithm for Hyperparameter Optimization
#'
#' @param FUN The function to be optimized. This Function should return a numeric value of validation performance,
#'     and the first argument of this function must indicate the resource.
#' @param maximize When it is TRUE, it means the larger the evaluation score the better.
#' @param bounds A named list of lower and upper bounds for each hyperparameter. The names of the list
#'     should be identical to the rest arguments of FUN.
#' @param R Resource parameter, the maximum amount of resource that can be allocated to a single hyperparameter configuration.
#' @param R_unit Resource unit, the minimum amount of computation where different hyperparameter configurations start to separate.
#'     The user can set unit as integer to force integer number of resources being allocated.
#' @param eta an input that controls the proportion of configurations discarded in each round of SuccessiveHalving.
#' @param verbose boolean, print the statistics during the process
#' @references Lisha Li, Kevin Jamieson, Giulia DeSalvo, Afshin Rostamizadeh, Ameet Talwalkar (2016) \emph{Hyperband: A Novel Bandit-Based Approach to Hyperparameter Optimization}
#' @examples
#' # Example 1: Optimization
#' Test_Fun <- function(r, x) {
#'   exp(-(x - 2)^2) + exp(-(x - 6)^2/10) + 1/ (x^2 + 1) + r * 10e-10
#' }
#' OPT_Res <- Hyperband(Test_Fun, maximize = TRUE, bounds = list(x = c(-50, +50)),
#'                      R = 81L, R_unit = 10L, eta = 3, verbose = TRUE)
#' \dontrun{
#' # Example 2: Parameter Tuning
#' library(xgboost)
#' data(agaricus.train, package = 'xgboost')
#' dtrain <- xgb.DMatrix(agaricus.train$data,
#'                       label = agaricus.train$label)
#' XGB_CV_FUN <- function(nrounds, lambda, lambda_bias, alpha) {
#'   XGB_CV <- xgb.cv(params = list(booster = "gblinear", eta = 0.1,
#'                                  lambda = lambda, lambda_bias = lambda_bias, alpha = alpha,
#'                                  objective = "binary:logistic", eval_metric = "logloss"),
#'                    data = dtrain, nrounds = nrounds, nfold = 5, verbose = 1,
#'                    callbacks = list(cb.early.stop(stopping_rounds = 10,
#'                                                   maximize = FALSE,
#'                                                   metric_name = "test-logloss"),
#'                                     cb.cv.predict(save_models = FALSE)))
#'   min(XGB_CV$evaluation_log$test_logloss_mean)
#' }
#' OPT_Res <- Hyperband(XGB_CV_FUN, maximize = FALSE,
#'                      bounds = list(lambda = c(0, 5),lambda_bias = c(0L, 10L),alpha = c(0, 5)),
#'                      R = 1000L, R_unit = 1L, eta = 3, verbose = TRUE)
#' }
#' @importFrom data.table data.table as.data.table set setnames
#' @importFrom foreach foreach %do%
#' @importFrom magrittr %>% %T>% extract extract2
#' @export

Hyperband <- function(FUN, maximize, bounds, R, R_unit, eta = 3, verbose = FALSE) {
  # Check
  stopifnot(all(names(formals(FUN))[-1] == names(bounds)))
  # Preparation
  R_Param <- names(formals(FUN))[1]
  R_Integer_Ind <- ifelse(class(R_unit) == "integer", TRUE, FALSE)
  DT_Bounds <- data.table(Parameter = names(bounds),
                          Lower = sapply(bounds, extract2, 1),
                          Upper = sapply(bounds, extract2, 2),
                          Type = sapply(bounds, class))
  DT_History <- lapply(seq_len(nrow(DT_Bounds) + 2), function(x) numeric(0)) %>%
    as.data.table(.) %>%
    setnames(., old = names(.), new = c(R_Param, DT_Bounds$Parameter, "Value"))
  # Initialization
  s_max <- floor(log(R, base = eta))
  B <- (s_max + 1) * R
  # Outer Loop
  for (s in s_max:0) {
    n <- ceiling((B * eta^s) / (R * (s + 1)))
    r <- R * eta^(-s)
    This_OuteR_Params <- DT_runif(n = n, name = DT_Bounds$Parameter,
                                  lower = DT_Bounds$Lower,
                                  upper = DT_Bounds$Upper) %T>% {
                                    if (any(DT_Bounds[, Type] == "integer")) {
                                      set(.,
                                          j = DT_Bounds[Type == "integer", Parameter],
                                          value = round(extract(., j = DT_Bounds[Type == "integer", Parameter], with = FALSE)))
                                    } else {
                                      .
                                    }
                                  }
    if (verbose == TRUE) {
      cat(paste("Bracket ", s_max - s + 1, ", initializing with ", n, " random configuration(s)", sep = ""), "\n")
    }
    # Inner Loop
    for (i in 0:s) {
      n_i <- floor(n * eta^(-i))
      r_i <- ((r * eta^i) * R_unit) %>% {
        ifelse(R_Integer_Ind, round(.), .)
      }
      k <- max(floor(n_i / eta), 1)
      This_InneR_Params <- cbind("Resource" = r_i,
                                 This_OuteR_Params) %T>%
        setnames(., old = "Resource", new = R_Param)
      if (verbose == TRUE) {
        cat(paste("SuccessiveHalving Round ", i + 1, ", #Configuration = ", n_i, ", #Resource = ", r_i, ", Top_K = ", k, sep = ""), "\n")
      }
      # Evaluation
      This_Inner_Values <- foreach(j = seq_len(nrow(This_InneR_Params)), .combine = "c") %do% {
        This_Log <- utils::capture.output({
          This_Time <- system.time({
            This_Value <- do.call(what = FUN, args = This_InneR_Params[j, ])
          }, gcFirst = FALSE)
        })
        if (verbose == TRUE) {
          paste(c("elapsed", names(This_OuteR_Params), "Value"),
                c(format(This_Time["elapsed"], trim = FALSE, digits = 0, nsmall = 2),
                  format(This_OuteR_Params[j, ], trim = FALSE, digits = 0, nsmall = 2),
                  format(This_Value, trim = FALSE, digits = 0, nsmall = 4)),
                sep = " = ", collapse = "\t") %>%
            cat(., "\n")
        }
        This_Value
      }
      This_InneR_Params_Values <- cbind(This_InneR_Params, "Value" = This_Inner_Values)
      DT_History <- rbind(DT_History, This_InneR_Params_Values)
      This_OuteR_Params <- DT_Top_K(This_InneR_Params_Values, Value_Var = "Value", K = k, maximize = maximize) %>%
        extract(., j = DT_Bounds$Parameter, with = FALSE)
    }
  }
  # Result
  DT_History[order(get("Value"), decreasing = maximize), ]
}
utils::globalVariables(c("Parameter", "Type", "j"))
