#' Replicated Cross-Validation for glmnet
#'
#' @param replications number of CV replications. Can be restricted to be less than
#'   `getOption("max.cv.replications")`, which is 10 by default. If ≤ 0, no restrictions
#'   apply.
#' @inheritParams cv.glmnet
#'
#' @returns the same object as [cv.glmnet()].
#'   If `keep=TRUE`, the `fit.preval` object is a list with one item per penalization
#'   level. Each item contains a matrix of predicted values, with observations on the
#'   rows and the replications in the columns.
replicated_cv_glmnet <- function (x, y, replications, weights = NULL, offset = NULL, lambda = NULL,
                                  type.measure = c("default", "mse", "deviance", "class", "auc",
                                                   "mae", "C"),
                                  family = c("gaussian", "binomial", "poisson",
                                             "multinomial", "cox", "mgaussian"),
                                  nfolds = 10, foldid = NULL, alignment = c("lambda", "fraction"),
                                  grouped = TRUE, keep = FALSE, parallel = FALSE,
                                  gamma = c(0, 0.25, 0.5, 0.75, 1), relax = FALSE,
                                  trace.it = 0,  ...) {
  requireNamespace('glmnet')
  if (!is.numeric(replications) ||
      length(replications) != 1L ||
      any(replications < 3)) {
    stop("Argument `replications` must be an integer greater than 2.")
  }
  
  type.measure <- match.arg(type.measure)
  family <- match.arg(family)
  max_replications <- getOption("max.cv.replications", default = 10)
  if (!is.numeric(max_replications) || max_replications <= 0) {
    max_replications <- Inf
  }
  
  if (replications > max_replications) {
    warning(sprintf("Replications are limited to %d by system policy.", max_replications))
    replications <- max_replications
  }
  
  call <- match.call()
  call[[1]] <- glmnet::cv.glmnet
  call$replications <- NULL
  
  first_fit <- eval(call)
  first_fit$call <- match.call()
  keep <- isTRUE(keep)
  
  call$lambda <- first_fit$lambda
  call$nlambda <- NULL
  
  if (keep) {
    preds <- apply(first_fit$fit.preval, 2, \ (x) {
      m <- matrix(NA_real_, nrow = length(x), ncol = replications)
      m[, 1] <- x
      m
    }, simplify = FALSE)
    
    repl_cvm <- matrix(NA_real_, nrow = length(first_fit$lambda), ncol = replications)
    repl_cvm[, 1] <- first_fit$cvm
    
    for (r in 1L + seq_len(replications - 1)) {
      fit <- eval(call)
      repl_cvm[, r] <- fit$cvm
      for (l in seq_along(fit$lambda)) {
        preds[[l]][, r] <- fit$fit.preval[, l]
      }
    }
    
    first_fit$fit.preval <- preds
  } else {
    repl_cvm <- vapply(seq_len(replications - 1), FUN.VALUE = first_fit$cvm,
                       FUN = function (.) {
                         fit <- eval(call)
                         fit$cvm
                       })
    
    repl_cvm <- cbind(repl_cvm, first_fit$cvm)
  }
  
  if (identical(type.measure, "mse") ||
      (identical(family, "gaussian") && identical(type.measure, "default"))) {
    repl_cvm <- sqrt(repl_cvm)
    first_fit$name[[1]] <- "RMSPE"
  }
  
  first_fit$cvm <- rowMeans(repl_cvm)
  first_fit$cvsd <- apply(repl_cvm, 1, sd)
  first_fit$cvup <- first_fit$cvm + first_fit$cvsd
  first_fit$cvlo <- first_fit$cvm - first_fit$cvsd
  
  sign <- if (identical(type.measure, "auc")) {
    -1
  } else {
    +1
  }
  
  which_min <- which.min(sign * first_fit$cvm)
  first_fit$lambda.min <- first_fit$lambda[[which_min]]
  one_se_lambda <- with(first_fit,
                        which(lambda >= lambda.min &
                                sign * cvm <= max(sign * cvup[[which_min]],
                                                  sign * cvlo[[which_min]])))
  if (length(one_se_lambda) > 0) {
    first_fit$lambda.1se <- max(first_fit$lambda[one_se_lambda])
  } else {
    first_fit$lambda.1se <- max(first_fit$lambda)
  }
  first_fit
}
