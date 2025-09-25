#' Predicted/fitted values for linreg
#' @param object a linreg object
#' @param ... unused; for S3 compatibility
#' @export
pred <- function(object, ...) UseMethod("pred")

#' Print method for linreg
#' @param x a linreg object
#' @param ... unused; for S3 compatibility
#' @export
print.linreg <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  if (!is.null(x$method))
    cat("\nMethod:", toupper(x$method), "\n")
  cat("\nCoefficients:\n")
  print(x$beta_hat)
  invisible(x)
}

#' Residuals for linreg (method for residuals())
#' @param object a linreg object
#' @param ... unused; for S3 compatibility
#' @export
residuals.linreg <- function(object, ...) unname(object$residuals)

#' Residuals for linreg (alias of residuals())
#' @param object a linreg object
#' @param ... unused; for S3 compatibility
#' @export
resid.linreg <- function(object, ...) unname(object$residuals)

#' Predicted/fitted values for linreg (method)
#' @inheritParams pred
#' @export
pred.linreg <- function(object, ...) unname(object$y_hat)

#' Coefficients for linreg
#' @param object a linreg object
#' @param ... unused; for S3 compatibility
#' @export
coef.linreg <- function(object, ...) {
  stats::setNames(object$beta_hat, names(object$beta_hat))
}

#' Summary for linreg
#' @param object a linreg object
#' @param ... unused; for S3 compatibility
#' @return A data.frame with columns: Estimate, Std. Error, t value, Pr(>|t|)
#' @export
summary.linreg <- function(object, ...) {
  out <- data.frame(
    Estimate = object$beta_hat,
    `Std. Error` = object$se,
    `t value` = object$t_values,
    `Pr(>|t|)` = object$p_values,
    check.names = FALSE
  )
  cat("Residual standard error:", sqrt(object$sigma_squared),
      "on", object$df,"degrees of freedom\n")
  if (!is.null(object$method))
    cat("Method:", toupper(object$method), "\n")
  return(out)
}


#' Diagnostic plots for linreg (ggplot2)
#' @param x a linreg object
#' @param ... unused; for S3 compatibility
#' @importFrom rlang .data
#' @export
plot.linreg <- function(x, ...) {
  # data
  stdres <- if (!is.null(x$rstandard)) x$rstandard else x$residuals / sqrt(x$sigma_squared)
  df <- data.frame(
    fitted = x$y_hat,
    resid  = x$residuals,
    stdres = stdres
  )
  df$sc1 <- sqrt(abs(df$stdres))

  # the median value
  med_resid <- stats::median(df$resid, na.rm = TRUE)
  med_sc1   <- stats::median(df$sc1,   na.rm = TRUE)

  # 1) Residuals vs Fitted
  p1 <- ggplot(df, aes(.data$fitted, .data$resid)) +
    geom_point(shape = 1, size = 3.0, stroke = 1.0) +
    geom_hline(yintercept = 0, linetype = "dashed") +         # reference line at 0
    geom_hline(yintercept = med_resid, linetype = "dotted") + # median of residuals
    geom_smooth(se = FALSE, method = "loess", formula = y ~ x, color = "red") +
    labs(
      title = "Residuals vs Fitted",
      x = "Fitted values",
      y = "Residuals"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5)
    )

  # 2) Scale–Location: √|standardized residuals|
  p2 <- ggplot(df, aes(.data$fitted, .data$sc1)) +
    geom_point(shape = 1, size = 3.0, stroke = 1.0) +
    geom_hline(yintercept = med_sc1, linetype = "dotted") +   # median of √|stdres|
    geom_smooth(se = FALSE, method = "loess", formula = y ~ x, color = "red") +
    labs(
      title = "Scale-Location",
      x = "Fitted values",
      y = "sqrt|Standardized residuals|"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5)
    )


  print(p1)
  print(p2)
  invisible(x)
}

