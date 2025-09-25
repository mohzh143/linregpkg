#' Linear regression via QR decomposition (S3)
#' @param formula model formula
#' @param data data.frame
#' @return an object of class 'linreg'
#' @export

linreg <- function(formula, data) {
  # remove entire rows with NA values in any variable used by the formula
  mf <- model.frame(formula = formula, data = data, na.action = na.omit)
  trm <- terms(mf) # metadata about the formula
  X <- model.matrix(trm, mf) # construct the design matrix X from mf
  y <- model.response(mf) # extract the response variable from mf
  stopifnot(is.numeric(y))

  n <- nrow(X) # the sample size n
  p <- ncol(X) # the number of regression parameters, including the intercept

  if (n <= p)
    stop("n must be larger than p.")
  if (qr(X)$rank < p)
    stop("Design matrix is rank-deficient")

  # OLS
  # XtX <- crossprod(X) # t(X) %*% X
  # Xty <- crossprod(X, y) #t(X) %*% y
  # beta_hat <- solve(XtX, Xty) # solve(A, b) solves Ax=b

  # QR beta_hat * R = Q^T * y {1:p}
  QR <- qr(X)
  R <- qr.R(QR)
  QTy <- qr.qty(QR, y) #
  beta_hat <- backsolve(R, QTy[seq_len(p)])

  # (X'X)^-1 = R^{-1} R^{-T}
  Rinv <- backsolve(R, diag(p))
  XtX_inv <- Rinv %*% t(Rinv)

  beta_hat <- as.vector(beta_hat)
  names(beta_hat) <- colnames(X)

  # drop(): returns a plain numeric vector (length n)
  y_hat <- drop(X %*% beta_hat)
  e_hat <- drop(y - y_hat)

  df <- n - p # degrees of freedom

  sigma_squared_hat <- as.numeric(crossprod(e_hat) / df)

  var_beta <- sigma_squared_hat * XtX_inv

  # diag(var_beta): extracts the diagonal elements of the covariance matrix
  se_beta <- sqrt(diag(var_beta))
  t_beta <- beta_hat / se_beta

  # pt(x, df): the Student’s t CDF
  p_values <- 2 * (1 - pt(abs(t_beta), df)) # two-sided p-values

  # diagnostics: hat diagonal & standardized residuals
  hat <- rowSums(X * (X %*% XtX_inv))
  hat <- pmin(pmax(hat, 0), 0.999999)
  rstandard <- e_hat / sqrt(sigma_squared_hat * (1 - hat))


  result <- list(
    beta_hat = beta_hat,
    y_hat = y_hat,
    residuals = drop(e_hat),
    sigma_squared = sigma_squared_hat,
    var_beta = var_beta,
    se = se_beta,
    t_values = t_beta,
    p_values = p_values,
    df = df,
    terms = trm,
    formula = formula,
    call = match.call(),
    method = "qr",
    X = X,
    y = y,
    hat = hat,
    rstandard = rstandard
  )
  class(result) <- "linreg"
  return(result)

}


