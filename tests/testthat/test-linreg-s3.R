test_that("linreg rejects erroneous input", {
  data(iris)

  # misspelled variable in the formula
  expect_error(linreg(Petal.Length ~ Sepdsal.Width + Sepal.Length, data = iris))

  # non-existent data object
  expect_error(linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = irfsfdis))
})

test_that("class is correct", {
  data(iris)
  fit <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
  expect_s3_class(fit, "linreg")
})

test_that("coef/pred/resid match lm()", {
  data(iris)
  f_linreg <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
  f_lm     <- lm    (Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)

  # coefficients, fitted values, and residuals are consistent with lm() (numerical tolerance allowed)
  expect_equal(unname(coef(f_linreg)), unname(coef(f_lm)), tolerance = 1e-6)
  expect_equal(pred(f_linreg),         unname(fitted(f_lm)), tolerance = 1e-6)
  expect_equal(resid(f_linreg),        unname(residuals(f_lm)), tolerance = 1e-6)

  # residual standard deviation (sigma)
  expect_equal(sqrt(f_linreg$sigma_squared), summary(f_lm)$sigma, tolerance = 1e-6)
})

test_that("print() contains Call and Coefficients", {
  data(iris)
  fit <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)

  # print output should not error and must contain Call and Coefficients
  expect_output(print(fit), "Call:")
  expect_output(print(fit), "Coefficients:")
  # allow inclusion of Method: QR
  expect_output(print(fit), "Method:\\s*QR")
})

test_that("summary() returns coefficient table similar to lm()", {
  data(iris)
  f_linreg <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
  f_lm     <- lm    (Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)

  s_linreg <- summary(f_linreg)           # data.frame (Estimate, Std. Error, t value, Pr(>|t|))
  s_lm     <- coef(summary(f_lm))         # matrix (Estimate, Std. Error, t value, Pr(>|t|))

  # align by row names and compare values
  s_linreg <- s_linreg[rownames(s_lm), ]
  expect_equal(s_linreg$Estimate,   unname(s_lm[, "Estimate"]),   tolerance = 1e-6)
  expect_equal(s_linreg$`Std. Error`, unname(s_lm[, "Std. Error"]), tolerance = 1e-6)
  expect_equal(s_linreg$`t value`,  unname(s_lm[, "t value"]),    tolerance = 1e-6)
  expect_equal(s_linreg$`Pr(>|t|)`, unname(s_lm[, "Pr(>|t|)"]),   tolerance = 1e-6)

  # print output must include residual standard error and degrees of freedom
  expect_output(summary(f_linreg), "Residual standard error:\\s*[0-9.]+\\s*on\\s*[0-9]+\\s*degrees of freedom")
})

test_that("plot() runs silently", {
  data(iris)
  fit <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
  expect_silent(plot(fit))
})

test_that("n <= p triggers error", {
  set.seed(1)
  df <- data.frame(y = rnorm(3), x1 = rnorm(3), x2 = rnorm(3), x3 = rnorm(3))
  expect_error(linreg(y ~ x1 + x2 + x3, df), "n must be larger than p")
})

test_that("rank-deficient design triggers error", {
  set.seed(1)
  df <- data.frame(y = rnorm(10), x1 = rnorm(10))
  df$x2 <- df$x1  # perfect collinearity
  expect_error(linreg(y ~ x1 + x2, df), "rank-deficient")
})

test_that("NA rows are removed consistently (na.omit behavior)", {
  df <- datasets::mtcars
  df$mpg[1] <- NA
  fit <- linreg(mpg ~ cyl + hp, df)
  n_complete <- nrow(stats::na.omit(df[, c("mpg","cyl","hp")]))
  expect_equal(length(resid(fit)), n_complete)
})
