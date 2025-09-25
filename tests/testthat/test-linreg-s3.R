# tests/testthat/test-linreg-s3.R

# ---- erroneous input ----
test_that("linreg rejects errounous input", {
  data("iris")

  # misspelled variable name in the formula
  expect_error(linreg(Petal.Length ~ Sepdsal.Width + Sepal.Length, data = iris))

  # non-existent data object
  expect_error(linreg(Petal.Length ~ Sepdsal.Width + Sepal.Length, data = irfsfdis))
})

# ---- class ----
test_that("class is correct", {
  data("iris")
  linreg_mod <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)
  expect_s3_class(linreg_mod, "linreg")
})

# ---- print() ----
test_that("print() works", {
  data("iris")
  linreg_mod <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)

  # Call line
  expect_output(
    print(linreg_mod),
    "linreg\\(formula = Petal\\.Length ~ Sepal\\.Width \\+ Sepal\\.Length, data = iris\\)"
  )

  # Coefficients header/columns present
  expect_output(
    print(linreg_mod),
    "( )*\\(Intercept\\)( )*Sepal\\.Width( )*Sepal\\.Length"
  )
})

# ---- pred() ----
test_that("pred() works", {
  data("iris")
  linreg_mod <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)

  # match exact positions and rounded values from the spec
  expect_equal(
    round(unname(pred(linreg_mod)[c(1, 5, 7)]), 2),
    c(1.85, 1.53, 1.09)
  )
})

# ---- resid() ----
test_that("resid() works", {
  data("iris")
  linreg_mod <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)

  expect_equal(
    round(unname(resid(linreg_mod)[c(7, 13, 27)]), 2),
    c(0.31, -0.58, -0.20)
  )
})

# ---- coef() ----
test_that("coef() works", {
  data("iris")
  linreg_mod <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)

  # coefficients rounded and contained in the expected set (from spec)
  expect_true(
    all(round(unname(coef(linreg_mod)), 2) %in% c(-2.52, -1.34, 1.78))
  )
})

# ---- summary() ----
test_that("summary() works", {
  data("iris")
  linreg_mod <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)

  # regex checks copied from the spec
  expect_output(
    summary(linreg_mod),
    "\\(Intercept\\)( )*-2.5[0-9]*( )*0.5[0-9]*( )*-4.4[0-9]*( )*.*( )*\\*\\*\\*"
  )
  expect_output(
    summary(linreg_mod),
    "Sepal.Width( )*-1.3[0-9]*( )*0.1[0-9]*( )*-10.9[0-9]*( )*.*( )*\\*\\*\\*"
  )
  expect_output(
    summary(linreg_mod),
    "Sepal.Length( )*1.7[0-9]*( )*0.0[0-9]*( )*27.5[0-9]*( )*.*( )*\\*\\*\\*"
  )
  expect_output(
    summary(linreg_mod),
    "Residual standard error: 0.6[0-9]* on 147 degrees of freedom"
  )
})
