linregpkg
================

# linregpkg

linregpkg is an R package created for Lab 3 at Linköping University. An
S3 implementation of linear regression via QR decomposition.

<!-- Badges here -->

<figure>
<img
src="https://github.com/mohzh143/linregpkg/actions/workflows/R-CMD-check.yaml/badge.svg"
alt="R-CMD-check" />
<figcaption aria-hidden="true">R-CMD-check</figcaption>
</figure>

## Installation

``` r
# install.packages("devtools")
devtools::install_github("mohzh143/linregpkg")
```

## Example

``` r
library(linregpkg)

# Fit model on iris dataset
fit <- linreg(Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)

# Methods
print(fit)
coef(fit)
head(pred(fit))
head(resid(fit))
summary(fit)
plot(fit)
```
