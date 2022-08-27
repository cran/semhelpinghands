## ---- include = FALSE-------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
optold <- options(width = 132)

## ---------------------------------------------------------------------------------------------------------------------------------
library(lavaan)
set.seed(1234)
n <- 100
X <- runif(n) - .5
M <- 0.20*X + rnorm(n)
Y <- 0.17*M + rnorm(n)
Data <- data.frame(X = X, Y = Y, M = M)
model <- ' # direct effect
             Y ~ c*X
           # mediator
             M ~ a*X
             Y ~ b*M
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
         '

## ----echo = FALSE-----------------------------------------------------------------------------------------------------------------
fit <- semhelpinghands:::std_boot_ci_v_fit
# std_boot_ci_v_fit <- sem(model,
#             data = Data,
#             se = "bootstrap",
#             bootstrap = 5000,
#             parallel ="snow", ncpus = 8)
# usethis::use_data(std_boot_ci_v_fit,
#                   internal = TRUE,
#                   overwrite = TRUE)

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------
#  fit <- sem(model,
#             data = Data,
#             se = "bootstrap",
#             bootstrap = 5000,
#             parallel ="snow", ncpus = 8)

## ---------------------------------------------------------------------------------------------------------------------------------
standardizedSolution(fit)

## ---------------------------------------------------------------------------------------------------------------------------------
library(semhelpinghands)
ci_boot <- standardizedSolution_boot_ci(fit)
ci_boot

## ---- include = FALSE---------------------------------------------------------
options(optold)

