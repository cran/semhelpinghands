## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(semhelpinghands)
library(lavaan)

## -----------------------------------------------------------------------------
data(dvs_ivs)
mod <-
"
y1 ~ x1 + x2 + x3
y2 ~ x1 + x3
y3 ~ y2 + x2
"
fit <- sem(model = mod, data = dvs_ivs,
           fixed.x = FALSE)

## -----------------------------------------------------------------------------
est <- parameterEstimates(fit)
est

## -----------------------------------------------------------------------------
fit_gp <- sem(model = mod, data = dvs_ivs,
              group = "gp",
              fixed.x = FALSE)

## -----------------------------------------------------------------------------
est_gp <- parameterEstimates(fit_gp)
est_gp

## -----------------------------------------------------------------------------
add_sig(est)

## -----------------------------------------------------------------------------
add_sig(est, use = c("pvalue", "ci"))

## -----------------------------------------------------------------------------
std <- standardizedSolution(fit)
add_sig(std)

## -----------------------------------------------------------------------------
filter_by(est, op = "~")

## -----------------------------------------------------------------------------
filter_by(est_gp, op = "~", group = "gp1", fit = fit_gp)

## -----------------------------------------------------------------------------
group_by_dvs(est)
group_by_ivs(est)

## -----------------------------------------------------------------------------
group_by_dvs(est, col_name = "pvalue")
group_by_ivs(est, col_name = "pvalue")

## -----------------------------------------------------------------------------
group_by_groups(est_gp)

## -----------------------------------------------------------------------------
group_by_groups(est_gp,
                col_names = c("est", "pvalue"))

## -----------------------------------------------------------------------------
group_by_groups(fit_gp,
                col_names = c("est", "pvalue"))

## -----------------------------------------------------------------------------
mod2 <-
"
y1 ~ x1 + x2 + x3
y2 ~ x1 + x2
y3 ~ y2 + x1
"
fit2 <- sem(model = mod2, data = dvs_ivs,
           fixed.x = FALSE)
est2 <- parameterEstimates(fit2)
est2

## -----------------------------------------------------------------------------
group_by_models(list(Model1 = est,
                     Model2 = est2))

## -----------------------------------------------------------------------------
group_by_models(list(Model1 = est,
                     Model2 = est2),
                col_names = c("est", "pvalue"))

## -----------------------------------------------------------------------------
out <- group_by_groups(est_gp,
                       col_names = c("est", "pvalue"))
out <- filter_by(out, op = c("~", "~~"))
sort_by(out, by = c("op", "rhs"))

## -----------------------------------------------------------------------------
est_gp |>
  add_sig() |>
  group_by_groups(col_names = c("est", "pvalue", "sig"),
                  group_first = FALSE) |>
  filter_by(op = c("~"))

## -----------------------------------------------------------------------------
out <- compare_estimators(fit,
         estimator = c("ML", "GLS", "MLR"))
group_by_models(out, col_names = c("se", "pvalue"))

## -----------------------------------------------------------------------------
se_ratios(out, reference = "ML")

## -----------------------------------------------------------------------------
data(dvs_ivs)
mod <-
"
y1 ~ x1 + x2 + x3
y2 ~ x1 + x3
y3 ~ y2 + x2
"
fit_default <- sem(model = mod, data = dvs_ivs)
show_more_options(fit_default)
fit_MLR <- sem(model = mod, data = dvs_ivs, estimator = "MLR")
show_more_options(fit_MLR)
fit_MLR_fiml <- sem(model = mod, data = dvs_ivs, estimator = "MLR",
               missing = "fiml")
show_more_options(fit_MLR_fiml)

