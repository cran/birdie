## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(birdie)
library(dplyr)

## -----------------------------------------------------------------------------
data(pseudo_vf)

print(pseudo_vf)

## -----------------------------------------------------------------------------
p_xr = prop.table(table(pseudo_vf$turnout, pseudo_vf$race), margin=2)
p_x = prop.table(table(pseudo_vf$turnout))
p_r = prop.table(table(pseudo_vf$race))

## -----------------------------------------------------------------------------
r_probs = bisg(~ nm(last_name) + zip(zip), data=pseudo_vf, p_r=p_r)
print(r_probs)

## -----------------------------------------------------------------------------
r_probs_me = bisg_me(~ nm(last_name) + zip(zip), data=pseudo_vf, p_r=p_r, iter=2000)

## -----------------------------------------------------------------------------
colMeans(r_probs)
colMeans(r_probs_me)
# actual 
p_r

## -----------------------------------------------------------------------------
est_weighted(r_probs, turnout ~ 1, data=pseudo_vf)

## ----messages=FALSE-----------------------------------------------------------
fit = birdie(r_probs, turnout ~ (1 | proc_zip(zip)), data=pseudo_vf, family=cat_mixed())
print(fit)

## -----------------------------------------------------------------------------
tidy(fit)

## -----------------------------------------------------------------------------
coef(fit)
p_xr # Actual

## -----------------------------------------------------------------------------
head(tidy(fit, subgroup=TRUE))

## -----------------------------------------------------------------------------
head(fitted(fit))

plot(r_probs$pr_white, fitted(fit)$pr_white, cex=0.1)

## -----------------------------------------------------------------------------
race_lbl = levels(pseudo_vf$race)
calc_bh_turn = function(race_imp) {
    is_bh = race_lbl[race_imp] %in% c("black", "hisp")
    mean((pseudo_vf$turnout == "yes")[is_bh])
}

est_birdie = simulate(fit, 200) |> # 200 imputations stored as an integer matrix
    apply(2, calc_bh_turn) # calculate turnout for each imputation
hist(est_birdie)

## -----------------------------------------------------------------------------
est_bisg = simulate(r_probs, 200) |> # simulate() works on BISG objects too
    apply(2, calc_bh_turn) 

tibble(
    actual = with(pseudo_vf, mean((turnout == "yes")[race %in% c("black", "hisp")])),
    est_birdie = mean(est_birdie),
    est_bisg = mean(est_bisg),
)

## -----------------------------------------------------------------------------
fit_boot = birdie(r_probs, turnout ~ 1, data=pseudo_vf, algorithm="em_boot", iter=200)

fit_boot$se

