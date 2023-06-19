skip("To be tested in an interactive session")
library(testthat)
library(lavaan)
library(semfindr)

# A path model
# fixed.x: TRUE (default)
# Labelled: Some are labelled
# User-defined parameters: At least one

mod <-
'
m1 ~ iv1 + a2 * iv2
dv ~ b * m1
a1b := a2 * b
'

dat <- pa_dat

dat0 <- dat[1:25, ]
fit0 <- lavaan::sem(mod, dat0)
fit0_15 <- lavaan::sem(mod, dat0[-15, ])

rerun_out <- lavaan_rerun(fit0, parallel = FALSE)
rerun_15 <- rerun_out$rerun[[15]]


est_change_raw_all <- est_change_raw(rerun_out)
est_change_raw_all_std <- est_change_raw(rerun_out, standardized = TRUE)
est_change_raw_all_paths <- est_change_raw(rerun_out,
                              c("m1 ~ iv1", " m1 ~ iv2 ", "dv ~    m1"))
est_change_raw_all_paths_std <- est_change_raw(rerun_out,
                                c("m1 ~ iv1", " m1 ~ iv2 ", "dv ~    m1"),
                                standardized = TRUE)

est_change_raw_approx_op1 <- est_change_raw_approx(fit0, c("~"))
est_change_raw_approx_op2 <- est_change_raw_approx(fit0, c("~~"))
est_change_raw_approx_mixed <- est_change_raw_approx(fit0, c("m1 ~ iv1", "~~"))

est_change_all <- est_change(rerun_out)
est_change_all_paths <- est_change(rerun_out,
                                c("m1 ~ iv1", " m1 ~ iv2 ", "dv ~    m1"))

est_change_approx_op1 <- est_change_approx(fit0, c("~"))
est_change_approx_op2 <- est_change_approx(fit0, c("~~"))
est_change_approx_mixed <- est_change_approx(fit0, c("m1 ~ iv1", "~~"))

print(est_change_raw_all)
print(est_change_raw_all, first = NULL)
print(est_change_raw_all, first = 10)
print(est_change_raw_all, first = 10, sort_by = NULL)
print(est_change_raw_all_std, first = 6)
print(est_change_raw_all_paths, first = 1)
print(est_change_raw_all_paths_std, first = 3)

print(est_change_raw_approx_op1)
print(est_change_raw_approx_op2, first = 5)
print(est_change_raw_approx_mixed, first = 6, sort_by = NULL)

print(est_change_all)
print(est_change_all_paths, first = 3)
print(est_change_all, sort_by = "est")

print(est_change_approx_op1)
print(est_change_approx_op2)
print(est_change_approx_mixed)

tmp <- est_change_all
tmp[c(1, 3, 16, 22, 20), "gcd"] <- NA
head(tmp)
print(tmp)
print(tmp, first = NULL)
print(est_change_all)
