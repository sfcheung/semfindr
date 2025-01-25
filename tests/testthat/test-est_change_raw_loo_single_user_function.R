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

rsq <- function(object) {
    est <- lavaan::parameterEstimates(object,
                                      rsquare = TRUE,
                                      se = FALSE)
    out <- c(m1_r2 = est[(est$lhs == "m1") &
                      (est$op == "r2"), "est"],
             dv_r2 = est[(est$lhs == "dv") &
                      (est$op == "r2"), "est"])
    return(out)
  }
rsq(fit0)

fm <- function(object, what = "chisq") {
    lavaan::fitMeasures(object, what)
  }
fm(fit0)
fm(fit0, what = c("chisq", "tli", "cfi"))

est0 <- lavaan::parameterEstimates(fit0, rsquare =  TRUE)
est0_15 <- lavaan::parameterEstimates(fit0_15, rsquare = TRUE)
est_change_rerun_all <- user_change_raw(rerun_out, rsq)
est_change_rerun_all_chisq <- user_change_raw(rerun_out, fm)
est_change_rerun_all_fm <- user_change_raw(rerun_out, fm, c("chisq", "tli", "cfi"))

(est0_15$est_all <- est0$est)
(est0_15$est_cha <- est0_15$est_all - est0_15$est)

(est_015_chisq_cha <- fitMeasures(fit0, "chisq") - fitMeasures(fit0_15, "chisq"))
(est_015_fm_cha <- fitMeasures(fit0, c("chisq", "tli", "cfi")) - fitMeasures(fit0_15, c("chisq", "tli", "cfi")))

p <- index_plot(est_change_rerun_all,
                column = "dv_r2",
                plot_title = "R-square Change: DV")

test_that("Compare raw changes for an arbitrary case", {
    expect_equal(ignore_attr = TRUE,
        est0_15[10:11, "est_cha"],
        est_change_rerun_all[15, ]
      )
    expect_equal(ignore_attr = TRUE,
        est_015_chisq_cha,
        est_change_rerun_all_chisq[15, ]
      )
    expect_equal(ignore_attr = TRUE,
        est_015_fm_cha,
        est_change_rerun_all_fm[15, ]
      )
    expect_equal(p$data$x,
                 unname(est_change_rerun_all[, "dv_r2"]))
  })

# Scalar with no name

test_that("scalar without names", {
rsq1 <- function(object) {
    est <- lavaan::parameterEstimates(object,
                                      rsquare = TRUE,
                                      se = FALSE)
    out <- c(est[(est$lhs == "m1") &
                      (est$op == "r2"), "est"])
    return(out)
  }
rsq1(fit0)
est_change_rerun_all_1 <- user_change_raw(rerun_out, rsq1)
est_change_rerun_all
expect_equal(est_change_rerun_all_1[, 1],
             est_change_rerun_all[, 1])
})

test_that("vector without names", {
rsq_unnamed <- function(object) {
    est <- lavaan::parameterEstimates(object,
                                      rsquare = TRUE,
                                      se = FALSE)
    out <- c(m1_r2 = est[(est$lhs == "m1") &
                      (est$op == "r2"), "est"],
             dv_r2 = est[(est$lhs == "dv") &
                      (est$op == "r2"), "est"])
    names(out) <- NULL
    return(out)
  }
rsq_unnamed(fit0)
est_change_rerun_all_unnamed <- user_change_raw(rerun_out, rsq_unnamed)
est_change_rerun_all_unnamed
expect_equal(est_change_rerun_all_unnamed[, 1],
             est_change_rerun_all[, 1])
expect_equal(est_change_rerun_all_unnamed[, 2],
             est_change_rerun_all[, 2])
})


test_that("fit not the first argument", {
rsq_non_first <- function(y_names = NULL,
                        dummy_names = NUL,
                        fit = NULL) {
    est <- lavaan::parameterEstimates(object = fit,
                                      rsquare = TRUE,
                                      se = FALSE)
    out <- c(m1_r2 = est[(est$lhs == "m1") &
                      (est$op == "r2"), "est"],
             dv_r2 = est[(est$lhs == "dv") &
                      (est$op == "r2"), "est"])
    return(out)
  }
rsq_non_first(y_names = 1:2, fit = fit0)
est_change_rerun_all_unnamed <- user_change_raw(rerun_out,
                                                rsq_non_first,
                                                fit_name = "fit")
est_change_rerun_all_unnamed
expect_equal(est_change_rerun_all_unnamed[, 1],
             est_change_rerun_all[, 1])
expect_equal(est_change_rerun_all_unnamed[, 2],
             est_change_rerun_all[, 2])
})