library(testthat)
library(lavaan)
library(semfindr)

# A path model
# fixed.x: TRUE (default)
# Labelled: Some are labelled
# User-defined parameters: At least one

mod <-
'
m1 ~ iv1 + c(a1, a2) * iv2
dv ~ c(b, b) * m1
a2b := a2 * b
'

dat <- pa_dat

dat0 <- dat[1:40, ]
set.seed(856041)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)

fit0 <- lavaan::sem(mod, dat0, group = "gp")
fit0_15 <- lavaan::sem(mod, dat0[-15, ], group = "gp")

rerun_out <- lavaan_rerun(fit0, parallel = FALSE)
rerun_15 <- rerun_out$rerun[[15]]

est0 <- lavaan::parameterEstimates(fit0)
est0_15 <- lavaan::parameterEstimates(fit0_15)

est_change_rerun_all <- est_change(rerun_out)
print(est_change_rerun_all, by = "est")
est_change_rerun_all_paths <- est_change(rerun_out,
                                c("m1 ~ iv1", " m1 ~ iv2 ", "dv ~    m1"))
print(est_change_rerun_all, by = "est")
parameters_names <- gsub(" ", "", c("m1 ~ iv1", " m1 ~ iv2 ", "dv ~    m1"))

est0_15$est_all <- est0$est
est0_15$std_cha <- (est0_15$est_all - est0_15$est)/est0_15$se
est0_15$par_names <- paste0(est0_15$lhs, est0_15$op, est0_15$rhs)
est0_15$coef_name <- est0_15$par_names
est0_15[est0_15$group == 2, "coef_name"] <-
  paste0(est0_15[est0_15$group == 2, "coef_name"], ".g2")
est0_15[est0_15$label == "", "label"] <-
  est0_15[est0_15$label == "", "coef_name"]

parameters_labels <- est0_15$label[est0_15$par_names %in% parameters_names]

est0_15_all_paths <- est0_15[est0_15$par_names %in% parameters_names, "std_cha"]

id_free <- !is.na(est0$z) & est0$op != ":="

est0_free <- est0[id_free, ]
est0_15_free <- est0_15[id_free, ]
k <- nrow(est0_free)
k2 <- length(parameters_names)

est0_15_v <- matrix(est0_free$est - est0_15_free$est, k, 1)
est0_15_v <- est0_15_v[!is.na(est0_free$z)]
est0_15_vcov <- vcov(fit0_15)
class(est0_15_vcov) <- "matrix"
# est0_15_gcd <- t(est0_15_v) %*% solve(est0_15_vcov) %*% est0_15_v
# est0_15_gcd
# est_change_rerun_all[15, "gcd"]

test_that("Compare standardized change for an arbitrary case", {
    expect_equal(ignore_attr = TRUE,
        (est0_free$est - est0_15_free$est)/est0_15_free$se,
        est_change_rerun_all[15, seq_len(k)]
      )
  })

test_that("Compare standardized change for an arbitrary case, with selected parameters", {
    expect_equal(ignore_attr = TRUE,
        est0_15_all_paths,
        est_change_rerun_all_paths[15, parameters_labels]
      )
  })

# test_that("Compare generalized Cook's distance for for an arbitrary case", {
#     expect_equal(ignore_attr = TRUE,
#         as.vector(est0_15_gcd),
#         est_change_rerun_all[15, "gcd"]
#       )
#   })

test_that("User parameters should return error or excluded", {
    expect_error(est_change(rerun_out, "a2b"))
    expect_equal(intersect(colnames(est_change(rerun_out, c("m1 ~ iv1", "a1b"))), "a1b"),
                 character(0))
  })
