library(testthat)
library(lavaan)

mod <-
'
iv1 ~~ iv2
m1 ~ iv1 + iv2
dv ~ m1
'

dat <- pa_dat

dat0 <- dat[1:50, ]
suppressWarnings(fit0 <- lavaan::sem(mod, dat0, meanstructure = TRUE))

fit0_data <- lavInspect(fit0, "data")
colnames(fit0_data) <- lavNames(fit0)
head(fit0_data)

fit0_implied <- implied_scores(fit0)
fit0_observed <- fit0_data[, colnames(fit0_implied)]
fit0_residual <- fit0_implied - fit0_observed
fit0_resid_md <- mahalanobis(fit0_residual, colMeans(fit0_residual),
                                            cov(fit0_residual))

resid_md_ordered <- order(fit0_resid_md, decreasing = TRUE, na.last = NA)

resid_md_top <- 4
resid_md_selected <- resid_md_ordered[seq_len(resid_md_top)]
resid_md_selected <- resid_md_selected[!is.na(resid_md_selected)]
rerun_md_top <- suppressWarnings(lavaan_rerun(fit0, to_rerun = resid_md_selected, parallel = FALSE))
rerun_out <- suppressWarnings(lavaan_rerun(fit0, resid_md_top = 4, parallel = FALSE))

test_that("Check the number of reruns", {
    expect_equal(length(rerun_out$rerun), resid_md_top)
  })

test_that("Check the names of reruns", {
    expect_equal(names(rerun_out$rerun), as.character(resid_md_selected))
  })

test_that("Check selected", {
    expect_equal(rerun_out$selected, resid_md_selected)
  })


# With Case ID

set.seed(80689)
case_id_test <- paste0(sample(letters, 50, replace = TRUE),
                       sample(letters, 50, replace = TRUE))
rerun_out <- suppressWarnings(lavaan_rerun(fit0, case_id = case_id_test, resid_md_top = 4, parallel = FALSE))

test_that("Check the number of reruns", {
    expect_equal(length(rerun_out$rerun), resid_md_top)
  })

test_that("Check the names of reruns", {
    expect_equal(names(rerun_out$rerun), case_id_test[resid_md_selected])
  })


test_that("Check selected", {
    expect_equal(case_id_test[rerun_out$selected], case_id_test[resid_md_selected])
  })

# Listwise

dat <- pa_dat

dat0 <- dat[1:20, ]
dat0[1, 2] <- dat0[2, 3] <- dat0[3, 4] <- dat0[5, ] <- NA
fit0 <- lavaan::sem(mod, dat0)

suppressWarnings(fit0 <- lavaan::sem(mod, dat0, meanstructure = TRUE))

fit0_data <- lavInspect(fit0, "data")
colnames(fit0_data) <- lavNames(fit0)
head(fit0_data)

fit0_implied <- implied_scores(fit0)
fit0_observed <- fit0_data[, colnames(fit0_implied)]
fit0_residual <- fit0_implied - fit0_observed
fit0_resid_md <- mahalanobis(fit0_residual, colMeans(fit0_residual),
                                            cov(fit0_residual))

resid_md_ordered <- order(fit0_resid_md, decreasing = TRUE, na.last = NA)

resid_md_top <- 4
resid_md_selected <- resid_md_ordered[seq_len(resid_md_top)]
resid_md_selected <- resid_md_selected[!is.na(resid_md_selected)]
resid_md_selected <- as.numeric(names(fit0_resid_md)[resid_md_selected])
rerun_md_top <- suppressWarnings(lavaan_rerun(fit0, to_rerun = resid_md_selected, parallel = FALSE))
rerun_out <- suppressWarnings(lavaan_rerun(fit0, resid_md_top = 4, parallel = FALSE))

test_that("Check the number of reruns", {
    expect_equal(length(rerun_out$rerun), resid_md_top)
  })

test_that("Check the names of reruns", {
    expect_equal(names(rerun_out$rerun), as.character(resid_md_selected))
  })

test_that("Check selected", {
    expect_equal(rerun_out$selected, resid_md_selected)
  })
