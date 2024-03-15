library(testthat)
library(lavaan)

mod <-
'
iv1 ~~ iv2
m1 ~ iv1 + iv2
dv ~ m1
'

dat <- pa_dat

dat <- pa_dat
dat0 <- dat[1:60, ]
set.seed(856041)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)
#dat0[1, 2] <- dat0[2, 3] <- dat0[3, 4] <- dat0[4, 1:4] <- NA
head(dat0)

suppressWarnings(fit0 <- lavaan::sem(mod, dat0, meanstructure = TRUE, group = "gp"))

fit0_data <- lav_data_used(fit0)
head(fit0_data)
case_idx <- lavInspect(fit0, "case.idx", drop.list.single.group = FALSE)
case_idx_full <- unlist(case_idx, use.names = FALSE)

fit0_implied <- implied_scores(fit0, output = "list")
y_names <- colnames(fit0_implied[[1]])
fit0_observed <- lapply(lavInspect(fit0, "data"), function(x) x[, y_names])
fit0_residual <- mapply(function(x1, x2) {x1 - x2},
                        x1 = fit0_implied,
                        x2 = fit0_observed,
                        SIMPLIFY = FALSE)
fit0_resid_md <- lapply(fit0_residual,
                        function(x) {
                            mahalanobis(x,
                                        colMeans(x),
                                        cov(x))
                          })
fit0_resid_md <- unlist(fit0_resid_md, use.names = FALSE)
names(fit0_resid_md) <- case_idx_full

resid_md_ordered <- order(fit0_resid_md, decreasing = TRUE, na.last = NA)

resid_md_top <- 4
resid_md_selected <- resid_md_ordered[seq_len(resid_md_top)]
resid_md_selected <- resid_md_selected[!is.na(resid_md_selected)]
resid_md_selected <- case_idx_full[resid_md_selected]
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
case_id_test <- paste0(sample(letters, nrow(dat0), replace = TRUE),
                       sample(letters, nrow(dat0), replace = TRUE))
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

dat0 <- dat[1:40, ]
dat0[1, 2] <- dat0[2, 3] <- dat0[3, 4] <- dat0[5, ] <- NA
set.seed(856041)
dat0$gp <- sample(c("gp2", "gp1"), size = nrow(dat0), replace = TRUE)

suppressWarnings(fit0 <- lavaan::sem(mod, dat0, group = "gp"))

fit0_data <- lavInspect(fit0, "data")
head(fit0_data[[1]])
head(fit0_data[[2]])

fit0_implied <- implied_scores(fit0, output = "list")
y_names <- colnames(fit0_implied[[1]])
fit0_observed <- lapply(lavInspect(fit0, "data"), function(x) x[, y_names])
fit0_residual <- mapply(function(x1, x2) {x1 - x2},
                        x1 = fit0_implied,
                        x2 = fit0_observed,
                        SIMPLIFY = FALSE)
fit0_resid_md <- lapply(fit0_residual,
                        function(x) {
                            mahalanobis(x,
                                        colMeans(x),
                                        cov(x))
                          })
fit0_resid_md <- unlist(fit0_resid_md, use.names = FALSE)
tmp1 <- lavaan::lavInspect(fit0, "case.idx",
                            drop.list.single.group = FALSE)
tmp2 <- order(unlist(tmp1, use.names = FALSE))
tmp3 <- sort(unlist(tmp1, use.names = FALSE))
fit0_resid_md <- fit0_resid_md[tmp2]
resid_md_ordered <- order(fit0_resid_md, decreasing = TRUE, na.last = NA)
resid_md_ordered <- tmp3[resid_md_ordered]

resid_md_top <- 4
resid_md_selected <- resid_md_ordered[seq_len(resid_md_top)]
resid_md_selected <- resid_md_selected[!is.na(resid_md_selected)]
# Should be: 16 40 26 25
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
