skip_if_not_installed("modi")
library(testthat)
library(lavaan)

# Can handle missing data

mod <-
'
iv1 ~~ iv2
m1 ~ iv1 + iv2
dv ~ m1
'

dat <- pa_dat

dat0 <- dat[1:50, ]
dat0[1, 2] <- dat0[2, 3] <- dat0[3, 4] <- dat0[4, ] <- NA
head(dat0)
suppressWarnings(fit0 <- lavaan::sem(mod, dat0, missing = "fiml.x"))

fit0_data <- lavInspect(fit0, "data")
colnames(fit0_data) <- lavNames(fit0)
head(fit0_data)

md_fit0 <- mahalanobis_rerun(fit0)
md_ordered <- order(md_fit0, decreasing = TRUE, na.last = NA)

md_top <- 4
md_selected <- md_ordered[seq_len(md_top)]
md_selected <- md_selected[!is.na(md_selected)]
rerun_md_top <- suppressWarnings(lavaan_rerun(fit0, to_rerun = md_selected, parallel = FALSE))
rerun_out <- suppressWarnings(lavaan_rerun(fit0, md_top = 4, parallel = FALSE))

test_that("Check the number of reruns", {
    expect_equal(length(rerun_out$rerun), md_top)
  })

test_that("Check the names of reruns", {
    expect_equal(names(rerun_out$rerun), as.character(md_selected))
  })

test_that("Check selected", {
    expect_equal(rerun_out$selected, md_selected)
  })

# For listwise

fit0_case_ids <- lavInspect(fit0, "case.idx")
test_that("Check case ids", {
    expect_equal(sort(as.numeric(rownames(md_fit0))),
                 sort(fit0_case_ids))
  })

# With Case ID

case_id_test <- paste0(sample(letters, 50, replace = TRUE),
                       sample(letters, 50, replace = TRUE))
rerun_out <- suppressWarnings(lavaan_rerun(fit0, case_id = case_id_test, md_top = 4, parallel = FALSE))

test_that("Check the number of reruns", {
    expect_equal(length(rerun_out$rerun), md_top)
  })

test_that("Check the names of reruns", {
    expect_equal(names(rerun_out$rerun), case_id_test[md_selected])
  })

test_that("Check selected", {
    expect_equal(case_id_test[rerun_out$selected], case_id_test[md_selected])
  })

