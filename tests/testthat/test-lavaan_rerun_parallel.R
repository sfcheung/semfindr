library(testthat)
library(lavaan)
library(semfindr)

#context("Test lavaan_rerun")
skip("To be tested in an interactive session")

test_that("ncores", {

mod <-
'
iv1 ~~ iv2
m1 ~ iv1 + iv2
dv ~ m1
'

dat <- pa_dat

dat0 <- dat[1:50, ]
fit0 <- lavaan::sem(mod, dat0)
fit0_15 <- lavaan::sem(mod, dat0[-15, ])

rerun_out <- lavaan_rerun(fit0,
                          parallel = TRUE,
                          ncores = 4)
rerun_15 <- rerun_out$rerun[[15]]

expect_equal(ignore_attr = TRUE,
  parameterEstimates(fit0_15), parameterEstimates(rerun_15)
)

})

