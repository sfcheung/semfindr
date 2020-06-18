#' A four-variable dataset with 100 cases
#'
#' A four-variable dataset with 100 cases
#'
#' Generated from this model:
#' ```
#' `m1 ~  0.5*iv1 + 0.6*iv2`
#' `dv ~  0.4*iv1 + 0.1*iv2 + 0.5*m1`
#' ```
"pa_dat"

#' A six-variable dataset with 100 cases
#'
#' A six-variable dataset with 100 cases.
#'
#' Generated from this model:
#' ```
#' `f1 =~  .7*x1 + .6*x2 + .8*x3 + .3*x5`
#' `f2 =~  .2*x1 + .6*x4 + .8*x5 + .7*x6`
#' `f1 ~~ .2*f2`
#' ```
"cfa_dat"

#' A four-variable dataset with 100 cases
#'
#' A four-variable dataset with 100 cases.
#'
#' Generated from this model:
#' ```
#' `f1 =~  .7*x1 + .6*x2 + .8*x3 + .3*x5`
#' `f2 =~  .2*x1 + .6*x4 + .8*x5 + .7*x6`
#' `f3 =~  .5*x4 + .2*x7 + .6*x8 + .8*x9`
#' `f2 ~   .3*f1`
#' `f3 ~   .3*f2 + .8*f1`
#' ```
"sem_dat"

#' The *n* [lavaan::sem()] results on [pa_dat] 
#'
#' The *n* [lavaan::sem()] results on [pa_dat].
#'
#' The [lavaan_rerun()] output fitting the following model to [pa_dat]
#' ```
#'`iv1 ~~ iv2`
#'`m1 ~ iv1 + iv2`
#'`dv ~ m1`
#' ```
"pa_rerun"

#' The *n* [lavaan::sem()] results on [cfa_dat] 
#'
#' The *n* [lavaan::sem()] results on [cfa_dat]. 
#'
#' The [lavaan_rerun()] output fitting the following model to [cfa_dat]
#' ```
#'`f1 =~ x1 + x2 + x3`
#'`f2 =~ x4 + x5 + x6`
#' ```
"cfa_rerun"

