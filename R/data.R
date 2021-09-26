#' Sample data set for a path model
#'
#' A four-variable dataset with 100 cases
#'
#' A Sample Model:
#' ```
#' `m1 ~  iv1 + iv2`
#' `dv ~  m1`
#' ```
"pa_dat"

#' Sample data set for a CFA model
#'
#' A six-variable dataset with 100 cases.
#'
#' A Sample Model:
#' ```
#' `f1 =~  x1 + x2 + x3`
#' `f2 =~  x4 + x5 + x6`
#' `f1 ~~ f2`
#' ```
"cfa_dat"

#' Sample data set for a structural equation model
#'
#' A nine-variable dataset with 100 cases.
#'
#' A Sample Model:
#' ```
#' `f1 =~  x1 + x2 + x3`
#' `f2 =~  x4 + x5 + x6`
#' `f3 =~  x7 + x8 + x9`
#' `f2 ~   f1`
#' `f3 ~   f2`
#' ```
"sem_dat"

#' Sample data set for a path model with an influential case
#'
#' A four-variable dataset with 100 cases and potential influential cases
#'
#' A Sample Model:
#' ```
#' `m1 ~  iv1 + iv2`
#' `dv ~  m1`
#' ```
#' Has one or more influential case
#'
"pa_dat2"

#' Sample data set for a CFA model with an influential case
#'
#' A six-variable dataset with 100 cases and potential influential cases
#'
#' A Sample Model:
#' ```
#' `f1 =~  x1 + x2 + x3`
#' `f2 =~  x4 + x5 + x6`
#' `f1 ~~ f2`
#' ```
#' Has one or more influential case
#'
"cfa_dat2"

#' Sample data set for a structural equation model with an influential case
#'
#' A nine-variable dataset with 100 cases and potential influential cases
#'
#' A Sample Model:
#' ```
#' `f1 =~  x1 + x2 + x3`
#' `f2 =~  x4 + x5 + x6`
#' `f3 =~  x7 + x8 + x9`
#' `f2 ~   f1`
#' `f3 ~   f2`
#' ```
#' Has one or more influential case
#'
"sem_dat2"

#' Sample data set for a structural equation model with a negative variance
#'
#' A six-variable dataset with 60 cases that has a case that, if not removed,
#' will result in a negative variance in the solution
#'
#' A Sample Model:
#' ```
#' `f1 =~  x1 + x2 + x3`
#' `f2 =~  x4 + x5 + x6`
#' `f1 ~~ f2`
#' ```
#'
"cfa_dat_heywood"
