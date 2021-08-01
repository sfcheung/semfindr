#' A four-variable dataset with 100 cases
#'
#' A four-variable dataset with 100 cases
#'
#' Fitted by this model:
#' ```
#' `m1 ~  iv1 + iv2`
#' `dv ~  m1`
#' ```
"pa_dat"

#' A six-variable dataset with 100 cases
#'
#' A six-variable dataset with 100 cases.
#'
#' Fitted by this model:
#' ```
#' `f1 =~  x1 + x2 + x3`
#' `f2 =~  x4 + x5 + x6`
#' `f1 ~~ f2`
#' ```
"cfa_dat"

#' A nine-variable dataset with 100 cases
#'
#' A nine-variable dataset with 100 cases.
#'
#' Fitted by this model:
#' ```
#' `f1 =~  x1 + x2 + x3`
#' `f2 =~  x4 + x5 + x6`
#' `f3 =~  x7 + x8 + x9`
#' `f2 ~   f1`
#' `f3 ~   f2`
#' ```
"sem_dat"

#' A four-variable dataset with 100 cases and potential influential cases
#'
#' A four-variable dataset with 100 cases and potential influential cases
#'
#' Fitted by this model:
#' ```
#' `m1 ~  iv1 + iv2`
#' `dv ~  m1`
#' ```
#' Has one or more influential case
#'
"pa_dat2"

#' A six-variable dataset with 100 cases and potential influential cases
#'
#' A six-variable dataset with 100 cases and potential influential cases
#'
#' Fitted by this model:
#' ```
#' `f1 =~  x1 + x2 + x3`
#' `f2 =~  x4 + x5 + x6`
#' `f1 ~~ f2`
#' ```
#' Has one or more influential case
#'
"cfa_dat2"

#' A nine-variable dataset with 100 cases and potential influential cases
#'
#' A nine-variable dataset with 100 cases and potential influential cases
#'
#' Fitted by this model:
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

