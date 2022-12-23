#' @title Sample Data: A Path Model
#'
#' @description A four-variable dataset with 100 cases.
#'
#' @format A data frame with 100 rows
#' and 5 variables:
#' \describe{
#'   \item{m1}{Mediator. Numeric.}
#'   \item{dv}{Outcome variable. Numeric.}
#'   \item{iv1}{Predictor. Numeric.}
#'   \item{iv2}{Predictor. Numeric.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(pa_dat)
#' mod <-
#' "
#' m1 ~ a1 * iv1 + a2 * iv2
#' dv ~ b * m1
#' a1b := a1 * b
#' a2b := a2 * b
#' "
#' fit <- sem(mod, pa_dat)
#' summary(fit)
"pa_dat"

#' @title Sample Data: A CFA Model
#'
#' @description A six-variable dataset with 100 cases.
#'
#' @format A data frame with 100 rows
#' and 6 variables:
#' \describe{
#'   \item{x1}{Indicator. Numeric.}
#'   \item{x2}{Indicator. Numeric.}
#'   \item{x3}{Indicator. Numeric.}
#'   \item{x4}{Indicator. Numeric.}
#'   \item{x5}{Indicator. Numeric.}
#'   \item{x6}{Indicator. Numeric.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(cfa_dat)
#' mod <-
#' "
#' f1 =~  x1 + x2 + x3
#' f2 =~  x4 + x5 + x6
#' "
#' fit <- cfa(mod, cfa_dat)
#' summary(fit)
"cfa_dat"

#' @title Sample Data: A Latent Variable Structural Model
#'
#' @description A nine-variable dataset with 200 cases.
#'
#' @format A data frame with 200 rows
#' and 9 variables:
#' \describe{
#'   \item{x1}{Indicator. Numeric.}
#'   \item{x2}{Indicator. Numeric.}
#'   \item{x3}{Indicator. Numeric.}
#'   \item{x4}{Indicator. Numeric.}
#'   \item{x5}{Indicator. Numeric.}
#'   \item{x6}{Indicator. Numeric.}
#'   \item{x7}{Indicator. Numeric.}
#'   \item{x8}{Indicator. Numeric.}
#'   \item{x9}{Indicator. Numeric.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(sem_dat)
#' mod <-
#' "
#' f1 =~  x1 + x2 + x3
#' f2 =~  x4 + x5 + x6
#' f3 =~  x7 + x8 + x9
#' f2 ~ a * f1
#' f3 ~ b * f2
#' ab := a * b
#' "
#' fit <- sem(mod, sem_dat)
#' summary(fit)
"sem_dat"

#' @title Sample Data: A Path Model with an Influential Case
#'
#' @description A four-variable dataset with 100 cases, with
#'  one influential case.
#'
#' @format A data frame with 100 rows
#' and 5 variables:
#' \describe{
#'   \item{case_id}{Case ID. Character.}
#'   \item{iv1}{Predictor. Numeric.}
#'   \item{iv2}{Predictor. Numeric.}
#'   \item{m1}{Mediator. Numeric.}
#'   \item{dv}{Outcome variable. Numeric.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(pa_dat2)
#' mod <-
#' "
#' m1 ~ a1 * iv1 + a2 * iv2
#' dv ~ b * m1
#' a1b := a1 * b
#' a2b := a2 * b
#' "
#' fit <- sem(mod, pa_dat2)
#' summary(fit)
#' inf_out <- influence_stat(fit)
#' gcd_plot(inf_out)
"pa_dat2"

#' @title Sample Data: A CFA Model with an Influential Case
#'
#' @description A six-variable dataset with 100 cases, with
#'  one influential case.
#'
#' @format A data frame with 100 rows
#' and 7 variables:
#' \describe{
#'   \item{case_id}{Case ID. Character.}
#'   \item{x1}{Indicator. Numeric.}
#'   \item{x2}{Indicator. Numeric.}
#'   \item{x3}{Indicator. Numeric.}
#'   \item{x4}{Indicator. Numeric.}
#'   \item{x5}{Indicator. Numeric.}
#'   \item{x6}{Indicator. Numeric.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(cfa_dat2)
#' mod <-
#' "
#' f1 =~  x1 + x2 + x3
#' f2 =~  x4 + x5 + x6
#' "
#' fit <- cfa(mod, cfa_dat2)
#' summary(fit)
#' inf_out <- influence_stat(fit)
#' gcd_plot(inf_out)
"cfa_dat2"

#' @title Sample Data: A Latent Variable Structural Model
#'  With an Influential Case
#'
#' @description A ten-variable dataset with 200 cases, with
#'  one influential case.
#'
#' @format A data frame with 200 rows
#' and 10 variables:
#' \describe{
#'   \item{case_id}{Case ID. Character.}
#'   \item{x1}{Indicator. Numeric.}
#'   \item{x2}{Indicator. Numeric.}
#'   \item{x3}{Indicator. Numeric.}
#'   \item{x4}{Indicator. Numeric.}
#'   \item{x5}{Indicator. Numeric.}
#'   \item{x6}{Indicator. Numeric.}
#'   \item{x7}{Indicator. Numeric.}
#'   \item{x8}{Indicator. Numeric.}
#'   \item{x9}{Indicator. Numeric.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(sem_dat2)
#' mod <-
#' "
#' f1 =~  x1 + x2 + x3
#' f2 =~  x4 + x5 + x6
#' f3 =~  x7 + x8 + x9
#' f2 ~ a * f1
#' f3 ~ b * f2
#' ab := a * b
#' "
#' fit <- sem(mod, sem_dat2)
#' summary(fit)
#' inf_out <- influence_stat(fit)
#' gcd_plot(inf_out)
"sem_dat2"

#' @title Sample Data: A CFA Model with a Heywood Case
#'
#' @description A six-variable dataset with 60 cases, with
#'  one case resulting in negative variance if not removed.
#'
#' @format A data frame with 60 rows
#' and 6 variables:
#' \describe{
#'   \item{x1}{Indicator. Numeric.}
#'   \item{x2}{Indicator. Numeric.}
#'   \item{x3}{Indicator. Numeric.}
#'   \item{x4}{Indicator. Numeric.}
#'   \item{x5}{Indicator. Numeric.}
#'   \item{x6}{Indicator. Numeric.}
#' }
#'
#' @examples
#' library(lavaan)
#' data(cfa_dat_heywood)
#' mod <-
#' "
#' f1 =~  x1 + x2 + x3
#' f2 =~  x4 + x5 + x6
#' "
#' # The following will result in a warning
#' fit <- cfa(mod, cfa_dat_heywood)
#' # One variance is negative
#' parameterEstimates(fit, output = "text")
#' # Fit the model with the first case removed
#' fit_no_case_1 <- cfa(mod, cfa_dat_heywood[-1, ])
#' # Results admissible
#' parameterEstimates(fit_no_case_1, output = "text")
"cfa_dat_heywood"
