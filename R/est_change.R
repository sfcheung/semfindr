#' @title Standardized Case Influence on Parameter Estimates
#'
#' @description Gets a [lavaan_rerun()] output and computes the
#' standardized changes in selected parameters for each case
#' if included.
#'
#' @details For each case, [est_change()] computes the differences in
#' the estimates of selected parameters with and without this case:
#' (estimate with all case) - (estimate without this case). The
#' differences are standardized by dividing the raw differences by
#' their standard errors (Pek & MacCallum, 2011).
#'
#' If the value of a case is positive, including the case increases an estimate.
#'
#' If the value of a case is negative, including the case decreases an estimate.
#'
#' If the analysis is not admissible or does not converge when a case
#' is deleted, `NA`s will be turned for this case on the differences.
#'
#' Unlike [est_change_raw()], [est_change()] does not support
#' computing the standardized changes of standardized estimates.
#'
#' Currently it only supports single-group models.
#'
#' @param rerun_out The output from [lavaan_rerun()].
#'
#' @param parameters A character vector to specify the selected
#' parameters. Each parameter is named as in `lavaan` syntax, e.g.,
#' `x ~ y` or `x ~~ y`, as appeared in the columns `lhs`, `op`, and `rhs`
#' in the output of [lavaan::parameterEstimates()].
#' Supports specifying an operator to select all parameters with this
#' operators: `~`, `~~`, `=~`, and `~1`. This vector can contain
#' both parameter names and operators. More details can be found
#' in the help of [pars_id()].
#' If omitted or `NULL`, the
#' default, changes on all free parameters will be computed.
#'
#' @return A matrix. The number of columns is equal to the number of
#' requested parameters plus one, the last column being the
#' approximate generalized Cook's
#' distance. The number of rows equal to the number
#' of cases. The row names are the case identification values used in
#' [lavaan_rerun()]. The elements are the standardized difference.
#' Please see Pek and MacCallum (2011), Equation 7.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>.
#'
#' @references Pek, J., & MacCallum, R. (2011). Sensitivity analysis
#' in structural equation models: Cases and their influence.
#' *Multivariate Behavioral Research, 46*(2), 202-228.
#' doi:10.1080/00273171.2011.561068
#'
#' @examples
#' library(lavaan)
#'
#' # A path model
#'
#' dat <- pa_dat
#' mod <-
#' "
#' m1 ~ a1 * iv1 + a2 * iv2
#' dv ~ b * m1
#' a1b := a1 * b
#' a2b := a2 * b
#' "
#' # Fit the model
#' fit <- lavaan::sem(mod, dat)
#' summary(fit)
#' # Fit the model several times. Each time with one case removed.
#' # For illustration, do this only for four selected cases
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
#'                           to_rerun = c(2, 4, 7, 9))
#' # Compute the changes in chisq if a case is removed
#' out <- est_change(fit_rerun)
#' # Results excluding a case
#' out
#' # Note that these are the differences divided by the standard errors
#' # The rightmost column, `gcd`, contains the
#' # generalized Cook's distances (Pek & MacCallum, 2011).
#' out[, "gcd", drop = FALSE]
#'
#' # Compute the changes for the paths from iv1 and iv2 to m1
#' out2 <- est_change(fit_rerun, c("m1 ~ iv1", "m1 ~ iv2"))
#' # Results excluding a case
#' out2
#' # Note that only the changes in the selected parameters are included.
#' # The generalized Cook's distance is computed only from the selected
#' # parameter estimates.
#'
#' # A CFA model
#'
#' dat <- cfa_dat
#' mod <-
#' "
#' f1 =~  x1 + x2 + x3
#' f2 =~  x4 + x5 + x6
#' f1 ~~ f2
#' "
#' # Fit the model
#' fit <- lavaan::cfa(mod, dat)
#'
#' # Examine four selected cases
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
#'                           to_rerun = c(2, 3, 5, 7))
#' # Compute the changes in parameter estimates if a case is removed
#' # For free loadings only
#' out <- est_change(fit_rerun, parameters = "=~")
#' out
#'
#' # A latent variable model
#'
#' dat <- sem_dat
#' mod <-
#' "
#' f1 =~  x1 + x2 + x3
#' f2 =~  x4 + x5 + x6
#' f3 =~  x7 + x8 + x9
#' f2 ~   a * f1
#' f3 ~   b * f2
#' ab := a * b
#' "
#' # Fit the model
#' fit <- lavaan::sem(mod, dat)
#'
#' # Examine four selected cases
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
#'                           to_rerun = c(2, 3, 5, 7))
#' # Compute the changes in parameter estimates if a case is removed
#' # For structural paths only
#' out <- est_change(fit_rerun, parameters = "~")
#' out
#'
#' @export
#' @importMethodsFrom lavaan vcov coef

est_change <- function(rerun_out,
                       parameters = NULL) {
  if (missing(rerun_out)) {
      stop("No lavaan_rerun output supplied.")
    }
  case_ids <- names(rerun_out$rerun)
  reruns <- rerun_out$rerun
  fit0   <- rerun_out$fit
  estorg   <- lavaan::parameterEstimates(
              fit0,
              se = FALSE,
              zstat = FALSE,
              pvalue = FALSE,
              ci = FALSE,
              standardized = FALSE,
              fmi = FALSE,
              cov.std = TRUE,
              rsquare = FALSE,
              remove.nonfree = TRUE,
              output = "data.frame"
              )
  ngroups <- lavaan::lavTech(fit0, "ngroups")
  if (ngroups == 1) estorg$group <- 1
  ptable <- lavaan::parameterTable(fit0)
  ptable_cols <- c("lhs", "op", "rhs",
                    "free", "label", "id",
                    "lavlabel")
  ptable$lavlabel <- lavaan::lav_partable_labels(ptable,
                                                 type = "user")
  est0 <- merge(estorg, ptable[, ptable_cols])
  est0 <- est0[order(est0$id), ]
  parameters_names <- est0$lavlabel
  if (!is.null(parameters)) {
      parameters_selected <- pars_id(parameters,
                                    fit = fit0,
                                    where = "coef")
    } else {
      parameters_selected <- est0$free[est0$free > 0]
    }
  out <- sapply(reruns,
                function(x, est, parameters_names, parameters_selected) {
                  chk <- suppressWarnings(lavaan::lavTech(x, "post.check"))
                  chk2 <- lavaan::lavTech(x, "converged")
                  if (isTRUE(chk) & isTRUE(chk2)) {
                      return(est_change_i(x, est = est,
                                    parameters_names = parameters_names,
                                    parameters_selected = parameters_selected)
                            )
                    } else {
                      return(rep(NA, length(parameters_selected) + 1))
                    }
                  },
                est = est0,
                parameters_names = parameters_names,
                parameters_selected = parameters_selected
              )
  # No need to check the number of columns because it is always greater than one

  out <- t(out)
  colnames(out) <- c(parameters_names[parameters_selected], "gcd")
  rownames(out) <- case_ids
  out
}

# est_change() for one fit object
#' @noRd

est_change_i <- function(x,
                         est,
                         parameters_names,
                         parameters_selected) {
  esti_full <- lavaan::parameterEstimates(
                x,
                se = TRUE,
                zstat = FALSE,
                pvalue = FALSE,
                ci = FALSE,
                standardized = FALSE,
                fmi = FALSE,
                cov.std = TRUE,
                rsquare = FALSE,
                remove.nonfree = TRUE,
                output = "data.frame"
                )
  esti_change <- (est$est - esti_full$est) / esti_full$se
  names(esti_change) <- parameters_names
  esti_change <- esti_change[parameters_selected]
  vcovi_full <- vcov(x)
  class(vcovi_full) <- "matrix"
  vcovi_full_names <- colnames(vcovi_full)
  q <- which(vcovi_full_names %in% est$label)
  colnames(vcovi_full)[q] <- parameters_names[q]
  rownames(vcovi_full)[q] <- parameters_names[q]
  vcovi_full <- vcovi_full[parameters_selected, parameters_selected]
  k <- length(esti_change)
  esti_change_raw <- (est$est - esti_full$est)
  names(esti_change_raw) <- parameters_names
  esti_change_raw <- esti_change_raw[parameters_selected]
  gcdi <- matrix(esti_change_raw, 1, k) %*% solve(vcovi_full) %*%
          matrix(esti_change_raw, k, 1)
  outi <- c(esti_change, gcdi)
  names(outi) <- c(parameters_names[parameters_selected], "gcd")
  outi
}