#' @title Standardized Case Influence on Parameter Estimates
#'
#' @description Gets a [lavaan_rerun()] output and computes the
#' standardized changes in selected parameters for each case
#' if included.
#'
#' @details For each case, [est_change()] computes the differences in
#' the estimates of selected parameters with and without this case:
#'
#' (Estimate with all case) - (Estimate without this case).
#'
#' The
#' differences are standardized by dividing the raw differences by
#' their standard errors (Pek & MacCallum, 2011).
#' This is a measure of the standardized influence of a case on
#' the parameter
#' estimates if it is included.
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
#' It will also compute generalized Cook's distance (*gCD*), proposed by
#' Pek and MacCallum (2011) for structural equation modeling.
#' Only the parameters selected (all free parameters, by default)
#' will be used in computing *gCD*.
#'
#' Since version 0.1.4.8, if (a) a model has one or more equality
#' constraints, and
#' (b) some selected parameters are linearly dependent or constrained
#' to be equal due to the constraint(s), *gCD* will be computed
#' by removing parameters such that the remaining parameters are
#' not linearly dependent nor constrained to be equal.
#' (Support for equality constraints and
#' linearly dependent parameters available in 0.1.4.8 and later version).
#'
#' Supports both single-group and multiple-group models.
#' (Support for multiple-group models available in 0.1.4.8 and later version).
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
#' @return An `est_change`-class object, which is
#' matrix with the number of columns equals to the number of
#' requested parameters plus one, the last column being the
#' generalized Cook's
#' distance. The number of rows equal to the number
#' of cases. The row names are the case identification values used in
#' [lavaan_rerun()]. The elements are the standardized difference.
#' Please see Pek and MacCallum (2011), Equation 7.
#' A print method is available for user-friendly output.
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
#' # Compute the standardized changes in parameter estimates
#' # if a case is included vs. if this case is excluded.
#' # That is, case influence on parameter estimates, standardized.
#' out <- est_change(fit_rerun)
#' # Case influence:
#' out
#' # Note that these are the differences divided by the standard errors
#' # The rightmost column, `gcd`, contains the
#' # generalized Cook's distances (Pek & MacCallum, 2011).
#' out[, "gcd", drop = FALSE]
#'
#' # Compute the changes for the paths from iv1 and iv2 to m1
#' out2 <- est_change(fit_rerun, c("m1 ~ iv1", "m1 ~ iv2"))
#' # Case influence:
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
#' # Compute the standardized changes in parameter estimates
#' # if a case is included vs. if a case is excluded.
#' # That is, case influence on parameter estimates, standardized.
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
#' # Compute the changes in parameter estimates if a case is included
#' # vs. if a case is excluded.
#' # That is, standardized case influence on parameter estimates.
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
  if (ngroups == 1) {
      estorg$group <- 1
      estorg$group[estorg$op == ":="] <- 0
    }
  ptable <- lavaan::parameterTable(fit0)
  ptable_cols <- c("lhs", "op", "rhs",
                    "free", "label", "id",
                    "lavlabel", "group")

  # Ensure that plabels are not used as lavlabels
  tmp1 <- ptable$plabel[ptable$plabel != ""]
  tmp2 <- ptable$label %in% tmp1
  tmp3 <- ptable$label
  ptable$label[tmp2] <- ""
  ptable$lavlabel <- lavaan::lav_partable_labels(ptable,
                                                 type = "user")
  ptable$label <- tmp3
  tmp <- (ptable$lavlabel == ptable$plabel)
  ptable$lavlabel[tmp] <- ""

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

  # Not yet support parameters constrained to be equal
  tmp <- tryCatch(solve(vcov(fit0)[parameters_selected,
                                   parameters_selected]),
                   error = function(e) e)
  if (inherits(tmp, "error")) {
      any_depend <- TRUE
    } else {
      any_depend <- FALSE
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

  attr(out, "call") <- match.call()
  attr(out, "change_type") <- "standardized"
  attr(out, "method") <- "leave_one_out"
  attr(out, "standardized") <- FALSE

  class(out) <- c("est_change", class(out))

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
  vcovi_full <- vcovi_full[parameters_selected, parameters_selected,
                           drop = FALSE]
  k <- length(esti_change)
  esti_change_raw <- (est$est - esti_full$est)
  names(esti_change_raw) <- parameters_names
  esti_change_raw <- esti_change_raw[parameters_selected]
  vcovi_full_inv <- tryCatch(solve(vcovi_full),
                             error = function(e) e)
  if (inherits(vcovi_full_inv, "error")) {
      vcovi_full_0 <- full_rank(vcovi_full)
      vcovi_full_1 <- vcovi_full_0$final
      p_kept <- seq_len(ncol(vcovi_full))
      if (length(vcovi_full_0$dropped) > 0) {
          p_kept <- p_kept[-vcovi_full_0$dropped]
        }
      vcovi_full_1_inv <- tryCatch(solve(vcovi_full_1),
                                   error = function(e) e)
      if (inherits(vcovi_full_1_inv, "error")) {
          gcdi <- NA
        } else {
          esti_change_raw_1 <- esti_change_raw[p_kept]
          k_1 <- length(p_kept)
          gcdi <- matrix(esti_change_raw_1, 1, k_1) %*% vcovi_full_1_inv %*%
                  matrix(esti_change_raw_1, k_1, 1)
        }
    } else {
      gcdi <- matrix(esti_change_raw, 1, k) %*% vcovi_full_inv %*%
              matrix(esti_change_raw, k, 1)
    }
  outi <- c(esti_change, gcdi)
  names(outi) <- c(parameters_names[parameters_selected], "gcd")
  outi
}