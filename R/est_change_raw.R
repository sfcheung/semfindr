#' @title Case Influence on Parameter Estimates
#'
#' @description Gets a [lavaan_rerun()] output and computes the
#' changes in selected parameters for each case if included.
#'
#' @details For each case, [est_change_raw()] computes the differences
#' in the estimates of selected parameters with and without this
#' case:
#'
#' (Estimate with all case) - (Estimate without this case).
#'
#' The
#' change is the raw change, either for the standardized or
#' unstandardized solution. The change is *not* divided by standard
#' error.
#' This is a measure of the influence of a case on the parameter
#' estimates if it is included.
#'
#' If the value of a case is positive, including the case increases an estimate.
#'
#' If the value of a case is negative, including the case decreases an estimate.
#'
#' If the analysis is not admissible or did not converge when a case
#' is deleted, `NA`s will be returned for this case on the
#' differences.
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
#' Supports specifying an operator to select all parameters with these
#' operators: `~`, `~~`, `=~`, and `~1`. This vector can contain
#' both parameter names and operators. More details can be found
#' in the help of [pars_id()].
#' If omitted or `NULL`, the
#' default, changes on all free parameters will be computed.
#'
#' @param standardized If `TRUE`, the changes in the full standardized
#' solution is returned (`type` = `std.all` in
#' [lavaan::standardizedSolution()]). Otherwise, the changes in the
#' unstandardized solution are returned. Default is `FALSE`.
#'
#' @param user_defined_label_full Logical. If `TRUE`, use the full
#' labels for user-defined parameters (parameters created by
#' `:=`), which include the definition. If `FALSE`, then only
#' the label on the right-hand side of `:=` will be used.
#' Default is `FALSE`. In previous version, the full labels
#' were used. Set to `TRUE` if backward compatibility
#' is needed.
#'
#' @return An `est_change`-class object, which is
#' matrix with the number of columns equals to the number of
#' requested parameters, and the number of rows equals to the number
#' of cases. The row names are the case identification values used in
#' [lavaan_rerun()]. The elements are the raw differences.
#' A print method is available for user-friendly output.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>.
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
#'
#' # Fit the model n times. Each time with one case is removed.
#' # For illustration, do this only for four selected cases
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
#'                           to_rerun = c(3, 5, 7, 8))
#' # Compute the changes in parameter estimates if a case is included
#' # vs. if this case is excluded.
#' # That is, case influence on parameter estimates.
#' out <- est_change_raw(fit_rerun)
#' # Results excluding a case
#' out
#' # Note that these are the differences in parameter estimates.
#'
#' # The parameter estimates from all cases
#' (coef_all <- coef(fit))
#' # The parameter estimates from manually deleting the third case
#' fit_no_3 <- lavaan::sem(mod, dat[-3, ])
#' (coef_no_3 <- coef(fit_no_3))
#' # The differences
#' coef_all - coef_no_3
#' # The first row of `est_change_raw(fit_rerun)`
#' round(out[1, ], 3)
#'
#' # Compute only the changes of the paths from iv1 and iv2 to m1
#' out2 <- est_change_raw(fit_rerun, c("m1 ~ iv1", "m1 ~ iv2"))
#' # Results excluding a case
#' out2
#' # Note that only the changes in the selected paths are included.
#'
#' # Use standardized = TRUE to compare the differences in standardized solution
#' out2_std <- est_change_raw(fit_rerun,
#'                            c("m1 ~ iv1", "m1 ~ iv2"),
#'                            standardized = TRUE)
#' out2_std
#' (est_std_all <- parameterEstimates(fit,
#'                  standardized = TRUE)[1:2, c("lhs", "op", "rhs", "std.all")])
#' (est_std_no_1 <- parameterEstimates(fit_no_3,
#'                  standardized = TRUE)[1:2, c("lhs", "op", "rhs", "std.all")])
#' # The differences
#' est_std_all$std.all - est_std_no_1$std.all
#' # The first row of `out2_std`
#' out2_std[1, ]
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
#' # Compute the changes in parameter estimates if a case is included
#' # vs. if this case is excluded.
#' # That is, case influence on parameter estimates.
#' # For free loadings only
#' out <- est_change_raw(fit_rerun, parameters = "=~")
#' out
#' # For standardized loadings only
#' out_std <- est_change_raw(fit_rerun, parameters = "=~",
#'                           standardized = TRUE)
#' out_std
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
#' # vs. if this case is excluded.
#' # That is, case influence on parameter estimates.
#' # For structural paths only
#' out <- est_change_raw(fit_rerun, parameters = "~")
#' out
#' # For standardized paths only
#' out_std <- est_change_raw(fit_rerun, parameters = "~",
#'                           standardized = TRUE)
#' out_std
#'
#'
#' @references Pek, J., & MacCallum, R. (2011). Sensitivity analysis
#'  in structural equation models: Cases and their influence.
#'  *Multivariate Behavioral Research, 46*(2), 202-228.
#'  doi:10.1080/00273171.2011.561068
#'
#' @export
#' @importMethodsFrom lavaan vcov

est_change_raw <- function(rerun_out,
                           parameters = NULL,
                           standardized = FALSE,
                           user_defined_label_full = FALSE) {
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
                  standardized = TRUE,
                  fmi = FALSE,
                  cov.std = TRUE,
                  rsquare = FALSE,
                  remove.nonfree = !standardized,
                  output = "data.frame"
                )
  estorg$est_id <- seq_len(nrow(estorg))
  ngroups <- lavaan::lavTech(fit0, "ngroups")
  if (ngroups == 1) {
      estorg$group <- 1
      estorg$group[estorg$op == ":="] <- 0
    }
  ptable <- lavaan::parameterTable(fit0)
  ptable_cols <- c("lhs", "op", "rhs",
                    "free", "id",
                    "lavlabel", "group")
  # Do not use user labels except for user-defined parameters
  # Ensure that plabels are not used as lavlabels
  tmp1 <- ptable$plabel[ptable$plabel != ""]
  tmp2 <- ptable$label %in% tmp1
  tmp3 <- ptable$label
  ptable$label[tmp2] <- ""
  ptable$label[ptable$op != ":="] <- ""
  ptable$lavlabel <- lavaan::lav_partable_labels(ptable,
                                                 type = "user")
  if (!user_defined_label_full) {
      ptable$label[ptable$op == ":="] <- tmp3[ptable$op == ":="]
    }
  tmp <- (ptable$lavlabel == ptable$plabel)
  ptable$lavlabel[tmp] <- ""

  est0 <- merge(estorg, ptable[, ptable_cols])
  est0 <- est0[order(est0$id), ]
  parameters_names <- est0$lavlabel
  if (!is.null(parameters)) {
      tmp <- pars_id(parameters,
                    fit = fit0,
                    where = "partable",
                    free_only = FALSE)
      tmp2 <- pars_id_to_lorg(tmp,
                              pars_source = ptable,
                              type = "all")
      tmp3 <- merge(estorg, tmp2)
      parameters_selected <- tmp3$est_id
    } else {
      parameters_selected <- seq_len(length(parameters_names))
    }
  out <- sapply(reruns,
                function(x, est, parameters_names, parameters_selected,
                                 standardized) {
                  chk <- suppressWarnings(lavaan::lavTech(x, "post.check"))
                  chk2 <- lavaan::lavTech(x, "converged")
                  if (isTRUE(chk) & isTRUE(chk2)) {
                      return(est_change_raw_i(x, est = est,
                              parameters_names = parameters_names,
                              parameters_selected = parameters_selected,
                              standardized = standardized)
                            )
                    } else {
                      return(rep(NA, length(parameters_selected)))
                    }
                  },
                est = est0,
                parameters_names = parameters_names,
                parameters_selected = parameters_selected,
                standardized = standardized
              )
  if (is.null(dim(out))) {
      out <- matrix(out, length(out), 1)
      colnames(out) <- parameters
    } else {
      out <- t(out)
    }
  colnames(out) <- parameters_names[parameters_selected]
  rownames(out) <- case_ids

  attr(out, "call") <- match.call()
  attr(out, "change_type") <- "raw"
  attr(out, "method") <- "leave_one_out"
  attr(out, "standardized") <- standardized

  class(out) <- c("est_change", class(out))

  out
}

# est_change_raw() for one fit object.
#' @noRd

est_change_raw_i <- function(x,
                             est,
                             parameters_names,
                             parameters_selected,
                             standardized) {
  esti_full <- lavaan::parameterEstimates(
                x,
                se = TRUE,
                zstat = FALSE,
                pvalue = FALSE,
                ci = FALSE,
                standardized = TRUE,
                fmi = FALSE,
                cov.std = TRUE,
                rsquare = FALSE,
                remove.nonfree = !standardized,
                output = "data.frame"
                )
  if (standardized) {
      esti_change <- (est$std.all - esti_full$std.all)
    } else {
      esti_change <- (est$est - esti_full$est)
    }
  names(esti_change) <- parameters_names
  outi <- esti_change[parameters_selected]
  names(outi) <- parameters_selected
  outi
}