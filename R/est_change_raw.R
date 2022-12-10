#' @title Case Influence on Parameter Estimates
#'
#' @description Gets a [lavaan_rerun()] output and computes the
#'  changes in selected parameters for each case.
#'
#' @details For each case, [est_change_raw()] computes the differences
#'  in the estimates of selected parameters with and without this
#'  case: (estimate with all case) - (estimate without this case). The
#'  change is the raw change, either for the standardized or
#'  unstandardized solution. The change is *not* divided by standard
#'  error.
#'
#' If the analysis is not admissible or did not converge when a case
#'  is deleted, `NA`s will be returned for this case on the
#'  differences.
#'
#' Currently it only supports single-group models.
#'
#' @param rerun_out The output from [lavaan_rerun()].
#' @param parameters A character vector to specify the selected
#'  parameters. Each parameter is named as in `lavaan` syntax, e.g.,
#'  `x ~ y` or `x ~~ y`, as appeared in the columns `lhs`, `op`, and `rhs`
#'  in the output of [lavaan::parameterEstimates()].
#'  Supports specifying an operator to select all parameters with this
#'  operators: `~`, `~~`, `=~`, and `~1`. This vector can contain
#'  both parameter names and operators.
#'  If `NULL`, the
#'  default, differences on all free parameters will be computed.
#' @param standardized If `TRUE`, the changes in the full standardized
#'  solution is returned (`type` = `std.all` in
#'  [lavaan::standardizedSolution()]). Otherwise, the changes in the
#'  unstandardized solution is returned. Default is `FALSE`.
#'
#' @return A matrix with the number of columns equals to the number of
#'  requested parameters, and the number of rows equals to the number
#'  of cases. The row names are the case identification values used in
#'  [lavaan_rerun()]. The elements are the raw differences.
#'
#' @author Shu Fai Cheung (shufai.cheung@gmail.com)
#'
#' @examples
#' library(lavaan)
#' dat <- pa_dat
#' # For illustration only, select only the first 50 cases
#' dat <- dat[1:50, ]
#' # The model
#' mod <-
#' "
#' m1 ~ iv1 + iv2
#' dv ~ m1
#' "
#' # Fit the model
#' fit <- lavaan::sem(mod, dat)
#' summary(fit)
#' # Fit the model n times. Each time with one case removed.
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE)
#' # Compute the changes in parameter estimates if a case is removed
#' out <- est_change_raw(fit_rerun)
#' # Results excluding a case, for the first few cases
#' head(out)
#' # Note that these are the differences in parameter estimates.
#'
#' # The parameter estimates from all cases
#' (coef_all <- coef(fit))
#' # The parameter estimates from manually deleting the first case
#' fit_no_1 <- lavaan::sem(mod, dat[-1, ])
#' (coef_no_1 <- coef(fit_no_1))
#' # The differences
#' coef_all - coef_no_1
#' # The first row of `est_change_raw(fit_rerun)`
#' round(out[1, ], 3)
#'
#' # Compute only the changes of the paths from iv1 and iv2 to m1
#' out2 <- est_change_raw(fit_rerun, c("m1 ~ iv1", "m1 ~ iv2"))
#' # Results excluding a case, for the first few cases
#' head(out2)
#' # Note that only the changes in the selected paths are included.
#'
#' # Use standardized = TRUE to compare the differences in standardized solution
#' out2_std <- est_change_raw(fit_rerun,
#'                           c("m1 ~ iv1", "m1 ~ iv2"),
#'                           standardized = TRUE)
#' head(out2_std)
#' (est_std_all <- parameterEstimates(fit,
#'                  standardized = TRUE)[1:2, c("lhs", "op", "rhs", "std.all")])
#' (est_std_no_1 <- parameterEstimates(fit_no_1,
#'                  standardized = TRUE)[1:2, c("lhs", "op", "rhs", "std.all")])
#' # The differences
#' est_std_all$std.all - est_std_no_1$std.all
#' # The first row of `out2_std`
#' out2_std[1, ]
#'
#' @references Pek, J., & MacCallum, R. (2011). Sensitivity analysis
#'  in structural equation models: Cases and their influence.
#'  *Multivariate Behavioral Research, 46*(2), 202–228.
#'  doi:10.1080/00273171.2011.561068
#'
#' @export
#' @importMethodsFrom lavaan vcov

est_change_raw <- function(rerun_out,
                       parameters = NULL,
                       standardized = FALSE
                       ) {
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
  if (ngroups == 1) estorg$group <- 1
  estorg[estorg$op == ":=", "group"] <- 0
  ptable <- lavaan::parameterTable(fit0)
  ptable_cols <- c("lhs", "op", "rhs",
                    "free", "id",
                    "lavlabel")
  # Do not use user labels
  ptable$label <- ""
  ptable$lavlabel <- lavaan::lav_partable_labels(ptable,
                                                 type = "user")
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