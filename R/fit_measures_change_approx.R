#' @title Case Influence on Fit Measures (Approximate)
#'
#' @description Gets a [lavaan::lavaan()] output and computes the
#' approximate change
#' in selected fit measures if a case is included.
#'
#' @details For each case, [fit_measures_change_approx()] computes the
#' approximate differences in selected fit measures with and
#' without this case:
#'
#' (Fit measure with all case) - (Fit measure without this case).
#'
#' If the value of a case is positive, including the case increases an estimate.
#'
#' If the value of a case is negative, including the case decreases an estimate.
#'
#' Note that an increase is an improvement in fit for
#' goodness of fit measures such as CFI and TLI, but a decrease
#' is an improvement in fit for badness of fit measures such as
#' RMSEA and model chi-square.
#' This is a measure of the influence of a case on a fit measure
#' if it is included.
#'
#' The model is not refitted. Therefore, the result is only an
#' approximation of that of [fit_measures_change()]. However, this
#' approximation is useful for identifying potentially influential
#' cases when the sample size is very large or the model takes a long
#' time to fit. This function can be used to identify potentially
#' influential cases quickly and then select them to conduct the
#' leave-one-out sensitivity analysis using [lavaan_rerun()] and
#' [fit_measures_change()].
#'
#' For the technical details, please refer to the vignette
#' on this approach: \code{vignette("casewise_scores", package = "semfindr")}
#'
#' Supports both single-group and multiple-group models.
#' (Support for multiple-group models available in 0.1.4.8 and later version).
#'
#' @param fit The output from [lavaan::lavaan()] or its wrappers (e.g.,
#' [lavaan::cfa()] and [lavaan::sem()]).
#'
#' @param fit_measures The argument `fit.measures` used in
#' [lavaan::fitMeasures]. Default is
#' `c("chisq", "cfi", "rmsea", "tli")`.
#' Currently, the approximate method supports only
#' these four measures.
#'
#' @param baseline_model The argument `baseline.model` used in
#' [lavaan::fitMeasures]. Default is `NULL`.
#'
#' @param case_id If it is a character vector of length equals to the
#' number of cases (the number of rows in the data in `fit`), then it
#' is the vector of case identification values. If it is `NULL`, the
#' default, then `case.idx` used by `lavaan` functions will be used
#' as case identification values.
#'
#' @param allow_inadmissible If `TRUE`, accepts a fit object with
#' inadmissible results (i.e., `post.check` from
#' [lavaan::lavInspect()] is `FALSE`). Default is `FALSE`.
#'
#' @param skip_all_checks If `TRUE`, skips all checks and allows
#' users to run this function on any object of `lavaan` class.
#' For users to experiment this and other functions on models
#' not officially supported. Default is `FALSE`.
#'
#' @return An `fit_measures_change`-class object, which is
#' matrix with the number of columns equals to the number of
#' requested fit measures, and the number of rows equals to the number
#' of cases. The row names are case identification values.
#' A print method is available for user-friendly output.
#'
#' @author Idea by Mark Hok Chio Lai <https://orcid.org/0000-0002-9196-7406>,
#' implemented by Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>.
#'
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
#'
#' # Approximate changes
#' out_approx <- fit_measures_change_approx(fit, fit_measures = "chisq")
#' head(out_approx)
#' # Fit the model several times. Each time with one case removed.
#' # For illustration, do this only for four selected cases
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
#'                           to_rerun = 1:5)
#' # Compute the changes in chisq if a case is included
#' # vs. if this case is excluded.
#' # That is, case influence on model chi-squared.
#' out <- fit_measures_change(fit_rerun, fit_measures = "chisq")
#' # Case influence, for the first few cases
#' head(out)
#' # Compare the results
#' plot(out_approx[1:5, "chisq"], out)
#' abline(a = 0, b = 1)
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
#' out_approx <- fit_measures_change_approx(fit, fit_measures = "chisq")
#' head(out_approx)
#'
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
#'                           to_rerun = 1:5)
#' # Compute the changes in chisq if a case is included
#' # vs. if this case is excluded.
#' # That is, case influence on fit measures.
#' out <- fit_measures_change(fit_rerun, fit_measures = "chisq")
#' # Results excluding a case, for the first few cases
#' head(out)
#' # Compare the results
#' plot(out_approx[1:5, "chisq"], out)
#' abline(a = 0, b = 1)
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
#' out_approx <- fit_measures_change_approx(fit, fit_measures = "chisq")
#' head(out_approx)
#'
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
#'                           to_rerun = 1:5)
#' # Compute the changes in chisq if a case is excluded
#' # vs. if this case is included.
#' # That is, case influence on model chi-squared.
#' out <- fit_measures_change(fit_rerun, fit_measures = "chisq")
#' # Case influence, for the first few cases
#' head(out)
#' # Compare the results
#' plot(out_approx[1:5, "chisq"], out)
#' abline(a = 0, b = 1)
#'
#' @importFrom methods .hasSlot
#' @export

fit_measures_change_approx <- function(fit,
                                       fit_measures = c("chisq",
                                                        "cfi",
                                                        "rmsea",
                                                        "tli"),
                                       baseline_model = NULL,
                                       case_id = NULL,
                                       allow_inadmissible = FALSE,
                                       skip_all_checks = FALSE
                                       ) {
    fit_measures <- match.arg(fit_measures, several.ok = TRUE)
    if (length(fit_measures) == 0) stop("No measure is requested.")
    if (missing(fit)) {
        stop("No lavaan output supplied.")
      }
    if (!inherits(fit, "lavaan")) {
        stop("The fit object is not a lavaan output.")
      }

    if (!skip_all_checks) {
      check_out <- approx_check(fit, print_messages = FALSE,
                                multiple_group = TRUE,
                                equality_constraint = TRUE)

      if (check_out != 0) {
          if ((check_out == -1) &&
              !(suppressWarnings(lavaan::lavInspect(fit, "post.check"))) &&
              allow_inadmissible) {
            } else {
              stop(attr(check_out, "info"))
            }
        }
      }
    ngroups <- lavaan::lavTech(fit, "ngroups")
    if (ngroups > 1) {
        n_j <- sapply(lavaan::lavInspect(fit, "data"), nrow)
        n <- sum(n_j)
      } else {
        n <- nrow(lavaan::lavInspect(fit, "data"))
        n_j <- n
      }
    case_ids <- lavaan::lavInspect(fit, "case.idx",
                                  drop.list.single.group = FALSE)
    case_ids <- unlist(case_ids, use.names = FALSE)
    case_ids_order <- order(case_ids)
    case_ids <- sort(case_ids)
    if (!is.null(case_id)) {
        if (length(case_id) != n) {
            stop("The length of case_id is not equal to the number of cases.")
          } else {
            case_ids <- case_id
          }
      }
    out <- matrix(NA, nrow = n, ncol = length(fit_measures))
    colnames(out) <- fit_measures
    rownames(out) <- case_ids
    mod_h1 <- lavaan::lav_partable_unrestricted(fit)
    opt_h1 <- fit@Options
    opt_h1$se <- "none"
    fit_h1 <- lavaan::lavaan(model = mod_h1,
                             slotOptions = opt_h1,
                             slotSampleStats = fit@SampleStats,
                             slotData = fit@Data)
    lli_h1 <- lavaan::lavInspect(fit_h1, what = "loglik.casewise",
                                 drop.list.single.group = FALSE)
    lli <- lavaan::lavInspect(fit, what = "loglik.casewise",
                              drop.list.single.group = FALSE)
    lli_h1 <- unlist(lli_h1, use.names = FALSE)[case_ids_order]
    lli <- unlist(lli, use.names = FALSE)[case_ids_order]
    chisq_change_fit <- 2 * (lli_h1 - lli)
    if ("chisq" %in% fit_measures) {
        out[, "chisq"] <- chisq_change_fit
      }
    tmp <- lavaan::fitMeasures(fit, c("chisq", "df"))
    chisq_fit <- unname(tmp["chisq"])
    df_fit <- unname(tmp["df"])
    chisq_fit_approx <- chisq_fit - chisq_change_fit

    # Adapted from lavaan:::lav_fit_measures
    if (!is.null(baseline_model)) {
      } else if (.hasSlot(fit, "baseline") &&
                length(fit@baseline) > 0L) {
        mod_base <- lavaan::lavInspect(fit,
                                       what = "baseline.partable")
        opt_baseline <- fit@Options
        opt_baseline$se <- "none"
        baseline_model <- lavaan::lavaan(model = mod_base,
                                slotOptions = opt_baseline,
                                slotSampleStats = fit@SampleStats,
                                slotData = fit@Data)
      } else {
        mod_base <- lavaan::lav_partable_independence(fit)
        opt_baseline <- fit@Options
        opt_baseline$se <- "none"
        baseline_model <- lavaan::lavaan(model = mod_base,
                                slotOptions = opt_baseline,
                                slotSampleStats = fit@SampleStats,
                                slotData = fit@Data)
      }
    lli_base <- lavaan::lavInspect(baseline_model,
                                    what = "loglik.casewise",
                                    drop.list.single.group = FALSE)
    lli_base <- unlist(lli_base, use.names = FALSE)[case_ids_order]
    tmp <- lavaan::fitMeasures(baseline_model, c("chisq", "df"))
    chisq_base <- unname(tmp["chisq"])
    df_base <- unname(tmp["df"])
    chisq_change_base <- 2 * (lli_h1 - lli_base)
    chisq_base_approx <- chisq_base - chisq_change_base
    if ("cfi" %in% fit_measures) {
        cfi_fit <- lavaan::fitMeasures(fit, "cfi",
                                       baseline.model = baseline_model)
        cfi_approx <- mapply(cfi, chisq_fit = chisq_fit_approx,
                                  chisq_baseline = chisq_base_approx,
                            MoreArgs = list(df_fit = df_fit,
                                            df_baseline = df_base))
        cfi_change <- cfi_fit - cfi_approx
        out[, "cfi"] <- cfi_change
      }
    if ("tli" %in% fit_measures) {
        tli_fit <- lavaan::fitMeasures(fit, "tli",
                                       baseline.model = baseline_model)
        tli_approx <- mapply(tli, chisq_fit = chisq_fit_approx,
                                  chisq_baseline = chisq_base_approx,
                            MoreArgs = list(df_fit = df_fit,
                                            df_baseline = df_base))
        tli_change <- tli_fit - tli_approx
        out[, "tli"] <- tli_change
      }
    if ("rmsea" %in% fit_measures) {
        rmsea_fit <- lavaan::fitMeasures(fit, "rmsea")
        # CHECK: The "n" of RMSEA in multiple group models.
        tmp <- (chisq_fit_approx / (n - 1)) / df_fit - 1 / (n - 1)
        rmsea_approx <- sqrt(pmax(tmp, 0, na.rm = TRUE))
        rmsea_change <- rmsea_fit - rmsea_approx
        out[, "rmsea"] <- rmsea_change
      }

    attr(out, "call") <- match.call()
    attr(out, "method") <- "approx"
    class(out) <- c("fit_measures_change", class(out))

    out

  }

#' @noRd

cfi <- function(chisq_fit, df_fit, chisq_baseline, df_baseline) {
    # Adapted from lavaan:::lav_fit_measures
    t1 <- max(c(chisq_fit - df_fit, 0))
    t2 <- max(c(chisq_fit - df_fit, chisq_baseline - df_baseline, 0))
    if (isTRUE(all.equal(t1, 0)) &&
        isTRUE(all.equal(t2, 0))) {
        return(1)
      } else {
        return(1 - t1 / t2)
      }
  }

#' @noRd

tli <- function(chisq_fit, df_fit, chisq_baseline, df_baseline) {
    # Adapted from lavaan:::lav_fit_measures
    t1 <- (chisq_fit - df_fit) * df_baseline
    t2 <- (chisq_baseline - df_baseline) * df_fit
    if (df_fit > 0 && abs(t2) > 0) {
          return(1 - t1 / t2)
      } else {
          return(1)
      }
  }