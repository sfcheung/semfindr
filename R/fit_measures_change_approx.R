#' @title Case Influence on Fit Measures (Approximated)
#'
#' @description Gets a [lavaan::lavaan()] output and computes the
#'  approximated change
#'  in selected fit measures if a case is deleted
#'
#' @details For each case, [fit_measures_change_approx()] computes the
#'  approximated differences in selected fit measures with and
#'  without this case.
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
#' Currently it only supports single-group models.
#'
#' @param fit The output from [lavaan::lavaan()].
#' @param fit_measures The argument `fit.measures` used in
#'  [lavaan::fitMeasures]. Default is
#'  `c("chisq", "cfi", "rmsea", "tli")`.
#' @param baseline_model The argument `baseline.model` used in
#'  [lavaan::fitMeasures]. Default is `NULL`.
#' @param case_id If it is a character vector of length equals to the
#'  number of cases (the number of rows in the data in `fit`), then it
#'  is the vector of case identification values. If it is `NULL`, the
#'  default, then `case.idx` used by `lavaan` functions will be used
#'  as case identification values.
#'
#' @return A matrix with the number of columns equals to the number of
#'  requested fit measures, and the number of rows equals to the number
#'  of cases. The row names are case identification values.
#'
#' @author Idea by Mark Hok Chio Lai <https://orcid.org/0000-0002-9196-7406>,
#'         Implemented by Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>.
#'
#'
#' @examples
#' library(lavaan)
#' dat <- pa_dat
#' # The model
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
#' # Approximated changes
#' out_approx <- fit_measures_change_approx(fit, fit_measures = "chisq")
#' head(out_approx)
#' # Fit the model several times. Each time with one case removed.
#' # For illustration, do this only for four selected cases
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE,
#'                           to_rerun = 1:10)
#' # Compute the changes in chisq if a case is removed
#' out <- fit_measures_change(fit_rerun, fit_measures = "chisq")
#' # Results excluding a case, for the first few cases
#' head(out)
#' # Compare the results
#' plot(out_approx[1:10, "chisq"], out)
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
#'                           to_rerun = 1:10)
#' # Compute the changes in chisq if a case is removed
#' out <- fit_measures_change(fit_rerun, fit_measures = "chisq")
#' # Results excluding a case, for the first few cases
#' head(out)
#' # Compare the results
#' plot(out_approx[1:10, "chisq"], out)
#'
#' # A latent variable model#'
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
#'                           to_rerun = 1:10)
#' # Compute the changes in chisq if a case is removed
#' out <- fit_measures_change(fit_rerun, fit_measures = "chisq")
#' # Results excluding a case, for the first few cases
#' head(out)
#' # Compare the results
#' plot(out_approx[1:10, "chisq"], out)
#'
#' @importFrom methods .hasSlot
#' @export

fit_measures_change_approx <- function(fit,
                         fit_measures = c("chisq", "cfi", "rmsea", "tli"),
                         baseline_model = NULL,
                         case_id = NULL
                         ) {
    if (length(fit_measures) == 0) stop("No measure is requested.")
    if (missing(fit)) {
        stop("No lavaan output supplied.")
      }
    if (!inherits(fit, "lavaan")) {
        stop("The fit object is not a lavaan output.")
      }
    n <- lavaan::lavTech(fit, "nobs")
    if (is.null(case_id)) {
        # Assume the model is a single-group model
        case_ids <- lavaan::lavInspect(fit, "case.idx")
      } else {
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
    lli_h1 <- lavaan::lavInspect(fit_h1, what = "loglik.casewise")
    lli <- lavaan::lavInspect(fit, what = "loglik.casewise")
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
                                    what = "loglik.casewise")
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
        tmp <- (chisq_fit_approx / (n - 1)) / df_fit - 1 / (n - 1)
        rmsea_approx <- sqrt(pmax(tmp, 0, na.rm = TRUE))
        rmsea_change <- rmsea_fit - rmsea_approx
        out[, "rmsea"] <- rmsea_change
      }
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