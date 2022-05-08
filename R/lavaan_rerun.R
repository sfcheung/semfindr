#' @title Rerun a `lavaan` Analysis (Leaving-One-Out-Each-Time)
#'
#' @description [lavaan_rerun()] reruns a `lavaan` analysis several
#' times, each time with one case removed.
#'
#' @details [lavaan_rerun()] gets an [lavaan::lavaan()] output and
#'  reruns the analysis *n* times, using the same arguments and
#'  options in the output, *n* equals to the number of cases. In each
#'  run, one case will be removed.
#'
#' Optionally, users can rerun the analysis with only selected cases
#'  removed. These cases can be specified by case IDs, by Mahalanobis
#'  distance computed from all variables used in the model, or by
#'  Mahalanobis distance computed from the residuals (observed score -
#'  implied scores) of observed outcome variables. See the help on the
#'  arguments `to_rerun`, `md_top`, and `resid_md_top`.
#'
#' It is not recommended to use Mahalanobis distance computed from all
#'  variables, especially for models with observed variables as
#'  predictors (Pek & MacCallum, 2011). Cases that are extreme on
#'  predictors may not be influential on the results. Nevertheless,
#'  this distance is reported in some SEM programs and so this option
#'  is provided.
#'
#' Mahalanobis distance based on residuals are supported for models
#'  with no latent factors. The implied scores are computed by
#'  [implied_scores()].
#'
#' If the sample size is large, it is recommended to use parallel
#'  processing. However, due to the nature of the [update()] method
#'  for [lavaan::lavaan-class] object, it is possible that parallel
#'  processing will fail. If this is the case, try to use serial
#'  processing, by simply remove the argument `parallel` or set it to
#'  `FALSE`.
#'
#' Many other functions in [semfindr] use the output from
#'  [lavaan_rerun()]. Instead of running the *n* analyses every time, do
#'  this step once and then users can compute whatever influence
#'  statistics they want quickly.
#'
#' If the analysis took a few minutes to run due to the large number
#'  of cases or the long processing time in fitting the model, it is
#'  recommended to save the output to an external file (e.g., by
#'  [base::saveRDS()]).
#'
#' Currently [lavaan_rerun()] only supports single-group models.
#'
#' @param fit The output from [lavaan::lavaan()], such as
#'  [lavaan::cfa()] and [lavaan::sem()].
#' @param case_id If it is a character vector of length equals to the
#'  number of cases (the number of rows in the data in `fit`), then it
#'  is the vector of case identification values. If it is `NULL`, the
#'  default, then `case.idx` used by `lavaan` functions will be used
#'  as case identification values. The case identification values will
#'  be used to name the list of *n* output.
#' @param to_rerun The cases to be processed. If `case_id` is
#'  specified, this should be a subset of `case_id`. If `case_id` is
#'  not specified, then this should be a vector of integers indicating
#'  the rows to te processed, as appeared in the data in `fit`. 
#'  `to_rerun` cannot be used together with `md_top` or
#'  `resid_md_top.`
#' @param md_top The number of cases to be processed based on the
#'  Mahalanobis distance computed on all observed variables used in
#'  the model. The cases will be ranked from the largest to the
#'  smallest distance, and the top `md_top` case(s) will be processed.
#'  `md_top` cannot be used together with `to_rerun` or
#'  `resid_md_top.`
#' @param resid_md_top The number of cases to be processed based on
#'  the Mahalanobis distance computed from the residuals of outcome
#'  variables. The cases will be ranked from the largest to the
#'  smallest distance, and the top `resid_md_top` case(s) will be
#'  processed. `resid_md_top` cannot be used together with `to_rerun`
#'  or `md_top.`
#' @param allow_inadmissible If `TRUE`, accepts a fit object with
#'  inadmissible results (i.e., `post.check` from
#'  [lavaan::lavInspect()] is `FALSE`). Default is `FALSE`.
#' @param skip_all_checks If `TRUE`, skips all checks and allow
#'  users to run this function on any object of `lavaan` class.
#'  For users to experiment this and other functions on models
#'  not officially supported. Default is `FALSE`.
#' @param parallel Whether parallel will be used. If `TRUE`, will use
#'  functions in the `parallel` package to rerun the analysis.
#'  Currently, only support `"snow"` type clusters using local CPU
#'  cores. Default is `FALSE`.
#' @param makeCluster_args A named list of arguments to be passed to
#'  [parallel::makeCluster()]. Default is `list(spec =
#'  getOption("cl.cores", 2)))`. If only the number of cores need to
#'  be specified, use `list(spec = x)`, where `x` is the number
#'  of cores to use.
#'
#' @return
#' A list with the following elements:
#'
#' - `rerun`: The *n* `lavaan` output objects.
#'
#' - `fit`: The original output from `lavaan`.
#'
#' - `post_check`: A list of length equals to *n*. Each analysis was
#'   checked by [lavaan::lavTech]`(x, "post.check")`, `x` being the
#'   `lavaan` results. The results of this test are stored in this
#'   list. If the value is `TRUE`, the estimation converged and the
#'   solution is admissible. If not `TRUE`, it is a warning message
#'   issued by [lavaan::lavTech()].
#'
#' - `call`: The call to [lavaan_rerun()].
#'
#' - `selected`: A numeric vector of the row numbers of cases selected
#'   in the analysis. Its length should be equal to the length of
#'   `rerun`. 
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
#' # Results excluding the first case
#' fitMeasures(fit_rerun$rerun[[1]], c("chisq", "cfi", "tli", "rmsea"))
#' # Results by manually excluding the first case
#' fit_01 <- lavaan::sem(mod, dat[-1, ])
#' fitMeasures(fit_01, c("chisq", "cfi", "tli", "rmsea"))
#' 
#' @importMethodsFrom lavaan coef
#' @export lavaan_rerun

lavaan_rerun <- function(fit,
                         case_id = NULL,
                         to_rerun,
                         md_top,
                         resid_md_top,
                         allow_inadmissible = FALSE,
                         skip_all_checks = FALSE,
                         parallel = FALSE,
                         makeCluster_args =
                            list(spec = getOption("cl.cores", 2))
                         ) {
  # Create the call
  # Create the boot function
  # Run it n times
  # Return the results

  call <- match.call()

  if (missing(fit)) {
      stop("lavaan output is missing.")
    }

  if (!inherits(fit, "lavaan")) {
      stop("The fit object is not a lavaan output.")
    }

  if (!skip_all_checks) {
    check_out <- lavaan_rerun_check(fit, print_messages = FALSE)

    if (check_out != 0) {
        if ((check_out == -1) &&
            !(suppressWarnings(lavaan::lavInspect(fit, "post.check"))) &&
            allow_inadmissible) {
          } else {
            stop(attr(check_out, "info"))
          }
      }
    }

  n <- nrow(lavaan::lavInspect(fit, "data"))

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

  if (sum(!missing(to_rerun), !missing(md_top), !missing(resid_md_top)) > 1) {
      stop("Among to_rerun, md_top, and resid_md_top, only one of them can be specified.")
    }

  # It does support a model without mean structure.
  # if (!missing(resid_md_top) & !lavaan::lavInspect(fit, "meanstructure")) {
  #     stop("resid_md_top does not support a model without mean structure.")
  #   }

  if (!missing(resid_md_top) & !all(lavaan::lavInspect(fit, "pattern") == 1) ) {
      stop("resid_md_top does not support analysis with missing data.")
    }

  if (!missing(to_rerun)) {
      if (!is.null(case_id)) {
          if (!all(to_rerun %in% case_id)) {
              stop("Some elements in to_rerun is not in the case_id vectors.")
            }
        } else {
          if (!all(to_rerun %in% seq_len(n))) {
              stop("Some elements in to_rerun is not valid row numbers.")
            }
        }
    } else {
      to_rerun <- case_ids
    }

  if (!missing(md_top)) {
      case_md <- as.vector(mahalanobis_rerun(fit))
      case_md_ordered <- order(case_md, decreasing = TRUE, na.last = NA)
      case_md_ordered <- case_md_ordered[!is.na(case_md_ordered)]
      case_md_selected <- case_md_ordered[seq_len(md_top)]
      case_md_selected <- case_md_selected[!is.na(case_md_selected)] 
      to_rerun <- case_ids[case_md_selected]
    }

  if (!missing(resid_md_top)) {
      fit_data <- lavaan::lavInspect(fit, "data")
      fit_implied <- implied_scores(fit)
      fit_observed <- fit_data[, colnames(fit_implied)]
      fit_residual <- fit_implied - fit_observed
      fit_resid_md <- stats::mahalanobis(fit_residual,
                                         colMeans(fit_residual),
                                         stats::cov(fit_residual))
      fit_resid_md_ordered <- order(fit_resid_md, decreasing = TRUE, na.last = NA)
      fit_resid_md_ordered <- fit_resid_md_ordered[!is.na(fit_resid_md_ordered)]
      fit_resid_md_selected <- fit_resid_md_ordered[seq_len(resid_md_top)]
      fit_resid_md_selected <- fit_resid_md_selected[!is.na(fit_resid_md_selected)] 
      to_rerun <- case_ids[fit_resid_md_selected]
    }

  if (!is.null(case_id)) {
      case_ids <- to_rerun
      id_to_rerun <- match(to_rerun, case_id)
    } else {
      case_ids <- lavaan::lavInspect(fit, "case.idx")[to_rerun]
      id_to_rerun <- to_rerun
    }
  fit_total_time <- lavaan::lavInspect(fit, "timing")$total
  environment(gen_fct) <- parent.frame()
  rerun_i <- gen_fct(fit)
  rerun_test <- suppressWarnings(rerun_i(NULL))
  if (!all.equal(unclass(coef(fit)), coef(rerun_test)[names(coef(fit))])) {
      stop("Something is wrong. The lavaan analysis cannot be rerun.")
    }

  if (parallel & requireNamespace("parallel", quietly = TRUE)) {
      pkgs <- .packages()
      pkgs <- rev(pkgs)
      cl <- do.call(parallel::makeCluster, makeCluster_args)
      time_expected <-  length(id_to_rerun) * fit_total_time[[1]] / length(cl)
      message(paste0("The expected CPU time is ", round(time_expected, 2),
                    " second(s)."))
      utils::flush.console()
      parallel::clusterExport(cl, "pkgs", envir = environment())
      parallel::clusterEvalQ(cl, {
                      sapply(pkgs,
                      function(x) library(x, character.only = TRUE))
                    })
      rt <- system.time(out <- suppressWarnings(
                          parallel::parLapplyLB(cl, id_to_rerun, rerun_i)))
      parallel::stopCluster(cl)

    } else {
      time_expected <-  length(id_to_rerun) * fit_total_time[[1]]
      message(paste0("The expected CPU time is ", round(time_expected, 2),
                    " second(s).\n",
                    "Could be faster if ran in parallel."))
      utils::flush.console()
      rt <- system.time(out <- suppressWarnings(lapply(id_to_rerun, rerun_i)))
    }

  if (rt[[3]] > 60) {
      message(paste0("The rerun took more than one minute.\n",
                     "Consider saving the output to an external file.\n",
                     "E.g., can use saveRDS() to save the object."))
      utils::flush.console()
    }
  names(out) <- case_ids

  # post.check
  post_check <- sapply(out, function(x) {
                    chk <- tryCatch(lavaan::lavTech(x, what = "post.check"),
                                    warning = function(w) w)
                    })
  any_warning <- !all(sapply(post_check, isTRUE))
  if (any_warning) {
      message(paste0("Some cases led to warnings if excluded.\n",
                    "Please check the element 'post_check'\n",
                    "for cases with values other than `TRUE`."))
      utils::flush.console()
    }

  out <- list(rerun = out,
              fit = fit,
              post_check = post_check,
              call = call,
              selected = id_to_rerun)
  class(out) <- "lavaan_rerun"
  out
}

gen_fct_old <- function(fit) {
  fit_call <- as.call(lavaan::lavInspect(fit, "call"))
  fit_call2 <- fit_call
  for (i in seq_len(length(fit_call2))) {
      fit_call2[[i]] <- eval(fit_call[[i]])
    }
  data_full <- fit_call2$data
  function(i = NULL) {
      if (is.null(i)) {
          return(eval(fit_call2))
        } else {
          fit_call2$data <- data_full[-i, ]
          return(eval(fit_call2))
        }
    }
  }

gen_fct <- function(fit) {
  fit_org <- eval(fit)
  # data_full <- fit_org@Data@X[[1]]
  data_full <- lavaan::lavInspect(fit_org, "data")
  # colnames(data_full) <- fit_org@Data@ov$name
  colnames(data_full) <- lavaan::lavNames(fit_org)
  function(i = NULL) {
      if (is.null(i)) {
          return(lavaan::update(fit_org, data = data_full))
        } else {
          return(lavaan::update(fit_org, data = data_full[-i, ]))
        }
    }
  }
