#' @title Rerun a 'lavaan' Analysis Using the Leaving-One-Out Approach
#'
#' @description Reruns a `lavaan` analysis several
#' times, each time with one case removed.
#'
#' @details [lavaan_rerun()] gets an [lavaan::lavaan()] output and
#' reruns the analysis *n0* times, using the same arguments and
#' options in the output, *n0* equals to the number of cases selected,
#' by default all cases in the analysis. In each
#' run, one case will be removed.
#'
#' Optionally, users can rerun the analysis with only selected cases
#' removed. These cases can be specified by case IDs, by Mahalanobis
#' distance computed from all variables used in the model, or by
#' Mahalanobis distance computed from the residuals (observed score -
#' implied scores) of observed outcome variables. See the help on the
#' arguments `to_rerun`, `md_top`, and `resid_md_top`.
#'
#' It is not recommended to use Mahalanobis distance computed from all
#' variables, especially for models with observed variables as
#' predictors (Pek & MacCallum, 2011). Cases that are extreme on
#' predictors may not be influential on the parameter estimates.
#' Nevertheless,
#' this distance is reported in some SEM programs and so this option
#' is provided.
#'
#' Mahalanobis distance based on residuals are supported for models
#' with no latent factors. The implied scores are computed by
#' [implied_scores()].
#'
#' If the sample size is large, it is recommended to use parallel
#' processing. However, it is possible that parallel
#' processing will fail. If this is the case, try to use serial
#' processing, by simply removing the argument `parallel` or set it to
#' `FALSE`.
#'
#' Many other functions in [semfindr] use the output from
#' [lavaan_rerun()]. Instead of running the *n* analyses every time, do
#' this step once and then users can compute whatever influence
#' statistics they want quickly.
#'
#' If the analysis took a few minutes to run due to the large number
#' of cases or the long processing time in fitting the model, it is
#' recommended to save the output to an external file (e.g., by
#' [base::saveRDS()]).
#'
#' Supports both single-group and multiple-group models.
#' (Support for multiple-group models available in 0.1.4.8 and later version).
#'
#' @param fit The output from [lavaan::lavaan()] or its wrappers (e.g.,
#' [lavaan::cfa()] and [lavaan::sem()]).
#'
#' @param case_id If it is a character vector of length equals to the
#' number of cases (the number of rows in the data in `fit`), then it
#' is the vector of case identification values. If it is `NULL`, the
#' default, then `case.idx` used by `lavaan` functions will be used
#' as case identification values. The case identification values will
#' be used to name the list of *n* output.
#'
#' @param to_rerun The cases to be processed. If `case_id` is
#' specified, this should be a subset of `case_id`. If `case_id` is
#' not specified, then this should be a vector of integers indicating
#' the rows to te processed, as appeared in the data in `fit`.
#' `to_rerun` cannot be used together with `md_top` or
#' `resid_md_top.`
#'
#' @param md_top The number of cases to be processed based on the
#' Mahalanobis distance computed on all observed variables used in
#' the model. The cases will be ranked from the largest to the
#' smallest distance, and the top `md_top` case(s) will be processed.
#' `md_top` cannot be used together with `to_rerun` or
#' `resid_md_top.`
#'
#' @param resid_md_top The number of cases to be processed based on
#' the Mahalanobis distance computed from the residuals of outcome
#' variables. The cases will be ranked from the largest to the
#' smallest distance, and the top `resid_md_top` case(s) will be
#' processed. `resid_md_top` cannot be used together with `to_rerun`
#' or `md_top.`
#'
#' @param allow_inadmissible If `TRUE`, accepts a fit object with
#' inadmissible results (i.e., `post.check` from
#' [lavaan::lavInspect()] is `FALSE`). Default is `FALSE`.
#'
#' @param skip_all_checks If `TRUE`, skips all checks and allow
#' users to run this function on any object of `lavaan` class.
#' For users to experiment this and other functions on models
#' not officially supported. Default is `FALSE`.
#'
#' @param parallel Whether parallel will be used. If `TRUE`, will use
#' functions in the `parallel` package to rerun the analysis.
#' Currently, only support `"snow"` type clusters using local CPU
#' cores. Default is `FALSE`.
#'
#' @param ncores The number of CPU cores
#' to use if parallel processing is
#' requested. Default is `NULL`, and the
#' number of cores is determine by
#' `makeCluster_args`. If set to an
#' integer, this number will override
#' the setting (`spec`) in
#' `makeCluster_args`.
#'
#' @param makeCluster_args A named list of arguments to be passed to
#' [parallel::makeCluster()]. Default is `list(spec =
#' getOption("cl.cores", 2)))`. If only the number of cores need to
#' be specified, use `list(spec = x)`, where `x` is the number
#' of cores to use. Alternatively, set `ncores` and its
#' value will be used in `spec`.
#'
#' @param progress If `TRUE`, the default,
#' progress will be displayed on screen.
#'
#' @param rerun_method How fit will be rerun. Default is
#' `"lavaan"`. An alternative method is `"update"`. For
#' internal use. If `"lavaan"` returns an error, try setting
#' this argument to `"update"`.
#'
#' @return
#' A `lavaan_rerun`-class object, which is a list with the following elements:
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
#' - `converged`: A vector of length equals to *n*. Each analysis was
#'   checked by [lavaan::lavTech]`(x, "converged")`, `x` being the
#'   `lavaan` results. The results of this test are stored in this
#'   vector. If the value is `TRUE`, the estimation converged. If
#'   not `TRUE`, then the estimation failed to converge if the corresponding
#'   case is excluded.
#'
#' - `call`: The call to [lavaan_rerun()].
#'
#' - `selected`: A numeric vector of the row numbers of cases selected
#'   in the analysis. Its length should be equal to the length of
#'   `rerun`.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>.
#'
#' @examples
#' library(lavaan)
#' dat <- pa_dat
#' # For illustration, select only the first 50 cases
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
#'
#' # Fit the model n times. Each time with one case removed.
#' fit_rerun <- lavaan_rerun(fit, parallel = FALSE)
#'
#' # Print the output for a brief description of the runs
#' fit_rerun
#'
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
                         ncores = NULL,
                         makeCluster_args =
                            list(spec = getOption("cl.cores", 2)),
                         progress = TRUE,
                         rerun_method = c("lavaan", "update")
                         ) {
  # Create the call
  # Create the boot function
  # Run it n times
  # Return the results

  rerun_method <- match.arg(rerun_method)

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

  ngroups <- lavaan::lavInspect(fit, "ngroups")
  if (ngroups > 1) {
      n_j <- sapply(lavaan::lavInspect(fit, "data"), nrow)
      n <- sum(n_j)
    } else {
      n <- nrow(lavaan::lavInspect(fit, "data"))
      n_j <- n
    }
  n_orig <- sum(lavaan::lavInspect(fit, "norig"))

  if (is.null(case_id)) {
      case_ids <- lavaan::lavInspect(fit, "case.idx",
                                     drop.list.single.group = FALSE)
      case_ids <- sort(unlist(case_ids, use.names = FALSE))
    } else {
      case_ids <- lavaan::lavInspect(fit, "case.idx",
                                    drop.list.single.group = FALSE)
      case_ids <- sort(unlist(case_ids, use.names = FALSE))
      if (length(case_id) != n_orig) {
          stop("The length of case_id is not equal to the number of cases.")
        } else {
          case_ids <- case_id[case_ids]
        }
    }

  if (sum(!missing(to_rerun), !missing(md_top), !missing(resid_md_top)) > 1) {
      stop("Among to_rerun, md_top, and resid_md_top, only one of them can be specified.")
    }

  # It does support a model without mean structure.
  # if (!missing(resid_md_top) & !lavaan::lavInspect(fit, "meanstructure")) {
  #     stop("resid_md_top does not support a model without mean structure.")
  #   }

  tmp <- sapply(lavaan::lavInspect(fit, "pattern",
                drop.list.single.group = FALSE),
              nrow)
  if (!missing(resid_md_top) && !all(tmp == 1)) {
      stop("resid_md_top does not support analysis with missing data.")
    }

  if (!missing(to_rerun)) {
      if (!is.null(case_id)) {
          if (!all(to_rerun %in% case_id)) {
              stop("Some elements in to_rerun is not in the case_id vectors.")
            }
        } else {
          if (!all(to_rerun %in% seq_len(n_orig))) {
              stop("Some elements in to_rerun is not valid row numbers.")
            }
          if (!all(to_rerun %in% case_ids)) {
              stop("Some cases in to_rerun is not used in lavaan output. Probably due to listwise deletion.")
            }
          to_reun_org <- to_rerun
          to_rerun <- match(to_rerun, case_ids)
        }
    } else {
      to_rerun <- order(unlist(case_ids, use.names = FALSE))
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
      # fit_data <- lavaan::lavInspect(fit, "data")
      # fit_implied <- implied_scores(fit)
      # fit_observed <- fit_data[, colnames(fit_implied)]
      # fit_residual <- fit_implied - fit_observed
      # fit_resid_md <- stats::mahalanobis(fit_residual,
      #                                    colMeans(fit_residual),
      #                                    stats::cov(fit_residual))
      fit_implied <- implied_scores(fit, output = "list")
      y_names <- colnames(fit_implied[[1]])
      fit_observed <- lapply(lavaan::lavInspect(fit, "data",
                                                drop.list.single.group = FALSE),
                             function(x) x[, y_names])
      fit_residual <- mapply(function(x1, x2) {x1 - x2},
                             x1 = fit_implied,
                             x2 = fit_observed,
                             SIMPLIFY = FALSE)
      fit_resid_md <- lapply(fit_residual,
                             function(x) {
                                 stats::mahalanobis(x,
                                             colMeans(x),
                                             stats::cov(x))
                               })
      fit_resid_md <- unlist(fit_resid_md, use.names = FALSE)
      tmp1 <- lavaan::lavInspect(fit, "case.idx",
                                 drop.list.single.group = FALSE)
      tmp2 <- sort(unlist(tmp1, use.names = FALSE))
      if (ngroups > 1) {
          tmp <- order(unlist(tmp1, use.names = FALSE))
          fit_resid_md <- fit_resid_md[tmp]
        }
      names(fit_resid_md) <- tmp2
      fit_resid_md_ordered <- order(fit_resid_md, decreasing = TRUE, na.last = NA)
      fit_resid_md_ordered <- fit_resid_md_ordered[!is.na(fit_resid_md_ordered)]
      fit_resid_md_selected <- fit_resid_md_ordered[seq_len(resid_md_top)]
      fit_resid_md_selected <- fit_resid_md_selected[!is.na(fit_resid_md_selected)]
      if (!is.null(case_id)) {
          to_rerun <- case_ids[fit_resid_md_selected]
        } else {
          to_rerun <- fit_resid_md_selected
        }
    }
  # listwise:
  #  to_rerun:
  #    no case_id: The positions in the *listwise* dataset
  #    case_id: The case id to rerun
  if (!is.null(case_id)) {
      case_ids <- to_rerun
      id_to_rerun <- match(to_rerun, case_id)
    } else {
      case_ids <- case_ids[to_rerun]
      tmp <- sort(unlist(lavaan::lavInspect(fit, "case.idx",
                    drop.list.single.group = FALSE),
                    use.names = FALSE))
      id_to_rerun <- tmp[to_rerun]
    }
  fit_total_time <- lavaan::lavInspect(fit, "timing")$total
  lav_case_idx <- sort(unlist(lavaan::lavInspect(fit, "case.idx",
                    drop.list.single.group = FALSE),
                    use.names = FALSE))
  if (rerun_method == "lavaan") {
      rerun_i <- gen_fct_use_lavaan(fit, lav_case_idx = lav_case_idx)
    }
  if (rerun_method == "update") {
      environment(gen_fct_use_update) <- parent.frame()
      rerun_i <- gen_fct_use_update(fit, lav_case_idx = lav_case_idx)
    }
  rerun_test <- suppressWarnings(rerun_i(NULL))
  if (!isTRUE(all.equal(unclass(coef(fit)),
                        coef(rerun_test)[names(coef(fit))]))) {
      stop("Something is wrong. The lavaan analysis cannot be rerun.")
    }

  if (parallel && !is.null(ncores)) {
    makeCluster_args <- utils::modifyList(makeCluster_args,
                                          list(spec = ncores))
  }

  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
      pkgs <- .packages()
      pkgs <- rev(pkgs)
      cl <- do.call(parallel::makeCluster, makeCluster_args)
      on.exit(try(parallel::stopCluster(cl), silent = TRUE))
      time_expected <-  1.5 * length(id_to_rerun) * fit_total_time[[1]] / length(cl)
      if (progress) {
        message(paste0("The expected CPU time is ", round(time_expected, 2),
                      " second(s)."))
      }
      utils::flush.console()
      parallel::clusterExport(cl, "pkgs", envir = environment())
      parallel::clusterEvalQ(cl, {
                      sapply(pkgs,
                      function(x) library(x, character.only = TRUE))
                    })
      if (progress) {
        rt <- system.time(out <- suppressWarnings(
                                    pbapply::pblapply(id_to_rerun,
                                                      FUN = rerun_i,
                                                      cl = cl)))
      } else {
        rt <- system.time(out <- suppressWarnings(
                            parallel::parLapplyLB(cl, id_to_rerun, rerun_i)))
      }
      parallel::stopCluster(cl)

    } else {
      time_expected <-  1.5 * length(id_to_rerun) * fit_total_time[[1]]
      if (progress) {
        message(paste0("The expected CPU time is ", round(time_expected, 2),
                      " second(s).\n",
                      "Could be faster if run in parallel."))
      }
      utils::flush.console()
      if (progress) {
        rt <- system.time(out <- suppressWarnings(
                                    pbapply::pblapply(id_to_rerun,
                                                      FUN = rerun_i)))
      } else {
        rt <- system.time(out <- suppressWarnings(lapply(id_to_rerun, rerun_i)))
      }
    }

  if (rt[[3]] > 60) {
      message(paste0("Note: The rerun took more than one minute. ",
                     "Consider saving the output to an external file. ",
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
      message(paste0("Note: Some cases led to warnings if excluded. ",
                    "Please check the element 'post_check' ",
                    "for cases with values other than `TRUE`."))
      utils::flush.console()
    }

  # converged
  converged <- sapply(out, function(x) {
                      lavaan::lavTech(x, what = "converged")
                    })
  any_not_converged <- !all(sapply(post_check, isTRUE))
  if (any_not_converged) {
      message(paste0("Note: Some cases led to nonconvergence if excluded. ",
                     "Please check the element 'converged' ",
                     "for cases with values other than `TRUE`."))
      utils::flush.console()
    }

  out <- list(rerun = out,
              fit = fit,
              post_check = post_check,
              converged = converged,
              call = call,
              selected = id_to_rerun)
  class(out) <- "lavaan_rerun"
  out
}

gen_fct_use_lavaan <- function(fit,
                               lav_case_idx) {
  slot_opt <- fit@Options
  slot_pat <- data.frame(fit@ParTable)
  slot_pat$est <- NULL
  slot_pat$start <- NULL
  slot_mod <- fit@Model
  data_full <- lav_data_used(fit)
  ngroups <- lavaan::lavInspect(fit, "ngroups")
  if (ngroups > 1) {
      gp_var <- lavaan::lavInspect(fit, "group")
      gp_label <- lavaan::lavInspect(fit, "group.label")
      slot_opt$group.label <- gp_label
      out <- function(i = NULL) {
          if (is.null(i)) {
              return(lavaan::lavaan(data = data_full,
                                    model = slot_pat,
                                    group = gp_var,
                                    group.label = gp_label,
                                    slotOptions = slot_opt))
            } else {
              i1 <- match(i, lav_case_idx)
              return(lavaan::lavaan(data = data_full[-i1, ],
                                    model = slot_pat,
                                    group = gp_var,
                                    group.label = gp_label,
                                    slotOptions = slot_opt))
            }
        }
    } else {
      out <- function(i = NULL) {
          if (is.null(i)) {
              return(lavaan::lavaan(data = data_full,
                                    model = slot_pat,
                                    slotOptions = slot_opt))
            } else {
              i1 <- match(i, lav_case_idx)
              return(lavaan::lavaan(data = data_full[-i1, ],
                                    model = slot_pat,
                                    slotOptions = slot_opt))
            }
        }
    }
  return(out)
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

gen_fct_use_update <- function(fit) {
  fit_org <- eval(fit)
  data_full <- lavaan::lavInspect(fit_org, "data")
  function(i = NULL) {
      if (is.null(i)) {
          return(lavaan::update(fit_org, data = data_full))
        } else {
          return(lavaan::update(fit_org, data = data_full[-i, ]))
        }
    }
  }
