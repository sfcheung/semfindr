#' @title
#' Rerun a `lavaan` analysis with one-left-out
#'
#' @description
#' Rerun a `lavaan` analysis *n* times, each time with one case left out.
#'
#' @details
#' Rerun a `lavaan` analysis using the same arguments and options *n* times,
#'  *n*
#' equal to the number of cases. In each run, one case will be left out.
#'
#' Currently only work for one group analysis.
#'
#' @param fit The output from `lavaan`, such as [lavaan::cfa()] and
#'        [lavaan::sem()].
#' @param case_id If this
#'               is a character vector of
#'               length equal to the number of cases (the number of rows in the
#'               data), then it is the vector of case identification values.
#'               If this is the `NULL`, the default, then `case.idx` used
#'               by `lavaan` functions will be used as case identification
#'               values. The case identification
#'               values will be used to name the list of `n` output.
#' @param parallel Whether parallel will be used. If `TRUE`, will use
#'                 parallel to rerun the analysis. Currently, only support
#'                 `"FORK"` type cluster using local CPU cores. Default is
#'                 `FALSE`.
#' @param makeCluster_args A named list of arguments to be passed to
#'                       [parallel::makeCluster()]. Default is
#'                        `list(spec = getOption("cl.cores", 2)))`. If
#'                        only need to specify the number of cores, use
#'                        `list(spec = ncpu)`, where `ncpu` is the number of
#'                        cores to use.
#'
#' @return
#' Return a list with two elements
#'
#' - `rerun`: The *n* `lavaan` output objects.
#'
#' - `fit`: The original output from `lavaan`.
#'
#' - `post_check`: A list of length equals to *n*. Each analysis was checked by
#'                 [lavaan::lavTech()]. If `TRUE`, the estimation converged
#'                 and the solution is admissible. If not `TRUE`, it is a
#'                 warning message issued by [lavaan::lavTech()].
#'
#' - `call`: The call to [lavaan_rerun()].
#'
#' @examples
#' library(lavaan)
#' dat <- pa_dat
#' # For illustration only, select only the first 50 cases
#' dat <- dat[1:50, ]
#' # The model
#' mod <-
#' '
#' m1 ~ iv1 + iv2
#' dv ~ m1
#' '
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
#' @importMethodsFrom lavaan coef
#' @export lavaan_rerun

lavaan_rerun <- function(fit,
                         case_id = NULL,
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

  if (fit@Data@ngroups != 1) {
      stop(paste0("The output is based on more than one group. \n",
                  "Multiple group analysis not yet supported."))
    }

  n <- fit@Data@nobs[[1]]

  if (is.null(case_id)) {
      case_ids <- fit@Data@case.idx[[1]]
    } else {
      if (length(case_id) != n) {
          stop("The length of case_id is not equal to the number of cases.")
        } else {
          case_ids <- case_id
        }
    }

  fit_total_time <- fit@timing$total
  time_expected <-  n * fit_total_time[[1]]
  message(paste0("The expected CPU time is ", round(time_expected, 2),
                 " second(s).\n",
                 "Could be faster if ran in parallel."))
  utils::flush.console()
  environment(gen_fct) <- parent.frame()
  rerun_i <- gen_fct(fit)
  rerun_test <- rerun_i(NULL)
  if (!identical(coef(fit), coef(rerun_test))) {
      stop("Something is wrong. The lavaan analysis cannot be rerun.")
    }

  if (parallel & requireNamespace("parallel", quietly = TRUE)) {
      pkgs <- .packages()
      pkgs <- rev(pkgs)
      cl <- do.call(parallel::makeCluster, makeCluster_args)
      parallel::clusterExport(cl, "pkgs", envir = environment())
      parallel::clusterEvalQ(cl, {
                      sapply(pkgs,
                      function(x) library(x, character.only = TRUE))
                    })
      rt <- system.time(out <- suppressWarnings(
                          parallel::parLapplyLB(cl, seq_len(n), rerun_i)))
      parallel::stopCluster(cl)

    } else {
      rt <- system.time(out <- suppressWarnings(lapply(seq_len(n), rerun_i)))
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
              call = call)
  class(out) <- "lavaan_rerun"
  out
}

gen_fct_old <- function(fit) {
  fit_call <- fit@call
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
  data_full <- fit_org@Data@X[[1]]
  colnames(data_full) <- fit_org@Data@ov$name
  function(i = NULL) {
      if (is.null(i)) {
          return(lavaan::update(fit_org, data = data_full))
        } else {
          return(lavaan::update(fit_org, data = data_full[-i, ]))
        }
    }
  }
