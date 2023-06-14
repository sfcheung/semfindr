#' @title Compatibility Check for 'lavaan_rerun'
#'
#' @description Gets a 'lavaan' output and checks whether it is
#' supported by [lavaan_rerun()].
#'
#' @details This function is not supposed to be used by users. It is
#' called by [lavaan_rerun()] to see if the analysis being passed to
#' it is supported. If not, messages will be printed to indicate why.
#'
#' @param fit The output from `lavaan`, such as [lavaan::cfa()] and
#' [lavaan::sem()].
#'
#' @param print_messages Logical. If `TRUE`, will print messages about the
#' check. If `FALSE`, the messages will be attached to the return value
#' as an attribute. Default is `TRUE`.
#'
#' @return A single-element vector. If confirmed to be supported, will
#' return 0. If not confirmed be support but may still work, return 1.
#' If confirmed to be not yet supported, will return a negative
#' number, the value of this number without the negative sign is the
#' number of tests failed.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>.
#'
#' @examples
#'
#' dat <- cfa_dat
#'
#' mod <-
#' "
#' f1 =~ x4 + x5 + x6
#' "
#' dat_gp <- dat
#' dat$gp <- rep(c("gp1", "gp2"), length.out = nrow(dat_gp))
#'
#' fit01 <- lavaan::sem(mod, dat)
#' # If supported, returns a zero.
#' lavaan_rerun_check(fit01)
#'
#' fit05 <- lavaan::cfa(mod, dat, group = "gp")
#' # If not supported, returns a negative number.
#' lavaan_rerun_check(fit05)
#'
#'@export

lavaan_rerun_check <- function(fit,
                               print_messages = TRUE) {

    p_table <- lavaan::parameterTable(fit)

    sem_options <- lavaan::lavInspect(fit, "options")
    sem_estimator <- sem_options$estimator
    sem_se <- sem_options$se
    sem_test <- sem_options$test
    sem_missing   <- sem_options$missing
    sem_converged <- lavaan::lavInspect(fit, "converged")
    sem_post_check <- suppressWarnings(lavaan::lavInspect(fit, "post.check"))
    sem_lavaan_ver <- lavaan::lavInspect(fit, "version")
    sem_lavaan_ordered <- lavaan::lavInspect(fit, "ordered")
    sem_ngroups <- lavaan::lavInspect(fit, "ngroups")
    sem_nlevels <- lavaan::lavInspect(fit, "nlevels")
    sem_max_nclusters <- max(unlist(lavaan::lavInspect(fit, "nclusters")))
    sem_data <- tryCatch(lavaan::lavInspect(fit, "data"), error = function(e) e)

    # `lavaan_rerun` does not concern what estimation method was used
    # `lavaan_rerun` does not concern the SE method
    # `lavaan_rerun` does not concern the robust method used, if any
    # `lavaan_rerun` does not concern the method for missing data

    model_formative_factor <- "<~" %in% p_table$op
    model_multilevel <- (sem_nlevels > 1)
    model_multicluster <- (sem_max_nclusters > 1)
    model_multigroup <- (sem_ngroups > 1)
    model_ordered <- (length(sem_lavaan_ordered) > 0)

    optim_converged <- sem_converged
    optim_admissible <- sem_post_check

    out <- 0
    msg <- NULL

    if (model_multilevel) {
          out <- ifelse(out >= 0, -1, out - 1)
          msg <- c(msg, "Multilevel models are not yet supported.")
        }

    if (model_multicluster) {
          out <- ifelse(out >= 0, -1, out - 1)
          msg <- c(msg, "Clustered models are not yet supported.")
        }

    # if (model_multigroup) {
    #       out <- ifelse(out >= 0, -1, out - 1)
    #       msg <- c(msg, "Multigroup models are not yet supported.")
    #     }

    if (!optim_converged) {
          out <- ifelse(out >= 0, -1, out - 1)
          msg <- c(msg,
                  "The estimation has not converged. Fix the estimation first.")
        }

    if (!optim_admissible) {
          out <- ifelse(out >= 0, -1, out - 1)
          msg <- c(msg,
                paste("The solution is not admissible by lavaan post.check.",
                        "Check the SEM results first."))
        }

    if (inherits(sem_data, "simpleError")) {
          out <- ifelse(out >= 0, -1, out - 1)
          msg <- c(msg,
                paste("Raw data cannot be retrieved. Only a model fitted to",
                        "raw data is supported."))
      }

    attr(out, "info") <- msg
    out
  }
