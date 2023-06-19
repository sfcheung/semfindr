#' @title Print Method for 'lavaan_rerun'
#'
#' @description Prints the results of [lavaan_rerun()].
#'
#' @return `x` is returned invisibly. Called for its side effect.
#'
#' @param x The output of [lavaan_rerun()].
#'
#' @param ... Other arguments. They will be ignored.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
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
#' fit_rerun
#'
#' @export

print.lavaan_rerun <- function(x,
                          ...) {
    nrerun <- length(x$rerun)
    i_converged <- sapply(x$converged, isTRUE)
    n_converged <- sum(i_converged)
    i_not_converged <- sapply(x$converged, isFALSE)
    n_not_converged <- sum(i_not_converged)
    i_checked <- sapply(x$post_check, isTRUE)
    n_checked <- sum(i_checked)
    i_failed <- sapply(x$post_check, inherits, "warning")
    n_failed <- sum(i_failed)
    if (n_failed > 0) {
        failed_messages <- sapply(x$post_check[which(!i_checked)],
                              function(x) {
                                  msg <- tryCatch(x$message,
                                                  error = function(e) e)
                                  if (inherits(msg, "SimpleError")) {
                                      msg <- tryCatch(as.character(x),
                                                  error = function(e) e)
                                    }
                                  if (inherits(msg, "SimpleError")) {
                                      msg <- "Unknown. Please check the fit object."
                                    }
                                  msg
                                })
        failed_messages_df <- as.data.frame(table(failed_messages))
        failed_messages_df$failed_messages <- as.character(failed_messages_df$failed_messages)
        failed_messages_df <- failed_messages_df[, c(2, 1)]
      }
    org_call <- x$call
    i_valid <- i_converged & i_checked
    n_valid <- sum(i_valid)
    i_invalid <- !(i_converged) | !(i_checked)
    n_invalid <- sum(i_invalid)
    cat("=== lavaan_rerun Output ===\n")
    cat("Call:\n")
    print(org_call)
    cat(paste0("Number of reruns: ", nrerun, "\n"))
    cat(paste0("Number of reruns that converged (solution found): ",
                n_converged, "\n"))
    cat(paste0("Number of reruns that failed to converge (solution not found): ",
                n_not_converged, "\n"))
    cat(paste0("Number of reruns that passed post.check of lavaan: ",
                n_checked, "\n"))
    cat(paste0("Number of reruns that failed post.check of lavaan: ",
                n_failed, "\n"))
    cat(paste0("Number of reruns that both converged and passed post.check: ",
               n_valid, "\n"))
    cat(paste0("Number of reruns that either did not converge or failed post.check: ",
               n_invalid, "\n"))
    if (n_not_converged > 0) {
        cat(paste0("Case(s) failed to converge:\n",
                  paste(names(x$rerun)[i_not_converged], collapse = ","), "\n"))
      }
    if (n_failed > 0) {
        cat(paste0("Case(s) failed post.check:\n",
                  paste(names(x$rerun)[i_failed], collapse = ","), "\n"))
        cat("Detail:\n")
        for (i in seq_len(nrow(failed_messages_df))) {
            cat("Warning/Message (",
                failed_messages_df$Freq[i],
                " case[s])",
                ":\n", sep = "")
            cat(failed_messages_df$failed_messages[i], "\n")
          }
        cat("\n")
      }
    invisible(x)
  }
