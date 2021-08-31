#' @title Print method for lavaan_rerun
#'
#' @description Print the results of lavaan_rerun
#'
#' @details Print the results of lavaan_rerun
#'
#' @return
#'  Nothing
#'
#' @param x The output of [lavaan_rerun()].
#' @param ... Other arguments. They will be ignored.
#'
#' @author S. F. Cheung (shufai.cheung@gmail.com)
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
    i_checked <- sapply(x$post_check, isTRUE)
    n_checked <- sum(i_checked)
    n_failed <- nrerun - n_checked
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
        failed_messages_df <- failed_messages_df[, c(2, 1)]
        colnames(failed_messages_df) <- c("N", "Warning or error messages")
      }
    org_call <- x$call
    cat("=== lavaan_rerun Output ===\n")
    cat("Call:\n")
    print(org_call)
    cat(paste0("Number of reruns: ", nrerun, "\n"))
    cat(paste0("Number of reruns that passed post.check of lavaan: ",
                n_checked, "\n"))
    cat(paste0("Number of reruns that failed post.check of lavaan: ",
                n_failed, "\n"))
    if (n_failed > 0) {
        cat("Detail:\n")
        print(failed_messages_df)
      }
  }
