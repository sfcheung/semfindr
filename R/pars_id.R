#' @title Convert Parameter Syntax to Position
#' or Row Numbers in the Parameter
#' Vector or Table
#'
#' @description It converts a vector of
#' parameters specified in lavaan syntax to the ids of them
#' in the vector of free parameters or the row numbers
#' in the parameter table.
#'
#' @details It supports the following ways to specify
#' the parameters to be included.
#'
#' - `lavaan` syntax
#'
#'   - For example, `"y ~ x"` denotes the regression coefficient
#'     regression `y` on `x`. It uses [lavaan::lavaanify()] to
#'     parse the syntax strings.
#'
#' - Operator
#'
#'   - For example, `"~"` denotes all regression coefficients.
#'
#' It is used by function such as [est_change()] and
#' [fit_measures_change()].
#'
#' @return
#' A numeric vector of the ids in the column "free" in the
#' parameter table of the fit object.
#'
#' @param pars A character vector of parameters specified
#' in lavaan syntax, e.g., `"y ~ x"` and `f1 =~ x3`. Can
#' mix specific parameters with operators. See the Details
#' section.
#'
#' @param fit A `lavaan`-class object. This object is used
#' to determine the number of groups and the parameters
#' in the model. Only parameters in `pars` that appear in
#' this model will be considered.
#'
#' @param where Where the values are to be found. Can be
#' "partable" (parameter table) or "coef"
#' (coefficient vector).
#' Default is "coef".
#'
#' @param free_only Wether only free parameters will be
#' kept. Default is `TRUE`.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' dat <- sem_dat
#' set.seed(64264)
#'
#' library(lavaan)
#' sem_model <-
#' "
#' f1 =~  x1 + x2 + x3
#' f2 =~  x4 + x5 + x6
#' f3 =~  x7 + x8 + x9
#' f2 ~   f1
#' f3 ~   f2
#' "
#'
#' fit_ng <- sem(sem_model, dat)
#'
#' pars <- c("f1 =~ x2", "f2 =~ x5", "f2 ~ f1")
#' tmp <- pars_id(pars, fit = fit_ng)
#' coef(fit_ng)[tmp]
#' tmp <- pars_id(pars, fit = fit_ng, where = "partable")
#' parameterTable(fit_ng)[tmp, ]
#'
#'
#' @export

pars_id <- function(pars,
                    fit,
                    where = c("coef",
                              "partable"),
                    free_only = TRUE) {
    where <- match.arg(where)
    ids1 <- pars_id_lorg_mod(pars = pars,
                             fit = fit,
                             where = where,
                             free_only = free_only)
    ids2 <- pars_id_lorg(pars = pars,
                         fit = fit,
                         where = where,
                         free_only = free_only)
    ids3 <- pars_id_op(pars = pars,
                       fit = fit,
                       where = where,
                       free_only = free_only)
    out <- sort(unique(c(ids1, ids2, ids3)))
    out
  }

# ###############
# # To be added when other functions support
# # multisample models
# ###############
#
# # Details
#
# # Multisample model
#
# If a model has more than one groups, a specification
# specified as in a single sample model denotes the same
# parameters in all group.
#
# For example, `"f1 =~ x2"` denote the factor loading of
# `x2` on `f1` in all groups. `"~~"` denotes covariances
# and error covariances in all groups.
#
# There are two ways to select parameters only in selected
# groups. First, the syntax to fix parameter values
# can be used, with `NA` denoting parameters to be selected.
#
# For example, `"f2 =~ c(NA, 1, NA) * x5"` select the
# factor loadings of `x5` on `f2` in the first and third
# groups.
#
# Users can also add ".grouplabel" to a specification,
# `grouplabel` being the group label of a group (the one
# appears in [summary()], not the one of the form `".gp2"`,
# `"gp3"`, etc.).
#
# For example, `"f2 =~ x5.Alpha"` denotes the factor loading
# of `x5` on `f2` in the group `"Alpha"`.
#
# @param pars A character vector of parameters specified
# in lavaan syntax, e.g., `"y ~ x"` and `f1 =~ x3`. For
# multisample models, if only the parameters in some groups
# are needed, use the modifier for labeling parameters and
# use `NA` to denote parameters to be requested. E.g.,
# `f1 =~ c(NA, 0, NA, NA) * x2` denotes the loadings of
# `x2` on `f1` in the first, third, and fourth groups.
# Example
#
# dat <- sem_dat
# set.seed(64264)
# dat$gp <- sample(c("Alpha", "Beta", "Gamma"),
#                  nrow(dat),
#                  replace = TRUE)
#
# library(lavaan)
# sem_model <-
# "
# f1 =~  x1 + x2 + x3
# f2 =~  x4 + x5 + x6
# f3 =~  x7 + x8 + x9
# f2 ~   f1
# f3 ~   f2
# "
#
# fit_ng <- sem(sem_model, dat)
# fit_gp <- sem(sem_model, dat, group = "gp")
#
# pars <- c("f1 =~ x2", "f2 =~ x5", "f2 ~ f1")
# tmp <- pars_id(pars, fit = fit_ng)
# coef(fit_ng)[tmp]
# tmp <- pars_id(pars, fit = fit_ng, where = "partable")
# parameterTable(fit_ng)[tmp, ]
#
# pars <- c("f1 =~ x2", "f2 =~ c(NA, 1, NA) * x5")
# tmp <- pars_id(pars, fit = fit_gp)
# coef(fit_gp)[tmp]
# tmp <- pars_id(pars, fit = fit_gp, where = "partable")
# parameterTable(fit_gp)[tmp, ]
#
# pars2 <- c("f1 =~ x2", "~~.Beta", "f2 =~ x5.Gamma")
# tmp <- pars_id(pars2, fit = fit_gp)
# coef(fit_gp)[tmp]
# tmp <- pars_id(pars2, fit = fit_gp, where = "partable")
# parameterTable(fit_gp)[tmp, ]
# # Note that group 1 is "Beta", not "Alpha"
# lavInspect(fit_gp, "group.label")


#' @title Get id based on lhs-op-rhs, using c() modifiers
#'
#' @noRd

pars_id_lorg_mod <- function(pars,
                             fit,
                             where = c("coef",
                                       "partable"),
                             free_only = TRUE) {
    where <- match.arg(where)
    pfree <- lavaan::lavInspect(fit, "npar")
    ngp <- lavaan::lavInspect(fit, "ngroups")
    ptable <- lavaan::parameterTable(fit)
    ptable$rowid <- seq_len(nrow(ptable))
    parspt <- tryCatch(lavaan::lavaanify(pars, ngroups = ngp),
                       error = function(e) e)
    if (inherits(parspt, "simpleError")) {
        return(numeric(0))
      }
    parspt2 <- as.data.frame(lavaan::lavParseModelString(pars))
    mcol <- c("lhs", "op", "rhs", "group", "free")
    parspt3 <- merge(parspt[, mcol],
                     parspt2)[, mcol]
    if (free_only) {
        parspt3 <- parspt3[parspt3$free > 0, ]
      }
    parspt4 <- merge(parspt3[, -which(mcol == "free")], ptable)
    if (where == "partable") {
        out <- parspt4$rowid
      }
    if (where == "coef") {
        out <- parspt4[parspt4$free > 0, "free"]
      }
    out
  }


#' @title Get id based on lhs-op-rhs and group
#'
#' @noRd

pars_id_lorg <- function(pars,
                         fit,
                         where = c("coef",
                                   "partable"),
                         free_only = TRUE) {
    where <- match.arg(where)
    pfree <- lavaan::lavInspect(fit, "npar")
    ngp <- lavaan::lavInspect(fit, "ngroups")
    glabels <- lavaan::lavInspect(fit, "group.label")
    ptable <- lavaan::parameterTable(fit)
    # Do not use user-supplied labels
    ptable$label <- ""
    ptable$rowid <- seq_len(nrow(ptable))
    ptable$lavlabel <- lavaan::lav_partable_labels(ptable, type = "user")
    pars_c <- sapply(pars, function(x) {
                              gsub(x = x,
                                   pattern = " ",
                                   replacement = "",
                                   fixed = TRUE)
                            }, USE.NAMES = FALSE)
    if (ngp > 1) {
        lavlabel_gp <- get_g1(ptable$lavlabel)
        ptable$lavlabel <- add_g1(ptable$lavlabel)
        # pars w gp label (gX)
        for (x in seq_along(glabels)) {
            pars_c <- gsub(paste0("\\.", glabels[x], "$"), paste0(".g", x), pars_c)
          }
        x <- pars_c %in% lavlabel_gp
        pars_c2 <- as.vector(sapply(pars_c[x],
                                function(y) paste0(y, ".g", seq_len(ngp))))
        pars_c <- c(pars_c[!x], pars_c2)
      } else {
        lavlabel_gp <- ptable$lavlabel
        x <- pars_c %in% lavlabel_gp
        pars_c <- pars_c[x]
      }
    parspt4 <- ptable[ptable$lavlabel %in% pars_c, ]
    if (where == "partable") {
      if (free_only) {
          out <- parspt4[parspt4$free > 0, "rowid"]
        } else {
          out <- parspt4$rowid
        }
      }
    if (where == "coef") {
        out <- parspt4[parspt4$free > 0, "free"]
      }
    out
  }

#' @title Find First Group Parameters
#' @noRd

fix_g1 <- function(x, action = c("get", "add")) {
    action <- match.arg(action)
    lavlabels <- x
    a1 <- grepl("^.*\\.g2$", lavlabels)
    lavlabels_ng <- gsub(".g2", "", lavlabels[a1])
    a2 <- lavlabels %in% lavlabels_ng
    if (action == "get") {
        return(lavlabels[a2])
      }
    if (action == "add") {
        lavlabels[a2] <- paste0(lavlabels[a2], ".g1")
        return(lavlabels)
      }
    stop("Unknown action in fix_g1().")
  }

add_g1 <- function(x) {
    fix_g1(x, action = "add")
  }

get_g1 <- function(x) {
    fix_g1(x, action = "get")
  }


#' @title Get id based on operator
#' "=~": All "=~" in all groups
#' "=~.gp1": All "=~" in groups with labels "gp1"
#' @noRd

pars_id_op <- function(pars,
                       fit,
                       where = c("coef",
                                 "partable"),
                       free_only = TRUE) {
    where <- match.arg(where)
    pfree <- lavaan::lavInspect(fit, "npar")
    ngp <- lavaan::lavInspect(fit, "ngroups")
    glabels <- lavaan::lavInspect(fit, "group.label")
    ptable <- lavaan::parameterTable(fit)
    # Do not use user-supplied labels
    ptable$label <- ""
    ptable$rowid <- seq_len(nrow(ptable))
    ptable$lavlabel <- lavaan::lav_partable_labels(ptable, type = "user")
    pars_c <- sapply(pars, function(x) {
                              gsub(x = x,
                                   pattern = " ",
                                   replacement = "",
                                   fixed = TRUE)
                            }, USE.NAMES = FALSE)
    # Extract operators in the table
    pt_ops <- unique(ptable$op)
    # Keep only operators
    pars_c <- keep_ops(pars_c, pt_ops)
    out0 <- integer(0)
    if (ngp > 1) {
        for (x in seq_along(pars_c)) {
            tmp <- sapply(glabels, function(y) {
                      grepl(paste0("\\.", y), pars_c[x])
                    })
            if (any(tmp)) {
                gp_tmp <- which(tmp)
                pt_tmp <- ptable[(ptable$group == gp_tmp) &
                                 (ptable$op ==
                                  gsub(pattern = paste0("\\.", glabels[gp_tmp]),
                                       replacement = "",
                                       x = pars_c[x])), ]
                out0 <- c(out0, pt_tmp$rowid)
              }
          }
      }
    # For both operators without suffixes
    # and models with only one group
    op_selected <- intersect(pars_c, pt_ops)
    if (length(op_selected) > 0) {
        pt_tmp <- ptable[ptable$op %in% op_selected, ]
        out0 <- c(out0, pt_tmp$rowid)
      }
    out0 <- sort(unique(out0))
    tmp <- ptable[out0, ]
    if (where == "partable") {
        if (free_only) {
            out <- tmp[tmp$free > 0, "rowid"]
          } else {
            out <- tmp$rowid
          }
      }
    if (where == "coef") {
        out <- tmp[tmp$free > 0, "free"]
      }
    out
  }

#' @title Keep op only
#' @noRd

keep_ops <- function(pars, ops) {
    tmp <- sapply(ops, function(x) {
              which(grepl(utils::glob2rx(paste0(x, "*")), pars))
            })
    unique(pars[unlist(tmp)])
  }

#' @title Get id based on wildcard
#' @noRd

pars_id_wild <- function(pars,
                         fits,
                         where = c("coef",
                                   "partable")) {
    stop("No ready for use")
    # ToDo
    # Placeholder
  }

#' @title Get id for special parameters
#' @noRd

pars_id_special <- function(pars,
                            fit,
                            where = c("coef",
                                      "partable")) {
    stop("No ready for use")
    # ToDo
    # Placeholder
  }


#' @title Convert Ids to "lhs-op-rhs-(group)"
#'
#' @description Convert ids generated by [pars_id()]
#' to values that can be used to extract the parameters
#' from a source.
#'
#' @details
#' If the source is a parameter estimates table (i.e.,
#' the output of [lavaan::parameterEstimates()], it returns
#' a data frame with columns "lhs", "op", and "rhs". If
#' "group" is present in the source, it also add a column
#' "group". These columns can be used to uniquely identify
#' the parameters specified by the ids.
#'
#' If the source is a named vector of parameters (e.g., the
#' output of [coef()]), it returns the names of parameters
#' based on the ids.
#'
#' @param pars_id A vector of integers. Usually the output
#' of [pars_id].
#'
#' @param pars_source Can be the output of
#' [lavaan::parameterEstimates()] or [lavaan::parameterTable()],
#' or a named vector of free parameters (e.g., the output
#' of [coef()] applied to a `lavaan`-class object).
#'
#' @param type The meaning of the values in `pars_id`.
#' If `"free"`, they
#' are the position in the vector of free parameters (i.e.,
#' the output of of `coef()`). If "all", they are the
#' row numbers in the parameter table (the output of
#' [lavaan::parameterTable()]). If `pars_source`
#' is the output of [lavaan::parameterEstimates()], which
#' does not indicate whether a parameter is free or fixed,
#' this argument will be ignored.
#'
#' @examples
#' dat <- sem_dat
#' set.seed(64264)
#' library(lavaan)
#' sem_model <-
#' "
#' f1 =~  x1 + x2 + x3
#' f2 =~  x4 + x5 + x6
#' f3 =~  x7 + x8 + x9
#' f2 ~   f1
#' f3 ~   f2
#' "
#' fit_ng <- sem(sem_model, dat)
#'
#' pars <- c("f1 =~ x2", "f2 =~ x5", "f2 ~ f1")
#' tmp <- pars_id(pars, fit = fit_ng)
#' pars_id_to_lorg(tmp, pars_source = coef(fit_ng))
#' tmp <- pars_id(pars, fit = fit_ng, where = "partable")
#' pars_id_to_lorg(tmp, pars_source = parameterEstimates(fit_ng))
#'

#'
#' @export

pars_id_to_lorg <- function(pars_id,
                            pars_source,
                            type = c("free", "all")) {
    type <- match.arg(type)
    if (is.null(dim(pars_source))) {
        is_coef <- TRUE
        has_group <- FALSE
      } else {
        is_coef <- FALSE
          if ("group" %in% colnames(pars_source)) {
              has_group <- TRUE
            } else {
              has_group <- FALSE
            }
      }
    if (is_coef) {
        out <- names(pars_source)[pars_id]
      } else {
        if (is.null(pars_source$free)) pars_source$free <- 1
        pars_source_1 <- switch(type,
                           free = pars_source[pars_source$free > 0, ],
                           all = pars_source)
        out <- pars_source_1[pars_id, ]
        if (has_group) {
            out <- out[, c("lhs", "op", "rhs", "group")]
          } else {
            out <- out[, c("lhs", "op", "rhs")]
          }
      }
    out
  }

# ###############
# # To be added when other functions support
# # multisample models
# ###############
#
# Example
#
# dat$gp <- sample(c("Alpha", "Beta", "Gamma"),
#                  nrow(dat),
#                  replace = TRUE)
#
# fit_gp <- sem(sem_model, dat, group = "gp")
#
# pars <- c("f1 =~ x2", "f2 =~ c(NA, 1, NA) * x5")
# tmp <- pars_id(pars, fit = fit_gp)
# pars_id_to_lorg(tmp, pars_source = coef(fit_gp))
# tmp <- pars_id(pars, fit = fit_gp, where = "partable")
# pars_id_to_lorg(tmp, pars_source = parameterEstimates(fit_gp))
#
# parameterTable(fit_gp)[tmp, ]
# pars2 <- c("f1 =~ x2", "~~.Beta", "f2 =~ x5.Gamma")
# tmp <- pars_id(pars2, fit = fit_gp)
# pars_id_to_lorg(tmp, pars_source = coef(fit_gp))
# tmp <- pars_id(pars2, fit = fit_gp, where = "partable")
# pars_id_to_lorg(tmp, pars_source = parameterEstimates(fit_gp))
# # Note that group 1 is "Beta", not "Alpha"
# lavInspect(fit_gp, "group.label")