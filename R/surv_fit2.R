#' @include utilities.R surv_group_by.R
NULL
#' Create Survival Curves
#' @rdname surv_fit
#' @export
surv_fit <- new_generic("surv_fit", c("formula", "data"))

method(surv_fit, list(new_class("formula"), class_data.frame)) <-
  function(formula, data, group.by = NULL, ...) {

    if (!is.null(group.by)) {
      data <- surv_group_by(data, group.by)
      return(surv_fit(formula, data$data, ...))
    }
    fit <- survfit(formula, data)
    fit$call$formula <- formula
    fit$call$data <- eval(bquote(.(substitute(data))))
    fit
  }

method(surv_fit, list(class_list, class_data.frame)) <-
  function(formula, data, group.by = NULL, match.fd, ...) {

    if (!is.null(group.by)) {
      data <- surv_group_by(data, group.by)
      return(surv_fit(formula, data$data, match.fd = match.fd, ...))
    }

    lapply(formula, surv_fit, data = data)
  }

method(surv_fit, list(new_class("formula"), class_list)) <-
  function(formula, data, ...) {
    lapply(data, \(x) {
      eval(bquote(surv_fit(formula, .(x))))
    })
  }

method(surv_fit, list(class_list, class_list)) <- function(formula, data, match.fd, ...) {

  if (!match.fd) {
    eg <- expand.grid(formula = formula, data = data)
    formula <- eg$formula
    data <- eg$data
  }

  mapply(
    \(x, y) eval(bquote(surv_fit(.(x), .(y)))),
    formula, data, ..., USE.NAMES = FALSE, SIMPLIFY = FALSE
  )

}
# surv_fit <- function(formula, data, group.by = NULL, match.fd = FALSE, ...){
#
#   # List of formulas and List of data sets with match.fd = TRUE
#   #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#   # They should have the same length.
#   # Each formula is applied to each data set of the same index
#   if(.is_list (formula) & .is_list (data) & match.fd){
#
#     if(length(formula) != length(data))
#       stop("When formula and data are lists, ",
#            "they should have the same length")
#
#     res <- purrr::map2(formula, data, .survfit1,  ...)
#
#   }
#
#   # List of formulas and List of data sets with match.fd = FALSE
#   #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#   # Map each formula to each of the  data in the data lists ==> returns a list of survfit objects for each formula
#   # Combine all lists to one single list
#   else if(.is_list (formula) & .is_list (data) & !match.fd){
#
#     .map_each <- function(formula, data){
#       purrr::map(data, .survfit2, formula, ...)
#     }
#     res <- purrr::map(formula, .map_each , data) %>%
#       dplyr::combine()
#   }
#
#   res
# }
