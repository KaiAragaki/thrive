#' @include utilities.R surv_group_by.R
NULL
#' Create Survival Curves
#' @rdname surv_fit
#' @export
surv_fit <- new_generic("surv_fit", c("formula", "data"), function(formula, data, ...) {
  edata <- eval_tidy(enquo(data))
  R7_dispatch()
})

method(surv_fit, list(class_list, class_list)) <-
  function(formula, data, ...) {

    browser()
    formula <- enexpr(formula)
    data_ex <- enexpr(data)
    if (!is.null(group.by) | .is_grouped_data(data)) {
      data <- surv_group_by(data, group.by)
      return(surv_fit(formula, data$data, ...))
    }
    fit <- eval(bquote(survfit(.(formula), .(data_ex))))

    browser()
    fit$call$formula <- formula
    fit$call$data <- as.name(data)
    fit
  }


method(surv_fit, list(new_class("formula"), class_data.frame)) <-
  function(formula, data, ...) {
    formula <- enquo(formula)
    edata <- enquo(data)
    browser()
    # if (!is.null(group.by) | .is_grouped_data(data)) {
    #   data <- surv_group_by(data, group.by)
    #   return(surv_fit(formula, data$data, ...))
    # }
    fit <- eval(bquote(survfit(.(formula), .(data_ex))))
    fit$call$formula <- formula
    fit$call$data <- as.name(data)
    fit
  }

method(surv_fit, list(class_list, class_data.frame)) <-
  function(formula, data, ...) {
    formula <- enquos(formula)
    data <- enquo(data)
    # if (!is.null(group.by)) {
    #   data <- surv_group_by(data, group.by)
    #   return(surv_fit(formula, data$data, match.fd = match.fd, ...))
    # }
    browser()
    mapply(function(x, y) surv_fit(x, y), formula, list(data), SIMPLIFY = F, USE.NAMES = F)
  }

method(surv_fit, list(new_class("formula"), class_list)) <-
  function(formula, data, ...) {
    lapply(data, \(x) eval(bquote(surv_fit(formula, .(x)))))
  }

method(surv_fit, list(class_list, class_list)) <-
  function(formula, data, ...) {

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
