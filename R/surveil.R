#'
#'
#' @param fit
#'
#' @return
#' @export
#'
#' @examples
new_thrive <- function(fit = numeric()) {
  stopifnot(is.numeric(fit))
  structure(fit, class = "Thrive")
}

validate_thrive <- function(Thrive) {

}

#' Coerce a fit to a thrive object
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
as_thrive <- function(x, ...) {
  UseMethod("as_thrive")
}

#' @export
#' @rdname as_thrive
as_thrive.character <- function(x, ...) {
  x <- as.numeric(x)
  new_thrive(x)
}


#' @export
#' @rdname as_thrive
as_thrive.numeric <- function(x, ...) {
  new_thrive(x)
}


ggsurvplot <- function(fit, data = NULL, fun = NULL,
                       color = NULL, palette = NULL, linetype = 1,
                       conf.int = FALSE, pval = FALSE, pval.method = FALSE,
                       test.for.trend = FALSE,
                       surv.median.line = "none",
                       risk.table = FALSE, cumevents = FALSE, cumcensor = FALSE,
                       tables.height = 0.25,
                       group.by = NULL, facet.by = NULL, add.all = FALSE, combine = FALSE
){

  if(length(group.by) > 2)
    stop("group.by should be of length 1 or 2.")


  # Options for ggsurvplot_df
  # Don't accept arguments for pval and survival tables
  opts_df <- list(
    fit = fit, fun = fun,
    color = color, palette = palette, linetype = linetype,
    conf.int = conf.int)

  # Options for the remaining ggsurvplot_*() functions
  opts <- list(
    fit = fit, data = data, fun = fun,
    color = color, palette = palette, linetype = linetype,
    conf.int = conf.int, pval = pval, pval.method = pval.method,
    test.for.trend = test.for.trend,
    surv.median.line = surv.median.line,
    risk.table = risk.table, cumevents = cumevents, cumcensor = cumcensor,
    tables.height = tables.height)




  if(is.list(fit)){
    if(combine)
      ggsurv <- do.call(ggsurvplot_combine, opts)
    else ggsurv <- do.call(ggsurvplot_list, opts)
  }

  else if(is.data.frame(fit))
    ggsurv <- do.call(ggsurvplot_df, opts_df)

  else if(.is_survfit(fit)){

    if(!is.null(group.by)){
      opts$group.by <- group.by
      ggsurv <- do.call(ggsurvplot_group_by, opts)
    }
    else if(!is.null(facet.by)){
      opts$facet.by <- facet.by
      ggsurv <- do.call(ggsurvplot_facet, opts)
    }
    else if(add.all){
      ggsurv <- do.call(ggsurvplot_add_all, opts)
    }

    else{
      if(is.null(fit$strata)){
        if(missing(conf.int)){
          opts$conf.int = TRUE
          opts$conf.int.fill = "strata"
        }
      }
      ggsurv <- do.call(ggsurvplot_core, opts)
    }

  }

  else if(inherits(fit, "flexsurvreg"))
    ggsurv <- do.call(ggflexsurvplot, opts)


  return(ggsurv)
}



#' @param x an object of class ggsurvplot
#' @method print ggsurvplot
#' @param newpage open a new page. See \code{\link{grid.arrange}}
#' @rdname ggsurvplot
#' @export
print.ggsurvplot <- function(x, surv.plot.height = NULL, risk.table.height = NULL, ncensor.plot.height = NULL, newpage = TRUE, ...){

  res <- .build_ggsurvplot(x = x, surv.plot.height = surv.plot.height,
                           risk.table.height = risk.table.height,
                           ncensor.plot.height = ncensor.plot.height)
  if(newpage) grid::grid.newpage()
  grid::grid.draw(res)

}
