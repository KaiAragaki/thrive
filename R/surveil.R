#' Construct a new thrive object
#'
#' @param x a dataframe
#'
#' @return
#' @export
new_thrive <- function(x = data.frame()) {
  stopifnot(is.data.frame(x))
  structure(x, class = c("thrive", "tbl_df", "tbl", "data.frame"))
}

validate_thrive <- function(thrive) {

  thrive_obj_names <- c("time", "n.risk", "n.event", "n.censor", "estimate",
                        "std.error", "conf.high", "conf.low")

  if (!all(rlang::has_name(thrive, thrive_obj_names))) {
    missing_names <- setdiff(thrive_obj_names, rlang::names2(thrive)) |> paste(collapse = ", ")
    warning("The following columns were expected but not found: ", missing_names)
  }

}

#' Coerce a fit to a thrive object
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
as_thrive <- function(x, ...) {
  UseMethod("as_thrive")
}

#' @export
#' @rdname as_thrive
as_thrive.survfit <- function(x, ...) {
  x <- tidy(x)
  x <- extract_strata(x, strata)
  new_thrive(x)
}

#' @export
#' @rdname as_thrive
as_thrive.numeric <- function(x, ...) {
  new_thrive(x)
}
