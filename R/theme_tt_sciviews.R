#' Default SciViews theme for tinytables
#'
#' This theme is applied by default to \{tinytable\} output. One can use a
#' different one with [tinytable::theme_tt()].
#'
#' @param x A **tinytable** object
#' @param ... Additional arguments (not used)
#'
#' @return A **tinytable** object with the default SciViews theme applied
#' @export
#'
#' @importFrom tinytable style_tt
theme_tt_sciviews <- function(x, ...) {
  nrow <- x@nrow
  x |>
    style_tt(bootstrap_class = "table table-sm table-borderless") |>
    style_tt(i = 0, line = "tb", line_color = "black") |>
    style_tt(i = nrow, line = "b", line_color = "black")
}
