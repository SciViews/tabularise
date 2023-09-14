#' Head and tail type tabularise generic (tabularise$headtail)
#'
#' The "headtail" type for tabularise presents the first (head) and last (tail)
#' few lines of a table. This is useful for a long table and often more useful
#' than just displaying only the first few lines of the same table (that you got
#' with the default type, see [tabularise_default()]).
#'
#' @param data An object
#' @param n The number of lines to display in the (truncated) table.
#' @param ... Further arguments (depending on the object class).
#' @param env The environment where to evaluate formulas (you probably do not
#' need to change the default).
#'
#' @return A **flextable** object you can print in different form or rearrange
#' with the {flextable} functions from set Stb$verb().
#' @export
#' @seealso [tabularise()], [head()], [tail()], [tabularise_default()]
#'
#' @examples
#' tabularise$headtail(iris)
tabularise_headtail <- function(data, n = 10, ..., env = env) {
  UseMethod("tabularise_headtail")
}

#' @export
#' @rdname tabularise_headtail
#' @method tabularise_headtail default
tabularise_headtail.default <- function(data, n = 10, ..., env = env) {
  stop("No method for tabularise_headtail() for this object")
}

#' @export
#' @importFrom flextable add_footer_lines autofit align valign colformat_char
#'   colformat_num
#' @rdname tabularise_headtail
#' @param auto.labs Are labels automatically used for names of table columns?
#' @method tabularise_headtail data.frame
tabularise_headtail.data.frame <- function(data, n = 10,  auto.labs = TRUE,
  ..., env = env) {
  # TODO: allow using labels and units + restrict rows and cols
  if (nrow(data) <= 1.5 * n) {
    flextable(data, ...)
  } else {# Display only first and last n rows + footer
    n <- n %/% 2
    if (n < 2) {
      warning("I don't tabularise with less than 4 rows")
      n <- 2
    }
    # Produces an error with data.tables, so, make sure we have a data.frame
    data <- as.data.frame(data)
    x <- rbind(head(data, n = n), data[n + 1, ] == NA, tail(data, n = n))

    # Use labels and units in the header, if available
    if (isTRUE(auto.labs)) {
      labels <- sapply(data, data.io::label, units = TRUE)
      if (any(labels != "")) {
        labels[labels == ""] <- names(data)[labels == ""] # set names if empty
        names(x) <- as.character(labels)
      }
    }
    x |>
      flextable(...) |>
      add_footer_lines(paste0("First and last ", n, " rows of a total of ",
        nrow(data))) |>
      # TODO: this symbol needs xelatex
      colformat_char(i = n + 1, na_str = "\U22EE", nan_str = "\U22EE") |>
      colformat_num(i = n + 1, na_str = "\U22EE", nan_str = "\U22EE") |>
      autofit() -> res
    caption <- knitr::opts_current$get('tbl-cap')
    if (!is.null(caption))
      res <- set_caption(res, caption)
    res
  }
}
