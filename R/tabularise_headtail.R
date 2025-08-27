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
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#' @param env The environment where to evaluate formulas (you probably do not
#' need to change the default).
#'
#' @return A **flextable** object you can print in different form or rearrange
#' with the \{flextable\} functions from set Stb$verb().
#' @export
#' @seealso [tabularise()], [head()], [tail()], [tabularise_default()]
#'
#' @examples
#' tabularise$headtail(iris)
tabularise_headtail <- function(data, n = 10, ..., kind = "ft", env = env) {
  UseMethod("tabularise_headtail")
}

#' @export
#' @rdname tabularise_headtail
#' @method tabularise_headtail default
tabularise_headtail.default <- function(data, n = 10, ...,
  kind = "ft", env = env) {
  stop("No method for tabularise_headtail() for this object")
}

#' @export
#' @importFrom flextable add_footer_lines autofit align valign colformat_char
#'   colformat_num
#' @importFrom tinytable tt format_tt
#' @rdname tabularise_headtail
#' @param auto.labs Are labels automatically used for names of table columns?
#' @param sep The separator between the first and last lines of a table. By
#' default, the vertical ellipse shape is used.
#' @param lang the natural language to use. The default value can be set with,
#'   e.g., `options(SciViews_lang = "fr")` for French.
#' @method tabularise_headtail data.frame
tabularise_headtail.data.frame <- function(data, n = 10,
    auto.labs = TRUE, sep = "...", ..., # sep = "\U22EE" makes problems in LaTeX
    lang = getOption("SciViews_lang", "en"), kind = "ft", env = env) {
  # TODO: allow using labels and units + restrict rows and cols
  if (nrow(data) <= 1.5 * n) {
    switch(kind,
      tt = tt(data, ...),
      ft = flextable(data, ...),
      gt = ,
      stop("Not implemented yet")
    )
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
      labels <- sapply(data, svBase::label, units = TRUE)
      if (any(labels != "")) {
        labels[labels == ""] <- names(data)[labels == ""] # set names if empty
        names(x) <- as.character(labels)
      }
    }

    footer <- .infos_lang.ht(lang = lang)

    switch(kind,
      tt = {
        tt(x, caption = knitr::opts_current$get('tbl-cap'),
          notes = paste0(footer[1], n, footer[2], nrow(data))) |>
          format_tt(i = n + 1, replace = sep)
      },
      ft = {
        flextable(x, ...) |>
          add_footer_lines(paste0(footer[1], n, footer[2], nrow(data))) |>
          # TODO: this symbol needs xelatex
          colformat_char(i = n + 1, na_str = sep, nan_str = sep) |>
          colformat_num(i = n + 1, na_str = sep, nan_str = sep) |>
          autofit() -> res
          caption <- knitr::opts_current$get('tbl-cap')
          if (!is.null(caption))
            res <- set_caption(res, caption)
          res
      },
      gt = ,
      stop("Not implemented yet")
    )
  }
}

# Internal function : Choose the lang and the infos_lang ----
.infos_lang.ht <- function(lang = getOption("SciViews_lang", "en")) {
  lang <- tolower(lang)
  if (lang != "fr") lang <- "en" # Only en or fr for now
  if (lang == "fr") {
    .infos_fr.ht
  } else {
    .infos_en.ht
  }
}

.infos_en.ht <- c("First and last ", " rows of a total of ")
.infos_fr.ht <- c("Premi\u00e8res et derni\u00e8res ", " lignes d'un total de ")
