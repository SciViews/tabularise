# TODO: continuous_summary + proc_freq + shift_table
# TODO: tabulator + tabulator_colnames + summarizor + labelizor
# TODO: also see in janitor package (tabyl(), ...)
# For the moment...
# This combines flextable(), as_flextable() (also regulartable() and
# qflextable()), as_flextable(as_grouped_data()), continuous_summary()
# Note: proc_freq() is used in as_flextable(table())

#' Tabularise an object (arrange or enter in tabular form)
#'
#' @param data An object
#' @param n The number of lines to tabularise
#' @param ... Further arguments (depending on the object class and on `type=`).
#' @param type The type of table to produce.
#' @param env The environment where to evaluate formulas (you probably do not
#' need to change the default).
#'
#' @return A **flextable** object you can print in different form or rearrange
#' with the {flextable} functions from set tb_$verb().
#' @export
#'
#' @examples
#' tabularise(iris)
#' tabularise$headtail(iris)
tabularise <- structure(
  function(data, n = 6, ..., type = "default", env = parent.frame()) {
    get_type("tabularise", type = type)(data, n = n, ...)
  }, class = c("function", "subsettable_type", "tabularise"))

# Synonyms
#' @export
#' @rdname tabularise
tabularize <- tabularise
#tab6e <- tabularise # Alphanumeric acronym ou numeronym

#' @export
#' @rdname tabularise
tabularise_default <- function(data, ..., env = env) {
  UseMethod("tabularise_default")
}

# TODO: limit max row and col + frame row/col
#' @export
#' @rdname tabularise
#' @method tabularise_default default
tabularise_default.default <- function(data, ..., type = NULL,
  env = parent.frame()) {
  res <- try(as_flextable(data, ...), silent = TRUE)
  if (inherits(res, "try-error"))
    stop("I don't know how to tabularise an object of class '",
      class(data)[1], "'")
  res
}

#' @export
#' @importFrom flextable add_footer_lines as_paragraph as_i
#' @rdname tabularise
#' @param formula A formula to create a table using the {tables} syntax
#' @param col_keys The names/keys to use for the table columns
#' @param cwidth Initial width for cell sizes in inches
#' @param cheight Initial height for cell sizes in inches
#' @param max.rows The maximum number of rows to display in the table
#' @param max.cols The maximum number of columns to display in the table
#' @param auto.labs Are labels automatically used for names of table columns?
#' @method tabularise_default data.frame
tabularise_default.data.frame <- function(data, formula = NULL,
  col_keys = names(data), cwidth = 0.75, cheight = 0.25, max.rows = 50,
  max.cols = 15, auto.labs = TRUE, ..., env = parent.frame()) {
  if (!missing(formula)) {
    if (!inherits(formula, "formula"))
      stop("Argument 'formula' must ba a formula object")
    # Use the tabular type instead
    stop("Not implemented yet: tabularise.tabular()")
    #`tabularise_tabular`(data, formula, ..., env = env)
  }
  # Use labels and units in the header, if available
  if (isTRUE(auto.labs)) {
    labels <- sapply(data, data.io::label, units = TRUE)
    if (any(labels != "")) {
      # Use a \n before the units
      labels <- sub(" +\\[([^]]+)\\]$", "\n[\\1]", labels)
      labels[labels == ""] <- names(data)[labels == ""] # set names if empty
      names(data) <- as.character(labels)
      # Also rework col_keys accordingly
      keys <- as.character(labels[col_keys])
      keys[is.na(keys)] <- col_keys[is.na(keys)]
      col_keys <- keys
    }
  }
  # Restrict table to max.rows and max.cols
  note <- ""
  n <- nrow(data)
  if (n > max.rows) {
    data <- data[1:max.rows, ]
    note <- paste(n - max.rows, "more rows\n")
  }
  m <- length(col_keys)
  if (m > max.cols) {
    m2 <- m - max.cols
    if (m2 == 1) {
      note <- paste0(note, m2, " more viariable: ", col_keys[max.cols + 1])
    } else {
      note <- paste0(note, m2, " more variables: ",
        paste(gsub("\n", "  ", col_keys[(max.cols + 1):length(col_keys)]),
          collapse = ", "))
    }
    col_keys <- col_keys[1:max.cols]
  }
  if (note == "") {
    res <- flextable(data, col_keys = col_keys, cwidth = cwidth,
      cheight = cheight)
  } else {
    res <- flextable(data, col_keys = col_keys, cwidth = cwidth,
      cheight = cheight) |>
      add_footer_lines(as_paragraph(as_i(note)))
  }
  if (isTRUE(auto.labs)) {
    align_h(res, align = "center", part = "header") |>
      align_v(valign = "bottom", part = "header")
  } else {
    res
  }
}

#' @export
#' @rdname tabularise
tabularise_headtail <- function(data, n = 10, ..., env = env) {
  UseMethod("tabularise_headtail")
}

#' @export
#' @rdname tabularise
#' @method tabularise_headtail default
tabularise_headtail.default <- function(data, n = 10, ..., env = env) {
  stop("No method for tabularise_headtail() for this object")
}

#' @export
#' @importFrom flextable add_footer_lines colformat_num
#' @rdname tabularise
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
      colformat_chr(i = n + 1, na_str = "\U22EE", nan_str = "\U22EE") |>
      colformat_num(i = n + 1, na_str = "\U22EE", nan_str = "\U22EE")
  }
}
