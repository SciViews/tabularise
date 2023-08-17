# Note: getOptions("digits") = 7 by default -> too high
# getOption("scipen") = 0 is OK, or a bit too large (sci lower than 10-5)
.format_sci <- function(x, digits = 3, scipen = 0, fancy = TRUE,
  op = c("\U00B7", "\U00D7", "*", "x")) {# Both middle dot and cross OK with default LaTeX engine
  out <- format(x, scientific = scipen, digits = digits)
  na_str <- flextable::get_flextable_defaults()$na_str
  nan_str <- flextable::get_flextable_defaults()$nan_str
  out[is.nan(x)] <- nan_str
  out[is.na(x)] <- na_str
  if (isTRUE(fancy)) {
    splt <- strsplit(out, "e", fixed = TRUE)
    out1 <- sapply(splt, `[`, 1L)
    out3 <- sapply(splt, `[`, 2L)
    out2 <- rep(paste0(op[1], "10"), length(out1))
    out2[is.na(out3)] <- ""
    flextable::as_paragraph(out1, out2, flextable::as_sup(out3))
  } else {
    flextable::as_paragraph(out)
  }
}


#' Scientific format for columns in flextable
#'
#' @param x a flextable object
#' @param i rows selection
#' @param j columns selection
#' @param digits number of digits to display
#' @param scipen penalty to use to decide if numbers are presented in decimal
#'   or scientific notation (generally use 0 or -1)
#' @param fancy use a perfect scientific notation (`TRUE`) or a simplified one
#'   like 1.34e-5 (`FALSE`).
#' @param op the operator character to use in fancy scientific notation
#'
#' @return the flextable object with the selected region formatted as scientific numbers.
#' @export
#'
#' @examples
#' summ <- summary(lm(Volume ~ Girth + Height, data = trees))
#' tabularise(as.data.frame(summ$coefficients)) |>
#'   colformat_sci() |>
#'   tb_$autofit_size()
colformat_sci <- function(x, i = NULL, j = NULL, digits = 3, scipen = 0,
  fancy = TRUE, op = c("\U00B7", "\U00D7", "*", "x")) {

  stopifnot(inherits(x, "flextable"))

  quo_fun <- quo(.format_sci(x, digits = digits, scipen = scipen, fancy = fancy,
    op = op))
  fun_ <- new_function(pairlist2(x = , digits = digits, scipen = scipen,
    fancy = fancy, op = op), get_expr(quo_fun))

  col_keys <- .filter_col_keys(x, j, function(x) is.double(x) &&
      !inherits(x, "POSIXt") && !inherits(x, "Date"))
  .docall_display(col_keys, fun_, x, i = i)
}
