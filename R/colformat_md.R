.format_md <- function(x, h.sizes = c(15, 13, 9),
  h.colors = c("black", "darkgray"),
  strike = list(color = h.colors[2], underlined = FALSE), bullet = "\U00b7",
  code.font = "monospace", code.shading = "#f8f8f8",
  link.color = "darkblue", link.underline = TRUE, autolink = TRUE) {
  para_md(x, h.sizes = h.sizes, h.colors = h.colors, strike = strike,
    bullet = bullet, code.font = code.font, code.shading = code.shading,
    link.color = link.color, link.underline = link.underline,
    autolink = autolink)
}

#' Scientific format for columns in {flextable}
#'
#' @param x a **flextable** object
#' @param i rows selection
#' @param j columns selection
#' @param h.sizes font sizes for titles
#' @param h.colors font colors for titles
#' @param strike a list with color and underlined = `TRUE` or `FALSE` for strikethrough text
#' @param bullet bullet character used for lists
#' @param code.font font used for code strings
#' @param code.shading background color (shading) for code strings
#' @param link.color color used for URL links
#' @param link.underline are URL links underlined?
#' @param autolink are links automatically constructed when there is an URL in the text?
#'
#' @return the **flextable** object with the selected region formatted as markdown strings.
#' @export
#'
#' @examples
#' dat <- data.frame(
#'   x = 1:3,
#'   y = noquote(c("#### text1", "text~2~", "text^3^")),
#'   names = noquote(c("*Iris setosa*", "Iris ~~virginica~~",
#'     "Iris **versicolor**")),
#'   id = c("*setosa*", "`virginica`", "**versicolor**"),
#'   factor = factor(c("*setosa*", "`virginica`", "**versicolor**"))
#' )
#' tabularise(dat)
#' tabularise(dat) |> colformat_md() |> Stb$autofit()
#' tabularise(dat) |> colformat_md(i = 2:3, j = 'names') |> Stb$autofit()
colformat_md <- function(x, i = NULL, j = NULL,
  h.sizes = c(15, 13, 9), h.colors = c("black", "darkgray"),
  strike = list(color = h.colors[2], underlined = FALSE), bullet = "\U00b7",
  code.font = "monospace", code.shading = "#f8f8f8",
  link.color = "blue", link.underline = TRUE, autolink = TRUE) {

  stopifnot(inherits(x, "flextable"))

  quo_fun <- quo(.format_md(x, h.sizes = h.sizes, h.colors = h.colors,
    strike = strike, bullet = bullet, code.font = code.font,
    code.shading = code.shading, link.color = link.color,
    link.underline = link.underline, autolink = autolink))
  fun_ <- new_function(pairlist2(x = , h.sizes = h.sizes, h.colors = h.colors,
    strike = strike, bullet = bullet, code.font = code.font,
    code.shading = code.shading, link.color = link.color,
    link.underline = link.underline, autolink = autolink), get_expr(quo_fun))

  col_keys <- .filter_col_keys(x, j, function(x) inherits(x, 'noquote'))
  .docall_display(col_keys, fun_, x, i = i)
}
