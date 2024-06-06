#' Default type tabularise generic (tabularise$default)
#'
#' The "default" type is the most obvious tabular representation for an object.
#' For data frames, it tabularises the first few rows and columns (so, in case
#' of a very large object, output remains limited). See also the "headtail" type
#' that creates a table with the few first and last rows of the table (see
#' [tabularise_headtail()]).
#'
#' @param data An object
#' @param ... Further arguments (depending on the object class).
#' @param kind The kind of table to produce: "tt" for tinytable, or "ft" for
#' flextable (default).
#' @param env The environment where to evaluate formulas (you probably do not
#' need to change the default).
#'
#' @return A **flextable** object you can print in different form or rearrange
#' with the \{flextable\} functions from set Stb$verb().
#' @export
#' @seealso [tabularise()], [tabularise_headtail()]
#'
#' @examples
#' tabularise$default(iris)
#' # Same as simply:
#' tabularise(iris)
tabularise_default <- function(data, ..., kind = "ft", env = parent.frame()) {
  UseMethod("tabularise_default")
}

#' @export
#' @rdname tabularise_default
#' @method tabularise_default default
tabularise_default.default <- function(data, ..., kind = "ft", env = parent.frame()) {
  res <- try(as_flextable(data, ...), silent = TRUE)
  if (inherits(res, "try-error"))
    stop("I don't know how to tabularise an object of class '",
      class(data)[1], "'")
  res
}

#' @export
#' @importFrom flextable add_footer_lines as_paragraph as_i autofit
#' @rdname tabularise_default
#' @param formula A formula to create a table using the \{tables\} syntax
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
      labels <- .handle_duplicate_labels(labels,replace_with_name = TRUE)
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
    align(res, align = "center", part = "header") |>
      valign(valign = "bottom", part = "header") |>
      autofit()
  } else {
    autofit(res)
  }
}

#' @export
#' @rdname tabularise_default
#' @param rownames col_keys to use for row names. If `FALSE`, do not add
#' row names. If a string, a first column is added in the table with that string
#' as label
#' @method tabularise_default matrix
tabularise_default.matrix <- function(data, col_keys = colnames(data),
rownames = " ", cwidth = 0.75, cheight = 0.25, ..., env = parent.frame()) {
  # TODO: allow using labels... except that column labels are not defined for
  # matrix object !?
  df <- as.data.frame(data)
  if (!isFALSE(rownames)) {# Add row names as first column
    rn <- rownames(data)
    if (is.null(rn))
      rn <- seq_len(NROW(data))
    df <- data.frame(rn, df)
    rownames <- as.character(rownames)[1]
    names(df)[1] <- rownames
    if (!is.null(col_keys))
      col_keys <- c(rownames, col_keys)
  }
  if (is.null(col_keys))
    col_keys <- names(df)
  flextable(df, col_keys = col_keys, cwidth = cwidth, cheight = cheight) |>
    colformat_sci() |>
    autofit()
}

#' @export
#' @rdname tabularise_default
#' @importFrom flextable add_footer_lines add_header_lines align
#' @param header do we add a header?
#' @param title do we add a title?
#' @param footer do we add a footer?
#' @param lang the natural language to use. The default value can be set with,
#'   e.g., `options(data.io_lang = "fr")` for French.
#' @method tabularise_default Correlation
tabularise_default.Correlation <- function(data, col_keys = colnames(data),
rownames = " ", header = TRUE, title = header, footer = TRUE,
cwidth = 0.75, cheight = 0.25, lang = getOption("data.io_lang", "en"),  ...,
env = parent.frame()) {
  # TODO: allow using labels with origdata= here?
  # row names should be the same as col_keys
  rownames(data) <- col_keys
  ft <- tabularise(unclass(data))

  # Add header and footer
  if (isTRUE(header)) {
    if (isTRUE(title)) {
      method <- .infos_lang.tb(lang)[["method"]]
      method <- method[attr(data, "method")]
      # In case the method is not known
      if (is.na(method))
        method <- attr(data, "method")
      if (!is.null(method)) {
        ft <- add_header_lines(ft, values = para_md(method))
        ft <- align(ft, i = 1, align = "right", part = "header")
      }
    }
  }

  if (isTRUE(footer)) {
    na_method <- .infos_lang.tb(lang)[["na_method"]]
    na_method <- na_method[attr(data, "na.method")]
    if (!is.null(na_method) && !is.na(na_method))
      ft <- add_footer_lines(ft, values = para_md(paste0("*", na_method, "*")))
  }
  ft
}

# Internal function : Choose the lang and the infos_lang ----
.infos_lang.tb <- function(lang = getOption("data.io_lang", "en")) {
  lang <- tolower(lang)
  if (lang != "fr") lang <- "en" # Only en or fr for now
  if (lang == "fr") {
    .infos_fr.tb
  } else {
    .infos_en.tb
  }
}

.infos_en.tb <- list(
  method = c(
    "Pearson's product-moment correlation" =
      "Matrix of Pearson's product-moment correlation *r*",
    "Kendall's rank correlation tau" =
      "Matrix of Kendall's rank correlation $\\tau$",
    "Spearman's rank correlation rho" =
      "Matrix of Spearman's rank correlation $\\rho$",
    "PCA variables and components correlation" =
      "PCA variables and components correlation"
  ),
  na_method = c(
    "all.obs" = NULL, # Generates an error if there are NAs
    "complete.obs" =
      "Only complete observations for all vars are used.",
    "pairwise.complete.obs" =
      "Missing data are pairwise deleted.",
    "everything" = NULL, # NAs propagate, nothing eliminated
    "na.or.complete" =
      "Only complete observations for all vars are used."
    # Note: this last one is identical to complete.obs, but produces NA if
    # there are no complete case, while complete.obs produces an error
  )
)

.infos_fr.tb <- list(
  method = c(
    "Pearson's product-moment correlation" =
      "Matrice de coefficients de corr\u00e9lation de Pearson *r*",
    "Kendall's rank correlation tau" =
      "Matrice de coefficients de corr\u00e9lation des rangs de Kendall $\\tau$",
    "Spearman's rank correlation rho" =
      "Matrice de coefficients de corr\u00e9lation des rangs de Spearman $\\rho$",
    "PCA variables and components correlation" =
      "Corr\u00e9lation entre variables et composantes de l'ACP"
  ),
  na_method = c(
    "all.obs" = NULL, # Generates an error if there are NAs
    "complete.obs" =
      "Seules les observations compl\u00e8tes pour toutes les variables sont utilis\u00e9es.",
    "pairwise.complete.obs" =
      "Les donn\u00e9es manquantes ont \u00e9t\u00e9 \u00e9limin\u00e9es par paires.",
    "everything" = NULL, # NAs propagate, nothing eliminated
    "na.or.complete" =
      "Seules les observations compl\u00e8tes pour toutes les variables sont utilis\u00e9es."
    # Note: this last one is identical to complete.obs, but produces NA if
    # there are no complete case, while complete.obs produces an error
  )
)
