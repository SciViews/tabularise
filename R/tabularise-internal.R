.onLoad <- function(libname, pkgname){
  # Define default themes for tables (both tinytable and flextable)
  options(tinytable_tt_theme = theme_tt_sciviews)
  flextable::set_flextable_defaults(big.mark = "\u00a0",
    font.family = "Arial", font.size = 12)
}

# Internal function for tabularise

# Copy of flextable:::get_j_from_formula() from v 0.9.2
.get_j_from_formula <- function(f, data) {
  if (length(f) > 2)
    stop("formula selection is not as expected ( ~ variables )", call. = FALSE)
  j <- attr(terms(f, data = data), "term.labels")
  j <- gsub("(^`|`$)", "", j)
  names_ <- names(data)
  invalid_names <- (!j %in% names_)
  if (any(invalid_names)) {
    invalid_names <- paste0("`", j[invalid_names], "`", collapse = ", ")
    stop(sprintf("`%s` is using unknown variable(s): %s",
      format(f), invalid_names), call. = FALSE)
  }
  j
}

# Copy of flextable:::get_columns_id() from v 0.9.2
.get_columns_id <- function(x, j = NULL) {
  maxcol <- length(x$col_keys)
  if (is.null(j)) {
    j <- seq_along(x$col_keys)
  }
  if (inherits(j, "formula")) {
    tmp_dat <- as.list(x$col_keys)
    names(tmp_dat) <- x$col_keys
    tmp_dat <- as.data.frame(tmp_dat, check.names = FALSE)
    j <- .get_j_from_formula(j, tmp_dat)
  }
  if (is.numeric(j)) {
    if (length(j) > 0 && all(j < 0)) {
      j <- setdiff(seq_along(x$col_keys), -j)
    }
    if (any(j < 1 | j > maxcol)) {
      stop(sprintf(
        "invalid columns selection\navailable range: [%s]\nissues: %s",
        paste0(range(seq_len(maxcol)), collapse = ", "),
        paste0(setdiff(j, seq_len(maxcol)), collapse = ", ")))
    }
  } else if (is.logical(j)) {
    if (length(j) != maxcol) {
      stop(sprintf(
        "invalid columns selection\n`j` should have a length of %.0f.",
        maxcol))
    } else {
      j <- which(j)
    }
  }
  else if (is.character(j)) {
    j <- gsub("(^`|`$)", "", j)
    if (any(is.na(j))) {
      stop("invalid columns selection: NA in selection")
    }
    else if (!all(is.element(j, x$col_keys))) {
      stop(sprintf("`%s` is using unknown variable(s): %s",
        "i", paste0("`", j[!is.element(j, x$col_keys)],
          "`", collapse = ",")))
    } else {
      j <- match(j, x$col_keys)
    }
  } else {
    stop("invalid columns selection: unknown selection type")
  }
  j
}

# Copy of flextable:::.filter_col_keys() from v 0.9.2
.filter_col_keys <- function(x, j, fun) {
  j <- .get_columns_id(x[["body"]], j)
  col_keys <- x$col_keys[j]
  col_keys[vapply(x[["body"]]$dataset[col_keys], fun, FUN.VALUE = NA)]
}

# Copy of flextable:::docall_display()
.docall_display <- function(col_keys, fun, x, i = NULL, part = "body") {
  if (inherits(i, "formula") && part %in% c("header", "footer"))
    stop("formulas are not supported in the 'header' and 'footer' parts.")

  for (varname in col_keys) {
    x <- flextable::mk_par(x = x, j = varname, i = i,
      value = fun(get(varname)), part = part)
  }
  x
}

# Links in R terminals and RStudio console are still experimental. So, we keep
# this function internal so that adaptation does not break the package for user
# .peek_help_link <- function(topic) {
#   if (is_interactive() && cli::ansi_has_hyperlink_support() &&
#       isTRUE(cli::ansi_hyperlink_types()$help)) { # RStudio help link
#     # topic must be package::helpage
#     pkg_page <- strsplit(topic, "::", fixed = TRUE)[[1]]
#     # Assume package = base if not provided
#     if (length(pkg_page) == 1L)
#       pkg_page <- c("base", pkg_page)
#     cli::style_hyperlink(topic, "ide:help",
#       params = c(package = pkg_page[1], topic = pkg_page[2]))
#   } else {
#     # Construct an https link like...
#     # "<https://davidgohel.github.io/flextable/reference/flextable.html>"
#     # for terminals that support hyperlinks like the Gnome terminal
#     # For now, just return topic (no hyperlink)
#     paste0("?", topic)
#   }
# }


# This function handles duplicates in a vector of labels. It either replaces
# duplicates with their names or adds a number to the end of each duplicate
# element. If replace_with_name is TRUE (default), duplicates are replaced with
# their names.
.handle_duplicate_labels <- function(x, replace_with_name = TRUE) {
  # Check for duplicates
  #duplicates  <- collapse::fduplicated(x)
  duplicates <- duplicated(x) | duplicated(x, fromLast = TRUE)

  if (any(duplicates)) {
    warning("There are duplicate elements in the labels.\n",
      "Tip: Use svBase::labelise() to replace your duplicate labels.",
      call. = FALSE)

    if (replace_with_name) {
      x[duplicates] <- names(x)[duplicates]
    } else {
      counts <- table(x)
      duplicates <- names(counts[counts > 1])
      # For each duplicate element, add a number at the end
      for (name in duplicates) {
        index <- which(x == name)
        x[index] <- paste(x[index], "(", seq_along(index), ")", sep = "")
      }
    }
  }

  x
}
