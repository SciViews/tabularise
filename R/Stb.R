# .hyperlink_type <- {
#   if (is_interactive() && cli::ansi_has_hyperlink_support()) {
#     types <- cli::ansi_hyperlink_types()
#     if (isTRUE(types$help)) {
#       "help"
#     } else if (isTRUE(types$href)) {
#       "href"
#     } else {
#       "none"
#     }
#   } else {
#     "none"
#   }
# }


# aka <- function(...) {
#   res <- svMisc::aka(...)
#   if (is.function(res))
#     class(res) <- c("aka", "function")
#   res
# }

# print.aka <- function(x, hyperlink_type = .hyperlink_type, ...) {
#   src <- attr(comment(x), "src")
#   link <- switch(hyperlink_type,
#     help = {
#       # src must be package::helpage
#       pkg_page <- strsplit(src, "::", fixed = TRUE)[[1]]
#       # Assume package = base if not provided
#       if (length(pkg_page) == 1L)
#         pkg_page <- c("base", pkg_page)
#       cli::style_hyperlink(src, "ide:help",
#         params = c(package = pkg_page[1], topic = pkg_page[2]))
#     },
#     href = src, # TODO: use an https:// URL here
#     src)
#
#   cat(cli::col_blue("\b = ", link, "()"))
#   invisible(x)
# }
#
# str.aka <- function(object, ...) {
#   cat("aka ", attr(comment(object), "src"), "()\n", sep = "")
# }

# section <- function(x, title) {
#   structure(function() get_section(x, title),
#     title = title, class = c("section", "function"))
# }
#
# print.section <- function(x, ...) {
#   title <- attr(x, "title")
#   # This is for RStudio. In terminal, use: cat("XXXXXXXXXXX\n\033[1A\033[KY\n")
#   back <- rep("\b", nchar(title) + 7L)
#   cat(back, cli::col_red("o  ", title, "   "), sep = "")
#   invisible(x)
# }
#
# str.section <- function(object, ...) {
#   cat("section\n")
# }
#
# get_section <- function(x, title) {
#   stopifnot(is.list(x), is.character(title), length(title) == 1L)
#
#   # We need tp rework title, so that it matches a section name
#   # Section title -> o__SECTION_TITLE__
#   title <- toupper(title)
#   title <- gsub(" ", "_", title, fixed = TRUE)
#   title <- paste0("o__", title, "__")
#
#   # Search the section in the list
#   names <- names(x)
#   l <- length(names)
#   start <- which(names == title)
#   if (!length(start)) # The section title is not found -> return an empty list
#       return(list())
#   end <- which(startsWith(names[(start + 1):l], "o__"))
#   if (length(end)) {
#     end <- end[1] + start - 1
#   } else {
#     end <- l
#   }
#   # Truncate the list
#   sel <- start:end
#   x[sel]
# }

#' Tabularise set of function (mainly from \{flextable\})
#'
#' This set provides all the functions you can use to manipulate `tabularise()`
#' tables. They mostly contain the \{flextable\} API. You are supposed to
#' use it like `Stb$verb(....)` where `verb` is one of the objects contained in
#' the collection. Use `Stb` to list all objects in the set.
#'
#' @return When printing `Stb` alone, a list of all verbs and other objects
#' provided in the set are returned.
#' @export
#' @importFrom svMisc aka section
#' @importFrom flextable flextable as_flextable as_chunk
#' @importFrom officer fp_border fp_text fp_par fp_cell
#'
#' @examples
#' # TODO...
Stb <- #structure(
  list(

    "o__FORMAT_GENERAL__"     = section(Stb, "FORMAT GENERAL"),
    get_flextable_defaults    = aka(flextable::get_flextable_defaults,
      url = "https://davidgohel.github.io/flextable/reference/get_flextable_defaults.html"),
    set_flextable_defaults    = aka(flextable::set_flextable_defaults,
      url = "https://davidgohel.github.io/flextable/reference/set_flextable_defaults.html"),
    init_flextable_defaults   = aka(flextable::init_flextable_defaults,
      url = "https://davidgohel.github.io/flextable/reference/set_flextable_defaults.html"),
    set_table_properties      = aka(flextable::set_table_properties,
      url = "https://davidgohel.github.io/flextable/reference/set_table_properties.html"),
    style                     = aka(flextable::style, # set_style
      url = "https://davidgohel.github.io/flextable/reference/style.html"),
    before                    = aka(flextable::before,
      url = "https://davidgohel.github.io/flextable/reference/before.html"),
    #as_grouped_data           = aka(flextable::as_grouped_data), # does not create a flextable!

    "o__FORMATTING_PROPERTIES__" = section(Stb, "FORMATTING PROPERTIES"),
    fp_text                   = aka(officer::fp_text),
    fp_text_default           = aka(flextable::fp_text_default),
    fp_border                 = aka(officer::fp_border),
    fp_border_default         = aka(flextable::fp_border_default),
    fp_par                    = aka(officer::fp_par),
    fp_cell                   = aka(officer::fp_cell),

    "o__FORMAT_TEXT__"        = section(Stb, "FORMAT TEXT"),
    font                      = aka(flextable::font), # set_font
    fontsize                  = aka(flextable::fontsize), # set_fontsize
    italic                    = aka(flextable::italic), # set_italic
    bold                      = aka(flextable::bold), # set_bold
    color                     = aka(flextable::color), # set_color
    highlight                 = aka(flextable::highlight), # highlight_text
    rotate                    = aka(flextable::rotate), # rotate_text

    "o__FORMAT_CELL__"        = section(Stb, "FORMAT CELL"),
    align                     = aka(flextable::align), # align_h
    valign                    = aka(flextable::valign), # align_v
    align_text_col            = aka(flextable::align_text_col),
    align_nottext_col         = aka(flextable::align_nottext_col),
    padding                   = aka(flextable::padding), # set_padding
    bg                        = aka(flextable::bg), # set_bg
    line_spacing              = aka(flextable::line_spacing), # set_line_spacing

    "o__FORMAT_LINE_BORDER__" = section(Stb, "FORMAT LINE BORDER"),
    border                    = aka(flextable::border), # border_cell
    border_inner              = aka(flextable::border_inner),
    border_inner_h            = aka(flextable::border_inner_h),
    border_inner_v            = aka(flextable::border_inner_v),
    border_outer              = aka(flextable::border_outer),
    border_remove             = aka(flextable::border_remove), # border_none
    surround                  = aka(flextable::surround), # border_around
    hline                     = aka(flextable::hline), # border_inner_b
    hline_top                 = aka(flextable::hline_top), # border_outer_t
    hline_bottom              = aka(flextable::hline_bottom), # border_outer_b
    vline                     = aka(flextable::vline), # border_inner_r
    vline_left                = aka(flextable::vline_left), # border_outer_l
    vline_right               = aka(flextable::vline_right), # border_outer_r

    "o__FLEXTHEMES__"         = section(Stb, "FLEXTHEMES"),
    # TODO: add a SciViews theme + flexthemes???
    theme_alafoli             = aka(flextable::theme_alafoli),
    theme_apa                 = aka(flextable::theme_apa),
    theme_booktabs            = aka(flextable::theme_booktabs),
    theme_box                 = aka(flextable::theme_box),
    theme_tron                = aka(flextable::theme_tron),
    theme_tron_legacy         = aka(flextable::theme_tron_legacy),
    theme_vader               = aka(flextable::theme_vader),
    theme_vanilla             = aka(flextable::theme_vanilla),
    theme_zebra               = aka(flextable::theme_zebra),

    "o__BODY_HEADER_FOOTER__" = section(Stb, "BODY HEADER FOOTER"),
    add_body                  = aka(flextable::add_body),
    add_body_row              = aka(flextable::add_body_row),
    add_header                = aka(flextable::add_header),
    add_header_row            = aka(flextable::add_header_row),
    add_header_lines          = aka(flextable::add_header_lines),
    set_header_df             = aka(flextable::set_header_df),
    set_header_labels         = aka(flextable::set_header_labels),
    separate_header           = aka(flextable::separate_header),
    add_footer                = aka(flextable::add_footer),
    add_footer_row            = aka(flextable::add_footer_row),
    add_footer_lines          = aka(flextable::add_footer_lines),
    set_footer_df             = aka(flextable::set_footer_df),
    delete_part               = aka(flextable::delete_part),

    "o__CELL_MERGING__"       = section(Stb, "CELL MERGING"),
    merge_at                  = aka(flextable::merge_at),
    merge_h                   = aka(flextable::merge_h),
    merge_h_range             = aka(flextable::merge_h_range),
    merge_v                   = aka(flextable::merge_v),
    merge_none                = aka(flextable::merge_none),
    fix_border_issues         = aka(flextable::fix_border_issues),

    "o__CAPTIONS_FOOTNOTES__" = section(Stb, "CAPTIONS FOOTNOTES"),
    set_caption               = aka(flextable::set_caption),
    footnote                  = aka(flextable::footnote), # set_footnote

    "o__TABLE_SIZE__"         = section(Stb, "TABLE SIZE"),
    ncol_keys                 = aka(flextable::ncol_keys),
    nrow_part                 = aka(flextable::nrow_part),
    flextable_dim             = aka(flextable::flextable_dim),
    dim                       = aka(base::dim), # dim.flextable method
    dim_pretty                = aka(flextable::dim_pretty),
    autofit                   = aka(flextable::autofit), # autofit_size
    empty_blanks              = aka(flextable::empty_blanks),
    width                     = aka(flextable::width), # set_width
    fit_to_width              = aka(flextable::fit_to_width),
    height                    = aka(flextable::height), # set_height
    height_all                = aka(flextable::height_all), # set_height_all
    hrule                     = aka(flextable::hrule),

    "o__CELL_CONTENT_FORMAT__" = section(Stb, "CELL CONTENT FORMAT"),
    colformat_char            = aka(flextable::colformat_char), # colformat_chr
    colformat_date            = aka(flextable::colformat_date),
    colformat_datetime        = aka(flextable::colformat_datetime),
    colformat_int             = aka(flextable::colformat_int),
    colformat_double          = aka(flextable::colformat_double), # colformat_dbl
    colformat_num             = aka(flextable::colformat_num),
    colformat_lgl             = aka(flextable::colformat_lgl),
    colformat_image           = aka(flextable::colformat_image),
    set_formatter             = aka(flextable::set_formatter),
    labelizor                 = aka(flextable::labelizor), # set_col_labels
    void                      = aka(flextable::void), # set_col_blank

    "o__CELL_MULTIFORMAT__"   = section(Stb, "CELL MULTIFORMAT"),
    compose                   = aka(flextable::compose), # compose_chunk, also mk_par
    append_chunks             = aka(flextable::append_chunks),
    prepend_chunks            = aka(flextable::prepend_chunks),
    as_paragraph              = aka(flextable::as_paragraph),
    as_chunk                  = aka(flextable::as_chunk),
    as_bracket                = aka(flextable::as_bracket),
    as_b                      = aka(flextable::as_b),
    as_i                      = aka(flextable::as_i),
    as_sub                    = aka(flextable::as_sub),
    as_sup                    = aka(flextable::as_sup),
    as_highlight              = aka(flextable::as_highlight),
    #as_mono                   = aka(tabularise::as_mono), # Our own function
    #as_underline              = aka(tabularise::as_underline), # Our own function
    colorize                  = aka(flextable::colorize), # as_color
    hyperlink_text            = aka(flextable::hyperlink_text), # as_hyperlink
    as_equation               = aka(flextable::as_equation),
    as_image                  = aka(flextable::as_image),
    plot_chunk                = aka(flextable::plot_chunk),
    linerange                 = aka(flextable::linerange), # linerange_chunk
    lollipop                  = aka(flextable::lollipop), # lollipop_chunk
    minibar                   = aka(flextable::minibar), # minibar_chunk
    chunk_dataframe           = aka(flextable::chunk_dataframe), # df_chunk
    gg_chunk                  = aka(flextable::gg_chunk),
    grid_chunk                = aka(flextable::grid_chunk),

    "o__CHUNK_FOMATTERS__"    = section(Stb, "CHUNK FOMATTERS"),
    fmt_int                   = aka(flextable::fmt_int), # Relace fmt by chkformat to avoid clash with gt:: everywhere?
    fmt_dbl                   = aka(flextable::fmt_dbl),
    fmt_pct                   = aka(flextable::fmt_pct), # fmt_percent
    fmt_n_percent             = aka(flextable::fmt_n_percent),
    fmt_avg_dev               = aka(flextable::fmt_avg_dev), # fmt_mean_sd
    fmt_2stats                = aka(flextable::fmt_2stats), # fmt_stats? Same as fmt_summarizor?
    fmt_header_n              = aka(flextable::fmt_header_n),

    "o__RENDERING__"          = section(Stb, "RENDERING"),
    # Also print(ft, preview = "...")
    save_as_html              = aka(flextable::save_as_html), # Should be in write()?
    save_as_docx              = aka(flextable::save_as_docx), # Should be in write()?
    save_as_pptx              = aka(flextable::save_as_pptx), # Should be in write()?
    save_as_rtf               = aka(flextable::save_as_rtf), # Should be in write()?
    save_as_image             = aka(flextable::save_as_image), # Should be in write()?
    flextable_to_rmd          = aka(flextable::flextable_to_rmd),
    htmltools_value           = aka(flextable::htmltools_value), # Render ft in Shiny (flextableOutput for Shiny)
    gen_grob                  = aka(flextable::gen_grob), # also as_raster. Limitation: no equation or hyperlink
    use_df_printer            = aka(flextable::use_df_printer), # Also df_print method
    use_model_printer         = aka(flextable::use_model_printer),
    add_latex_dep             = aka(flextable::add_latex_dep),
    flextable_html_dependency = aka(flextable::flextable_html_dependency)

    #"o__RENDERING_OFFICE__"   = fsection(Stb, "RENDERING OFFICE"), (rename office_... to indicate specific use?)
    #body_add_flextable        = aka(flextable::body_add_flextable),
    #body_replace_flextable_at_bkm = aka(flextable::body_replace_flextable_at_bkm),
    #headers_flextable_at_bkm  = aka(flextable::headers_flextable_at_bkm),
    #footers_flextable_at_bkm  = aka(flextable::footers_flextable_at_bkm),
    #as_word_field             = aka(flextable::as_word_field),
    #keep_with_next            = aka(flextable::keep_with_next),
    #paginate                  = aka(flextable::paginate),
  ) #, set = "Stb", class = c("fun_set", "list"))

# TODO: not needed any more!
# @export
# @rdname Stb
# @param x A Stb object
# @param section Print only a section in the set
# @param ... Further arguments (not used yet)
# @method print fun_set
# print.fun_set <- function(x, section = NULL, ...) {
#   names <- names(x)
#
#   # Possibly restrict to a given section
#   l <- length(names)
#   sel <- 1:l # Select everything by default
#   if (!is.null(section)) {
#     section <- paste0("o__", toupper(section), "__")
#     start <- which(names == section)
#     if (!length(start)) {
#       warning("Section not found, displaying everything")
#     } else {
#       end <- which(startsWith(names[(start + 1):l], "o__"))
#       if (length(end)) {
#         end <- end[1] - 1
#       } else {
#         end <- l
#       }
#       sel <- start:end
#     }
#   }
#   items <- names <- names[sel]
#
#   # For titles
#   is_title <- sapply(names, startsWith, prefix = "o__")
#
#   # Format titles
#   items[is_title] <- cli::col_red(items[is_title])
#
#   # Format items
#   items[!is_title] <- paste0("  $", names[!is_title])
#   # When the item or src is a function, append () to make it clear
#   is_fun <- sapply(x[sel], is.function)
#   items[is_fun & !is_title] <- paste0(items[is_fun & !is_title], "()")
#
#   # Reformat titles
#   items[is_title] <- gsub("_", " ", items[is_title], fixed = TRUE)
#
#   srcs <- sapply(x[sel], function(obj) attr(comment(obj), "src") %||% "")
#   # Default sources is the same function as the name
#   srcs[srcs == "" & !is_title] <- names[srcs == "" & !is_title]
#
#   # Add a link to items
#   for (i in 1:sum(!is_title))
#     items[!is_title][i] <- paste0(items[!is_title][i], cli::col_blue(" = ",
#     .peek_help_link(srcs[!is_title][i])))
#
#   cat(items, sep = "\n")
#
#   invisible(x)
# }

# TODO: not needed any more!
# @export
# @rdname Stb
# @method .DollarNames subsettable_Stb
# .DollarNames.subsettable_Stb <- function(x, pattern = "") {
#   l <- c(
#     # Format general
#     "get_flextable_defaults", "set_flextable_defaults",
#     "init_flextable_defaults", "set_table_properties", "set_style", "before",
#
#     # Formatting properties (officer) + print, format & update methods
#     "fp_text", "fp_text_default", "fp_border", "fp_border_default", "fp_par",
#     "fp_cell",
#
#     # Format text
#     "set_font", "set_fontsize", "set_italic", "set_bold", "set_color",
#     "highlight_text", "rotate_text",
#
#     # Format cell
#     "align_h", "align_v", "align_text_col", "align_nottext_col",
#     "set_padding", "set_bg", "set_line_spacing",
#
#     # Format border (use fp_border)
#     "border_outer",
#     "border_outer_t", "border_outer_b", "border_outer_l", "border_outer_r",
#     "border_inner",
#     "border_inner_h", "border_inner_v", "border_inner_b", "border_inner_r",
#     "border_cell", "border_around",
#     "border_none",
#
#     # Flexthemes
#     "flextheme_alafoli", "flextheme_apa", "flextheme_booktabs", "flextheme_box",
#     "flextheme_tron", "flextheme_tron_legacy", "flextheme_vader",
#     "flextheme_vanilla", "flextheme_zebra",
#
#     # Layout body, header and footer - colwidths
#     "add_body", "add_body_row",
#     "add_header", "add_header_row", "add_header_lines", "set_header_df",
#     "set_header_labels", "separate_header", "add_footer", "add_footer_row",
#     "add_footer_lines", "set_footer_df", "delete_part",
#
#     # Layout cell merging
#     "merge_at", "merge_h", "merge_h_range", "merge_v", "merge_none",
#     "fix_border_issues",
#
#     # Layout captions & footnotes
#     "set_caption", "set_footnote",
#
#     # Table size
#     "ncol_keys", "nrow_part", "flextable_dim", "dim", "dim_pretty",
#     "autofit_size", "empty_blanks", "set_width", "set_width_max",
#     "set_height", "set_height_all", "hrule",
#
#     # Cell content simple formatting
#     "colformat_chr", "colformat_date", "colformat_datetime",
#     "colformat_int", "colformat_dbl", "colformat_num", "colformat_lgl",
#     "colformat_image", "set_formatter", "set_col_labels", "set_col_blank",
#
#     # Cell formatting multicontent
#     "compose_chunks", "append_chunks", "prepend_chunks", "as_paragraph",
#     "as_chunk", "as_bracket", "as_b", "as_i", "as_sub", "as_sup",
#     "as_highlight", "as_mono", "as_underline", "as_color", "as_hyperlink",
#     "as_equation", "as_image",
#     "plot_chunk", "linerange_chunk", "lollipop_chunk", "minibar_chunk",
#     "df_chunk", "gg_chunk", "grid_chunk",
#
#     # Chunk formatting
#     "chkformat_int", "chkformat_dbl", "chkformat_percent",
#     "chkformat_n_percent", "chkformat_mean_sd", "chkformat_stats",
#     "chkformat_header_n",
#
#     # Rendering also print(ft, preview = "...")
#     "save_as_html", "save_as_docx", "save_as_pptx", "save_as_rtf",
#     "save_as_image", "flextable_to_rmd", "htmltools_value", "gen_grob",
#     "use_df_printer", "use_model_printer",
#     "add_latex_dep", "flextable_html_dependency"
#
#     # Rendering with officer
#     #"body_add_flextable", "body_replace_flextable_at_bkm",
#     #"headers_flextable_at_bkm", "footers_flextable_at_bkm",
#     #"as_word_field", "keep_with_next", "paginate"
#   )
#   # We need to add opening parenthesis for functions
#   l <- paste0(l, "(")
#   sort(l)
# }
