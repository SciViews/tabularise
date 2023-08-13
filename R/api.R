# {flextable} API - more consistent function names ------------------------

# For now, these functions are not exported and only accessible via the tb_ set.

# 54 synonym functions for tabularise over flextable
# Explanation for adding these synonyms:
#
# 1) flextable function are verbs (mostly) but when they are not, they could be
# confused too easily with functions with same name in other R packages
# e.g.: bold vs crayon::bold, border vs ggpubr::border, etc.
# To satisfy verb + avoid name clash, place set_ before the name (set_bold).
#
# 2) There is a confusion between theme_xxx in ggplot and flextable. This is
# annoying when autocompleting themes -> flextheme_xxx as synonyms.
#
# 3) There is a possible clash between fmt_xxx and gt::fmt_xxx -> chkformat_xxx
# are synonyms to fmt_xxx in tabularise (reminiscent to colformat_). Also
# homogenise chr <- char and dbl <- double for colformat_xxx.
#
# 4) border_ vs (h|v)line is confusing => rename everything border_xxxx
#
# 5) Make sure to end functions that create chunks with _chunk (linerange,
# lollipop, minibar and chunk_dataframe has df_chunk as synonym).
#
# 6) 9 special synonyms to further minimise possible clashes or rationalise:
# higlight_text = highlight, rotate_text = rotate,
# align_h = align, align_v = valign,
# autofit_size = autofit, set_col_labels = labelizor, set_col_blank = void,
# compose_chunks = compose, as_color = colorize
aka <- svMisc::aka

set_style <- aka(flextable::style)
set_font <- aka(flextable::font)
set_fontsize <- aka(flextable::fontsize)
set_italic <- aka(flextable::italic)
set_bold <- aka(flextable::bold)
set_color <- aka(flextable::color)
highlight_text <- aka(flextable::highlight)
rotate_text <- aka(flextable::rotate)
align_h <- aka(flextable::align)
align_v <- aka(flextable::valign)
set_padding <- aka(flextable::padding)
set_bg <- aka(flextable::bg)
set_line_spacing <- aka(flextable::line_spacing)
border_outer_t <- aka(flextable::hline_top)
border_outer_b <- aka(flextable::hline_bottom)
border_outer_l <- aka(flextable::vline_left)
border_outer_r <- aka(flextable::vline_right)
border_inner_b <- aka(flextable::hline)
border_inner_r <- aka(flextable::vline)
border_cell <- aka(flextable::border)
border_around <- aka(flextable::surround)
border_none <- aka(flextable::border_remove)
flextheme_alafoli <- aka(flextable::theme_alafoli)
flextheme_apa <- aka(flextable::theme_apa)
flextheme_booktabs <- aka(flextable::theme_booktabs)
flextheme_box <- aka(flextable::theme_box)
flextheme_tron <- aka(flextable::theme_tron)
flextheme_tron_legacy <- aka(flextable::theme_tron_legacy)
flextheme_vader <- aka(flextable::theme_vader)
flextheme_vanilla <- aka(flextable::theme_vanilla)
flextheme_zebra <- aka(flextable::theme_zebra)
set_footnote <- aka(flextable::footnote)
autofit_size <- aka(flextable::autofit)
set_width <- aka(flextable::width)
set_height <- aka(flextable::height)
set_height_all <- aka(flextable::height_all)
colformat_chr <- aka(flextable::colformat_char)
colformat_dbl <- aka(flextable::colformat_double)
set_col_labels <- aka(flextable::labelizor)
set_col_blank <- aka(flextable::void)
compose_chunks <- aka(flextable::compose)
as_color <- aka(flextable::colorize)
as_hyperlink <- aka(flextable::hyperlink_text)
linerange_chunk <- aka(flextable::linerange)
lollipop_chunk <- aka(flextable::lollipop)
minibar_chunk <- aka(flextable::minibar)
df_chunk <- aka(flextable::chunk_dataframe)
chkformat_int <- aka(flextable::fmt_int)
chkformat_dbl <- aka(flextable::fmt_dbl)
chkformat_percent <- aka(flextable::fmt_pct)
chkformat_n_percent <- aka(flextable::fmt_n_percent)
chkformat_mean_sd <- aka(flextable::fmt_avg_dev)
chkformat_stats <- aka(flextable::fmt_2stats) #Same as fmt_summarizor?
chkformat_header_n <- aka(flextable::fmt_header_n)

# We do not use ftExtra (too slow for rendering markdown and too much
# dependencies), but we lack some formattings
as_mono <- function(x, font = "monospace", shading = "#f8f8f8") {
  if (!inherits(x, "chunk")) {
    x <- as_chunk(x)
  }
  x$font.family = font
  x$shading.color = shading
  x
}

as_underline <- function(x) {
  if (!inherits(x, "chunk")) {
    x <- as_chunk(x)
  }
  x$underlined = TRUE
  x
}

# Not useful?
#as_eq <- function(x) {
#  if (!inherits(x, "chunk")) {
#    x <- as_chunk(x)
#  }
#  x$eq_data <- x$txt
#  x$txt <- NA
#  x
#}
