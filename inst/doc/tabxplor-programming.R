## ----include = FALSE----------------------------------------------------------
# Messages and warnings are off for every chunk: the teaching notes tabxplor prints (an
# auto-detected family, an over-dispersion caveat) are explained in the prose where they
# matter, and repeated under every table they only clutter it. Re-enable one with
# `message = TRUE` on the chunk that needs it.
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      message = FALSE, warning = FALSE)

## ----setup--------------------------------------------------------------------
library(tabxplor)

# Pin the legend language: it defaults to "auto" = the ambient locale, so building this English
# vignette on a French machine silently renders French legends and captions (the -fr articles pin
# "fr" for the same reason). Output must not depend on where it is built.
options(tabxplor.lang = "en")
# The shape table a continuous predictor draws under the footer is not this vignette's subject.
options(tabxplor.shape_table = "no")
Sys.setenv(LANGUAGE = "en")   # the test-summary / model-fit row labels go through gettext, not this option
library(dplyr)

# Tables render as tabxplor's real html tables (the recommended everyday setting); the shared
# stylesheet is emitted once by tab_css() below, and the hover tooltips are kept off here.
options(tabxplor.print = "html")
options(tabxplor.tab_kable_css = FALSE)
options(tabxplor.tab_kable_tooltips = FALSE)

# Console outputs (vectors, fields...) keep their terminal colors, turned to html by fansi.
options(cli.num_colors = 256)
set_color_palette(theme = "light")

## ----echo = FALSE, results = "asis"-------------------------------------------
# The website carries a light/dark switch and tab_css("auto") follows it; a shipped vignette
# is always read on a light page, so there it stays light.
cat(tab_css(theme = if (Sys.getenv("IN_PKGDOWN") == "true") "auto" else "light"))

## ----echo = FALSE, include = FALSE--------------------------------------------
# Colour the console outputs (ANSI -> html, via fansi), but hand as-is results (the html tables,
# marked by knitr with an ASIS token) back to knitr's default hook untouched.
# Escape the three HTML specials before fansi turns the ANSI codes into markup.
esc_html <- function(x) gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", x, fixed = TRUE),
                                               fixed = TRUE), fixed = TRUE)
# fansi is Suggests-only, so the ANSI -> html step degrades: without it the escape codes are
# stripped and the output is handed on uncoloured, which is what a check run with no Suggests gets.
ansi_html <- if (requireNamespace("fansi", quietly = TRUE)) {
  function(x) fansi::sgr_to_html(x = esc_html(x), warn = FALSE)
} else {
  function(x) esc_html(gsub("\033\\[[0-9;]*m", "", x))
}
default_output_hook <- knitr::knit_hooks$get("output")
knitr::knit_hooks$set(output = function(x, options) {
  if (grepl("KNITR_ASIS_OUTPUT_TOKEN", x, fixed = TRUE)) return(default_output_hook(x, options))
  paste0('<pre class="r-output"><code>',
         ansi_html(x),
         '</code></pre>')
})
# A cli message or warning is its own kind of condition, so knitr routes each through its own hook,
# not `output`: without these two it would land in the collapsed source block, ANSI codes and all.
for (hook in c("message", "warning")) {
  knitr::knit_hooks$set(stats::setNames(list(function(x, options) {
    paste0('<pre class="r-output"><code>',
           ansi_html(x),
           '</code></pre>')
  }), hook))
}

## -----------------------------------------------------------------------------
gss_simple <- gss_cat_data_formatting()
tabs <- tab(gss_simple, race, marital, pct = "row", color = "difference")

## -----------------------------------------------------------------------------
tabs |> mutate(across(where(is_fmt), get_num))

## ----eval = FALSE-------------------------------------------------------------
# tabs |> mutate(across(where(is_fmt), format))

## -----------------------------------------------------------------------------
tabs$Married$pct

## -----------------------------------------------------------------------------
as.matrix(tab(gss_simple, race, marital))

## -----------------------------------------------------------------------------
vctrs::vec_data(tabs$Married)

## ----eval = FALSE-------------------------------------------------------------
# tabs$Married$pct
# tabs |> mutate(across(where(is_fmt), ~ .$pct))
# tabs |> mutate(across(where(is_fmt), ~ vctrs::field(., "pct")))

## -----------------------------------------------------------------------------
ci_tab <- tab(gss_simple, race, marital, pct = "row", ci = "cell")
ci_tab$Married$ci_inf
ci_tab$Married$ci_sup

## ----eval = FALSE-------------------------------------------------------------
# tabs |> set_display("diff")
# tabs |> mutate(across(where(is_fmt), ~ set_display(., "diff")))

## ----eval = FALSE-------------------------------------------------------------
# tabs |> mutate(across(where(is_fmt), ~ mutate(., digits = 2L)))

## ----eval = FALSE-------------------------------------------------------------
# tab(gss_simple, race, c(age, tvhours), digits = 1L) |>
#   mutate(across(
#     c(age, tvhours),
#     ~ mutate(., var = sqrt(var), display = "var", digits = 1L) |> set_color("no"),
#     .names = "{.col}_sd"
#   ))

## -----------------------------------------------------------------------------
tabs |> set_display("{pct} ({diff})")

## -----------------------------------------------------------------------------
tab(gss_simple, race, marital, pct = "row") |>
  mutate(across(where(is_fmt), ~ set_display(., "diff"), .names = "{.col}_diff"))

## ----eval = FALSE-------------------------------------------------------------
# tab_reg(gss_simple, "married", c("race", "rincome"), family = "binomial",
#         display = "est_base") |>
#   mutate(Model_pct = Model_OR |> set_display("{base}") |> set_color("") |>
#                        set_pvalue(NA_real_),
#          .after = Model_OR)

## ----eval = FALSE-------------------------------------------------------------
# # fewer decimals on the total row than on the body:
# tab(gss_simple, race, marital, race, pct = "row") |>
#   mutate(across(
#     where(is_fmt),
#     ~ if_else(is_totrow(.), mutate(., digits = 1L), mutate(., digits = 2L))
#   ))

## -----------------------------------------------------------------------------
t <- tab(gss_simple, race, marital, pct = "row", ci = "ref", color = "difference")
tab_structure(t)

## -----------------------------------------------------------------------------
tab_columns(t) |> dplyr::select(column, scale, pct_type, ref, ci_method, totcol)

## -----------------------------------------------------------------------------
fmt_attr(t$Married, "scale")
fmt_attr(t$Married, "ci_method")

## -----------------------------------------------------------------------------
fmt(n = c(10L, 20L, 30L), pct = c(0.1, 0.2, 0.7), display = "pct", digits = 0L)

## -----------------------------------------------------------------------------
counts <- dplyr::count(gss_simple, marital, race)
tab_counts(counts, marital, race, counts = n, pct = "row", color = TRUE)
# identical to tab(gss_simple, marital, race, pct = "row", color = "difference")

## ----eval = FALSE-------------------------------------------------------------
# tab_counts(table(gss_simple$marital, gss_simple$race), pct = "row", color = "difference")
# 
# wide <- tidyr::pivot_wider(counts, names_from = race, values_from = n)
# tab_counts(wide, row_var = marital, cols = c(White, Black, Other),
#            col_name = "race", pct = "row", color = "difference")

## -----------------------------------------------------------------------------
tab(gss_simple, relig, marital, year, pct = "row", totaltab = "no", tot = "row") |>
  dplyr::select(year, relig, Married) |>
  tab_spread(year)

## ----eval = FALSE-------------------------------------------------------------
# specs <- tibble::tribble(
#   ~row_var, ~col_var,  ~pct,
#   "race",   "marital", "row",
#   "relig",  "party3",  "row",
# )
# purrr::pmap(specs, \(row_var, col_var, pct)
#             tab(gss_simple, all_of(row_var), all_of(col_var), pct = pct))

