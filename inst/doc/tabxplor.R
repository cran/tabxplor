## ----setup, include = FALSE---------------------------------------------------
# Messages and warnings are off for every chunk: the teaching notes tabxplor prints (an
# auto-detected family, an over-dispersion caveat) are explained in the prose where they
# matter, and repeated under every table they only clutter it. Re-enable one with
# `message = TRUE` on the chunk that needs it.
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)
library(tabxplor)

# Pin the legend language: it defaults to "auto" = the ambient locale, so building this English
# vignette on a French machine silently renders French legends and captions (the -fr articles pin
# "fr" for the same reason). Output must not depend on where it is built.
options(tabxplor.lang = "en")
Sys.setenv(LANGUAGE = "en")   # the test-summary / model-fit row labels go through gettext, not this option

# The tables below are tabxplor's real html tables, rendered live: the setup sets
# options(tabxplor.print = "html") --- the recommended everyday setting in RStudio/Positron ---
# so every bare tab() chunk knits as a colored html table. The shared stylesheet is emitted ONCE
# by the tab_css() chunk below (tab_kable_css = FALSE stops each table re-inlining it), and the
# hover tooltips are off document-wide (see the dedicated tooltips section, which re-enables them).
options(tabxplor.print = "html")
options(tabxplor.tab_kable_css = FALSE)
options(tabxplor.tab_kable_tooltips = FALSE)

# The few console examples still show terminal colors: cli emits ANSI (options("cli.num_colors")),
# and the fansi hook below turns that ANSI into colored HTML.
options(cli.num_colors = 256)
set_color_palette(theme = "light") # type = "text"

## ----echo = FALSE, results = "asis"-------------------------------------------
# The website carries a light/dark switch and tab_css("auto") follows it; a shipped vignette
# is always read on a light page, so there it stays light.
cat(tab_css(theme = if (Sys.getenv("IN_PKGDOWN") == "true") "auto" else "light"))

## ----eval = FALSE-------------------------------------------------------------
# install.packages("tabxplor", dependencies = TRUE)

## ----eval = FALSE-------------------------------------------------------------
# library(tabxplor)

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

## ----echo = TRUE--------------------------------------------------------------
gss_simple <- gss_cat_data_formatting()

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, marital, race)

## ----eval = FALSE-------------------------------------------------------------
# options(tabxplor.print = "html")

## ----include = FALSE----------------------------------------------------------
options(tabxplor.print = "console")

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, marital, race)

## ----include = FALSE----------------------------------------------------------
options(tabxplor.print = "html")

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, marital, race, pct = "row")

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, marital, age)

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, c(race, relig), c(party3, tvhours), na = "drop_all", pct = "row")

## ----echo = TRUE--------------------------------------------------------------
counts <- dplyr::count(gss_simple, marital, race) # or a published table
tab_counts(counts, marital, race, counts = n, pct = "row", color = "difference")

## ----echo = FALSE-------------------------------------------------------------
options(tabxplor.cleannames = TRUE)

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, race, party3, pct = "row", color = "difference")

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, rincome, c(party3, marital), pct = "row", color = "auto")

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, rincome, tvhours, color = "difference")

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, relig, year, pct = "col", color = "ratio", ref = 1)

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, rincome, party3, race, na = "drop", pct = "row", 
    color = "auto", comp="all")

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, c(race, relig), party3, pct = "row", color = "difference",
    ref = c(race = 1, relig = "tot"), na = "drop")

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, race, c(party3, marital), pct = "col", color = "difference",
    ref = c("first", "tot"), na = "drop")

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, race, party3, pct = "row", color = "difference", 
    color_signif = "grey_non_signif")

## ----echo = TRUE--------------------------------------------------------------
gss_simple |>
  dplyr::filter(year == "2012") |> # n=1 974
  tab(race, party3, pct = "row", color = "difference", color_signif = "guaranteed_effect")

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, relig, race, pct = "row", n_min = 400)

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, relig, race, pct = "row",  other_if_less_than = 400)

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, race, party3, pct = "row", ci = "cell") # by default, conf_level = 0.95

## ----echo = TRUE--------------------------------------------------------------
gss_simple |>
  dplyr::filter(year == "2012") |> # n=1 974
  tab(race, party3, pct = "row", 
      color = "difference", ref = 1, color_signif = "guaranteed_effect",
      display = "base_ci" # "{base} {ci}"
  )

## ----echo = TRUE--------------------------------------------------------------
gss_simple |>
  dplyr::filter(year == "2012") |> # n=1 974
  tab(rincome, party3, pct = "row", ref = 1, display = "ci", stars = TRUE)

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, race, c(party3, tvhours), pct = "row", test = TRUE)

## ----echo = TRUE--------------------------------------------------------------
tea_when_vars <- c("breakfast", "lunch", "tea.time", "evening", "dinner", "always")
# levels(facto_tea$breakfast)   # always check: the "yes" answer must come first

## ----echo = TRUE--------------------------------------------------------------
tea <- facto_tea |> score_from_lv1("tea_when", vars_list = tea_when_vars) # score variable
tab(tea, SPC, all_of(c(tea_when_vars, "tea_when")), pct = "row", 
    levels = "first", na = "drop", color = "difference")

## ----echo = TRUE--------------------------------------------------------------
tab(tea, sex, c(breakfast, evening, SPC), pct = "row", 
    levels = "auto", na = "drop", tot = "row")

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, race, party3, rincome, na = "drop", pct = "row")

## ----echo = TRUE, eval = FALSE------------------------------------------------
# tab(gss_simple, c(married, income25k), race, pct = "row", output_list = TRUE)

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, rincome, c(married, tvhours), tab_vars = race, spread_vars = race,
    pct = "row", na = "drop", levels = "first", comp = "all",
    color = "auto", color_signif = "grey_non_signif")

## ----echo = TRUE--------------------------------------------------------------
gss_w <- dplyr::mutate(gss_simple, w = ifelse(marital %in% "Never married", 2.5, 0.8))
tab(gss_w, race, party3, wt = w, pct = "row", na = "drop")

## ----echo = TRUE, eval = FALSE------------------------------------------------
# tabs <- tab(gss_simple, race, party3, pct = "row", color = "difference")
# tab_export(tabs) # default : html table (RStudio Viewer, .Rmd/.qmd, etc.)
# tab_export(tabs, format = "xl", path = "table") # Excel export
# tab_export(tabs, format = "md", path = "table") # flat markdown file

## ----echo = TRUE, eval = FALSE------------------------------------------------
# tab_export(tabs, theme = "auto") # HTML that follows the reader's light/dark modes

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, party3, c(race, tvhours), pct = "row",
    color = "ratio", display = "base", n = "min") |>
  tab_html(transpose = TRUE)

## ----echo = TRUE, eval = FALSE------------------------------------------------
# options(tabxplor.tab_kable_css = FALSE)
# tab_css(theme = "auto")   # emit once, near the top of the document

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, race, party3, pct = "row", color = "difference") |>
  tab_html(theme = "print_ready")

## ----echo = TRUE, eval = FALSE------------------------------------------------
# options(tabxplor.theme = "print_ready")

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, race, c(party3, tvhours), pct = "row", display = "base_moe")

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, race, party3, pct = "row", color = "difference", display = "{pct} ({diff})")

## ----echo = TRUE--------------------------------------------------------------
tabs <- tab(gss_simple, race, party3, pct = "row")
set_display(tabs, "{pct} (n={n})")

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, race, party3, color = "contrib")
# tab(gss_simple, race, party3, pct = "all", color = "contrib")  # works with pct too

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, race, party3, color = "contrib") |> set_display("ctr")

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, race, party3, color = "contrib", color_signif = "guaranteed_effect") |>
  set_display("resid")

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, race, party3, pct = "row", color = "contrib",
    display = "{pct} ({resid})")

## ----echo = TRUE, eval = FALSE------------------------------------------------
# tab(gss_simple, race, party3, pct = "row", color = "difference")

## ----echo = FALSE, eval = TRUE------------------------------------------------
tab(gss_simple, race, party3, pct = "row", color = "difference") |>
  tab_html(tooltips = TRUE)

## ----echo = TRUE, fig.width = 8, fig.height = 4, eval = requireNamespace("ggplot2", quietly = TRUE)----
tab(tea, SPC, c(breakfast, lunch, evening, dinner), pct = "row",
    levels = "first", na = "drop",
    color = "ratio", color_signif = "guaranteed_effect", ref = 1) |>
  forest_plot()

## ----echo = TRUE, message = FALSE---------------------------------------------
library(dplyr)
tab(gss_simple, race, marital, pct = "row") |>
  arrange(desc(Married))

## ----echo = TRUE--------------------------------------------------------------
tab(gss_simple, race, marital, pct = "row",
    subtext = c("Population: ", "Source: GSS, 2000-2014")) |>
  set_caption("Custom title")

