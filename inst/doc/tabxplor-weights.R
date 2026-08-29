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
Sys.setenv(LANGUAGE = "en")   # the test-summary / model-fit row labels go through gettext, not this option
library(dplyr)

# Tables render as tabxplor's real html tables (the recommended everyday setting); the shared
# stylesheet is emitted once by tab_css() below, and the hover tooltips are kept off here.
options(tabxplor.print = "html")
options(tabxplor.tab_kable_css = FALSE)
options(tabxplor.tab_kable_tooltips = FALSE)

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

## ----data, include = FALSE----------------------------------------------------
gss_simple <- gss_cat_data_formatting()

## ----echo=TRUE, eval=FALSE----------------------------------------------------
# tab(my_survey, education, job, wt = weight, pct = "row")
# #> Weighted by weight; confidence intervals and tests use the unweighted sample size.

## ----echo=TRUE, eval=FALSE----------------------------------------------------
# tab(my_survey, education, job, wt = weight, pct = "row", design_effect = TRUE)
# options(tabxplor.design_effect = TRUE)   # or once, for the whole session
# #> Weighted by weight; confidence intervals and tests account for the weighting.

## ----echo = TRUE--------------------------------------------------------------
gss_w <- dplyr::mutate(gss_simple, w = ifelse(marital %in% "Never married", 2.5, 0.8))

tab(gss_w, race, party3, wt = w, pct = "row", ci = "cell", na = "drop")

## ----echo = TRUE--------------------------------------------------------------
tab(gss_w, race, party3, wt = w, pct = "row", ci = "cell", na = "drop",
    design_effect = TRUE)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
# library(survey)
# d <- svydesign(ids = ~psu, strata = ~stratum, weights = ~w, data = my_survey, nest = TRUE)
# tab(d, race, marital, pct = "row", color = TRUE, test = TRUE)
# #> Design-based (survey): weighted estimates, intervals and tests account for the sample design.

## ----eval = FALSE-------------------------------------------------------------
# tab_reg(data, "outcome", c("pred1", "pred2"), wt = "weight")
# 
# library(survey)
# d <- svydesign(ids = ~psu, strata = ~stratum, weights = ~w, data = my_survey, nest = TRUE)
# tab_reg(d, "outcome", c("pred1", "pred2"), empirical = TRUE)

