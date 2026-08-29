## ----include = FALSE----------------------------------------------------------
# Messages and warnings are off for every chunk: the teaching notes tabxplor prints (an
# auto-detected family, an over-dispersion caveat) are explained in the prose where they
# matter, and repeated under every table they only clutter it. Re-enable one with
# `message = TRUE` on the chunk that needs it.
knitr::opts_chunk$set(collapse = TRUE, comment = "#>",
                      message = FALSE, warning = FALSE)

# Pin the legend language: it defaults to "auto" = the ambient locale, so building this English
# article on a French machine silently renders French legends and captions. Output must not
# depend on where it is built.
options(tabxplor.lang = "en")
Sys.setenv(LANGUAGE = "en")   # the model-fit row labels go through gettext, not this option

# The shape table a continuous predictor draws under the footer is only this article's subject in
# one section, which switches it on and off again around itself.
options(tabxplor.shape_table = "no")

## ----setup, message=FALSE-----------------------------------------------------
library(tabxplor)
library(dplyr)

# html tables in RStudio/Positron Viewer (recommended)
options(tabxplor.print = "html")

## ----setup-hidden, include=FALSE, messages=FALSE------------------------------
# The shared stylesheet is emitted once by tab_css() below, tooltips kept off to keep the page light.

if (!interactive()) {
options(tabxplor.tab_kable_css = FALSE)
options(cli.num_colors = 256)
options(tabxplor.tab_kable_tooltips = FALSE)
options(tabxplor.shape_table = "no")
}

# set_color_palette(theme = "light")

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
ucb <- as.data.frame(UCBAdmissions) |>                    # base R, no package needed
  tidyr::uncount(Freq) |> tibble::as_tibble() |>
  dplyr::mutate(Admit  = factor(Admit,  levels = c("Admitted", "Rejected")),
         Gender = factor(Gender, levels = c("Male", "Female")))

## -----------------------------------------------------------------------------
tab(car_arrests, colour, released, pct = "row", ref = "first",
    color = "difference", color_signif = "grey_non_signif")

## -----------------------------------------------------------------------------
tab(car_arrests, c(colour, sex, employed, citizen), released, pct = "row",
    color = "difference", color_signif = "grey_non_signif", ref = "first")

## -----------------------------------------------------------------------------
car_arrests |>
  tab(colour, released, pct = "row", ref = "first", stars = TRUE,
      color = "difference", color_signif = "grey_non_signif") |>
  mutate(difference = set_display(Yes, "difference"),
         ratio      = set_display(Yes, "ratio"),
         odds_ratio = set_display(Yes, "odds_ratio") )

## -----------------------------------------------------------------------------
tab(car_arrests, colour, released, pct = "row", ref = "first", ci = "ref",
    color = "difference", color_signif = "grey_non_signif")

## ----eval = FALSE-------------------------------------------------------------
# tab_reg(car_arrests, "checks", c("colour", "employed", "citizen"), family = "gaussian")
# tab_reg(car_arrests, "checks", c("colour", "employed", "citizen"), family = "poisson" )
# tab_reg(car_arrests, "checks", c("colour", "employed", "citizen"), family = "binomial", trials = 6)

## ----message=FALSE------------------------------------------------------------
car_arrests |>
  mutate(checks_gaussian = checks, checks_poisson = checks, checks_binomial = checks) |>
  tab_reg(c("checks_gaussian", "checks_poisson", "checks_binomial"),
          family = c("gaussian", "poisson", "binomial"), trials = c(checks_binomial = 6),
          predictors = c("colour", "employed", "citizen"),
          stats = NULL, empirical = FALSE
         )

## -----------------------------------------------------------------------------
reg_measures(car_arrests, "checks")

## -----------------------------------------------------------------------------
model <- tab_reg(car_arrests, "released", c("colour", "sex", "employed", "citizen", "checks"))
model

## -----------------------------------------------------------------------------
tab(car_arrests, c(colour, sex, employed, citizen), released, pct = "row", ref = "first",
    stars = TRUE, color = "odds_ratio", color_signif = "grey_non_signif", display = "OR")

## -----------------------------------------------------------------------------
model

## ----include=FALSE------------------------------------------------------------
options(tabxplor.shape_table = "all")

## -----------------------------------------------------------------------------
tab_reg(car_arrests, "released", c("colour", "checks"), stats = NULL)

## -----------------------------------------------------------------------------
tab_reg(car_arrests, "released", c("colour", "checks"),
        ref = c(checks = 0), multiplier = c(checks = 1),
        empirical = FALSE, stats = NULL)

## -----------------------------------------------------------------------------
tab_reg(car_arrests, "released", c("colour", "checks"), shape = c(checks = "quartiles"), stats = NULL)

## ----include=FALSE------------------------------------------------------------
options(tabxplor.shape_table = "no")

## -----------------------------------------------------------------------------
tab_reg(car_arrests, "released", c("colour", "checks"),
        ref = c(checks = 0), multiplier = c(checks = 1), stats = NULL)

## -----------------------------------------------------------------------------
tab_reg(car_arrests, "released", c("colour", "sex", "employed", "citizen"), empirical = FALSE, stats = NULL)

## -----------------------------------------------------------------------------
tab_reg(car_arrests, "released", c("colour", "sex", "employed", "citizen", "checks"), stats=NULL)

## -----------------------------------------------------------------------------
tab_reg(car_arrests, "released", c("colour", "sex", "employed", "citizen", "checks"),
        measure = "ratio", stats=NULL)

## -----------------------------------------------------------------------------
tab_reg(car_arrests, "released", c("colour", "sex", "employed", "citizen", "checks"),
        measure = "ratio", stats=NULL, outcome_level = "No")

## -----------------------------------------------------------------------------
tab_reg(car_arrests, "released", c("colour", "sex", "employed", "citizen", "checks"),
        measure = "difference", stats=NULL)

## ----eval=FALSE, include=FALSE------------------------------------------------
# tab_reg(car_arrests, "released", c("colour", "sex", "employed", "citizen", "checks"),
#         measure = "difference", display = "base", stats=NULL)

## ----eval=FALSE, include=FALSE------------------------------------------------
# tab_reg(car_arrests, "released", c("colour", "sex", "employed", "citizen", "checks"),
#         link = "ratio", stats=NULL)

## ----eval=FALSE, include=FALSE------------------------------------------------
# reg_formulas(tab_reg(car_arrests, "released", c("colour", "checks"), measure = "ratio", empirical = FALSE))
# reg_formulas(tab_reg(car_arrests, "released", c("colour", "checks"), link    = "ratio", empirical = FALSE))

## -----------------------------------------------------------------------------
tab_reg(car_arrests, "released", c("colour", "sex", "employed", "citizen", "checks"),
        measure = "difference", stats = NULL)

## -----------------------------------------------------------------------------
model <-
tab_reg(car_arrests, "released",
        list("colour"         = "colour",
             "+ who they are" = c("colour", "sex", "citizen", "employed"),
             "+ prior record" = c("colour", "sex", "citizen", "employed", "checks")),
        measure = "difference", display = "est", stats = NULL)
model

## -----------------------------------------------------------------------------
tab_reg(car_arrests, "released",
        list("colour"         = "colour",
             "+ who they are" = c("colour", "sex", "citizen", "employed"),
             "+ prior record" = c("colour", "sex", "citizen", "employed", "checks")),
        measure = "difference", color = "adjustment", color_signif = "guaranteed_effect",
        stats = NULL)

## ----echo=FALSE, eval=FALSE---------------------------------------------------
# tab_reg(car_arrests, "released",
#         list("colour"         = "colour",
#              "+ who they are" = c("colour", "sex", "citizen", "employed"),
#              "+ prior record" = c("colour", "sex", "citizen", "employed", "checks")),
#         measure = "difference", color = c("measure", "adjustment"), stats = NULL)

## -----------------------------------------------------------------------------
tab_reg(car_arrests, "released",
        list("without prior record" = c("colour", "employed", "citizen"),
             "with prior record"    = c("colour", "employed", "citizen", "checks") ),
        measure = "difference", display="est")

## -----------------------------------------------------------------------------
tab(car_arrests, colour, checks, pct = "row", color = "difference", ref = 1)

## -----------------------------------------------------------------------------
tab_reg(car_arrests, "checks", c("colour", "sex", "employed", "citizen"),
        family = "binomial", trials = 6)

## -----------------------------------------------------------------------------
tab_reg(car_salaries, "salary",
        list("sex alone"      = "sex",
             "+ field, years" = c("sex", "discipline", "yrs.service"),
             "+ rank"         = c("sex", "discipline", "yrs.service", "rank")),
        family = "gaussian", empirical = FALSE)

## -----------------------------------------------------------------------------
tab(car_salaries, sex, is_prof, pct = "row", color = "difference", ref = 1, color_signif = "grey_non_signif")

## -----------------------------------------------------------------------------
tab_reg(car_salaries, "salary", c("sex", "discipline", "yrs.service", "rank"),
        family = "gaussian", display = "est_ci", empirical = FALSE)

## -----------------------------------------------------------------------------
tab(ucb, Gender, Admit, pct = "row", color = "difference", color_signif = "grey_non_signif")

## -----------------------------------------------------------------------------
tab_reg(ucb, "Admit", c("Gender", "Dept"), measure = "difference")

## -----------------------------------------------------------------------------
tab(ucb, Gender, Dept, pct = "row", color = "difference", ref = 1)

## -----------------------------------------------------------------------------
tab_reg(questionr_hdv, "cinema", c("qualif", "age"))

## -----------------------------------------------------------------------------
tab(questionr_hdv, qualif, c(cinema, age), pct = "row", na = "drop_all",
    color = "difference", ref = 1)

## -----------------------------------------------------------------------------
tab_reg(questionr_hdv, "cinema", c("qualif", "age"), measure = "difference")

