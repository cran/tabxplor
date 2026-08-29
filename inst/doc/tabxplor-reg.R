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

# As in the introduction vignette, tables render as tabxplor's real html tables (the recommended
# everyday setting); the shared stylesheet is emitted once by tab_css() below, tooltips kept off.
options(tabxplor.print = "html")
options(tabxplor.tab_kable_css = FALSE)
options(tabxplor.tab_kable_tooltips = FALSE)

# The shape table a continuous predictor draws under the footer is the subject of one
# section below, and noise everywhere else: it is switched on there and off here.
options(tabxplor.shape_table = "no")

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

## -----------------------------------------------------------------------------
tea_where_vars <- c("home", "work", "tearoom", "friends", "resto", "pub")
tea <- facto_tea |> score_from_lv1("tea_where", vars_list = tea_where_vars)

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "rincome", c("race", "relig"),
        empirical = TRUE, color = c(TRUE, "adjustment"))

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "rincome", c("race", "relig"), measure = "difference",
        empirical = TRUE, display = "est_base", color = c(TRUE, "adjustment"))

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "party3", c("race", "relig"), family = "multinomial",
        empirical = TRUE, color = c(TRUE, "adjustment"))

## -----------------------------------------------------------------------------
tab_reg(tea, "tea_where", c("sex", "SPC", "Sport"),
        family = "binomial", trials = length(tea_where_vars),
        empirical = TRUE, color = c(TRUE, "adjustment"))

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "party3", c("race", "relig"), family = "multinomial",
        measure = "difference", empirical = TRUE, color = c(TRUE, "adjustment"),
        color_signif = "grey_non_signif")

## ----eval = FALSE-------------------------------------------------------------
# tab_reg(gss_simple, "married", c("race", "age"), effect = "at_reference")

## -----------------------------------------------------------------------------
reg_measures(gss_simple, "married")

## -----------------------------------------------------------------------------
reg_measures(gss_simple, "married", link = "all")

## -----------------------------------------------------------------------------
reg_measures(gss_simple, "married", link = "ratio")

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"))

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"), empirical = TRUE)

## -----------------------------------------------------------------------------
gss_simple |> 
  dplyr::filter(dplyr::if_all(all_of(c("race", "age", "rincome", "relig")), ~ !is.na(.) )) |> 
  tab(race, married, pct = "row", na = "drop", 
    display = "{or}", ref = "first", color = "odds_ratio", color_signif = "grey_non_signif"
   )

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"),
        measure = "difference", empirical = TRUE)

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"),
        measure = "ratio", empirical = TRUE)

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"),
        measure = "odds_ratio", effect = "marginal", empirical = TRUE)

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"),
        link = "ratio", empirical = TRUE)

## -----------------------------------------------------------------------------
reg_formulas(tab_reg(gss_simple, "married", c("race", "age"), measure = "ratio"))
reg_formulas(tab_reg(gss_simple, "married", c("race", "age"), link    = "ratio"))

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "age", c("race", "marital", "relig", "rincome"), family = "gaussian")

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "age", c("race", "marital", "relig", "rincome"), family = "gaussian", empirical = TRUE)

## -----------------------------------------------------------------------------
tab(gss_simple, "race", "age", pct = "row", digits = 2, na = "drop",
    color = "difference", ref = 1,  ci_method = c(mean_diff = "ols")
) |> 
  mutate(diff = set_display(age, "diff"))
# ols : the variance pooled over every level of the variable, so the intervals are exactly those a
#  linear regression puts on its coefficients ("student" pools the two compared groups only).

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "tvhours", c("race", "marital", "relig", "rincome"), family = "poisson")

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "tvhours", c("race", "marital", "relig", "rincome"), family = "poisson", empirical = TRUE)

## -----------------------------------------------------------------------------
tab(gss_simple, "race", "tvhours", pct = "row", digits = 2, na = "drop",
    color = "ratio", ref = 1,  ci_method = c(mean_ratio = "quasipoisson")
) |> 
  mutate(IRR = set_display(tvhours, "ratio"))
# the default method is a robust ratio of means (each group's own variance) ;
#  we use "quasipoisson" to match those computed by quasi-poisson regression -- one dispersion
#  estimated over every level (assumption : variance is proportional to mean). 

## -----------------------------------------------------------------------------
tab_reg(tea, "tea_where", c("sex", "SPC", "Sport"),
        family = "binomial", trials = length(tea_where_vars))

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "rincome", c("race", "age", "relig"))

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "rincome", c("race", "age", "relig"),
        measure = "difference", display = "est_base")

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "party3", c("race", "age", "rincome", "relig"))

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "party3", c("race", "age", "rincome", "relig"), measure = "difference", empirical = TRUE) # |> tab_export()

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married", c("race", "rincome", "relig"),
        empirical = TRUE, color = c(TRUE, "adjustment"))

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married", c("race", "rincome"), empirical = TRUE) |>
  set_display("{est} (obs {obs})")

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married", c("race", "rincome", "relig"),
        measure = "ratio", empirical = TRUE, color = c(TRUE, "adjustment"))

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married", c("race", "rincome", "relig"),
        link = "ratio", empirical = TRUE,
        color = c(TRUE, "adjustment"), color_signif = "grey_non_signif")

## -----------------------------------------------------------------------------
set.seed(1)
small <- gss_simple[sample(nrow(gss_simple), 2000), ]
tab_reg(small, "married", c("race", "rincome", "relig"),
        link = "ratio", empirical = TRUE,
        color = c(TRUE, "adjustment"), color_signif = "grey_non_signif")

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married", c("race", "age"), display = "est_ci")

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married", c("race", "age"), display = "est_base")

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married", c("race", "age"))

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married", c("race", "age"), multiplier = c(age = 10))

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married", c("race", "age"), ref = c(race = "Black", age = 40))

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married", c(race*party3, relig), empirical = TRUE)

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married",
        list(additive = c(race, party3), crossed = c(race*party3)),
        stats = "compare_sequential")

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married", c(age*race, relig), empirical = TRUE)

## ----eval = FALSE-------------------------------------------------------------
# tab_reg(gss_simple, "married", c(age*race, relig), shape = c(age = "quartiles"))

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married", c(age*tvhours, race), empirical = TRUE)

## -----------------------------------------------------------------------------
tab_reg(gss_simple,
        "married",
        list("Race only"    = "race",
             "+ age"        = c("race", "age"),
             "+ party"      = c("race", "age", "party3")),
        stats = "compare_sequential")

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married", c("race", "rincome"), tab_vars = "year")

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married", c("race", "rincome"), tab_vars = "party3",
        color = c(TRUE, "between_groups"), color_signif = "grey_non_signif")

## ----eval = FALSE-------------------------------------------------------------
# tab_reg(data, "outcome", c("pred1", "pred2"), wt = "weight")
# 
# library(survey)
# d <- svydesign(ids = ~psu, strata = ~stratum, weights = ~w, data = my_survey, nest = TRUE)
# tab_reg(d, "outcome", c("pred1", "pred2"), empirical = TRUE)

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married", c("race", "age", "rincome", "relig"), stats = c("n", "linearity", "dispersion", "influence", "collinearity"))

## ----include = FALSE----------------------------------------------------------
options(tabxplor.shape_table = "all")   # this section is about it

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married", c("race", "age", "tvhours"), family = "binomial")

## -----------------------------------------------------------------------------
set.seed(20260823)
small <- gss_simple[sample(nrow(gss_simple), 200), ]
tab_reg(small, "married", c("race", "age", "tvhours"), family = "binomial")

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married", c("race", "age"), family = "binomial",
        shape = c(age = "quintiles"), empirical = TRUE)

## -----------------------------------------------------------------------------
tab_reg(gss_simple, "married", c("race", "age"), family = "binomial",
        shape = c(age = "quadratic"))

## ----eval = FALSE-------------------------------------------------------------
# reg_check_plots(t)                       # the default panels, one titled grid per model
# reg_check_plots(t, check = "all")        # plus dispersion and collinearity
# reg_check_plots(t, check = "linearity")  # just one

## ----include = FALSE----------------------------------------------------------
options(tabxplor.shape_table = "no")    # back to the vignette's own setting

## ----eval = FALSE-------------------------------------------------------------
# t <- tab_reg(gss_simple, "married", c("race", "rincome"), family = "binomial")
# forest_plot(t)

## ----eval = FALSE-------------------------------------------------------------
# t <- tab_reg(gss_simple, "married", c("race", "rincome"),
#              link = "ratio", empirical = TRUE)          # risk ratios
# forest_plot(t)

