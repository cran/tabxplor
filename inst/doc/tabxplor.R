## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tabxplor)
set_color_style(type = "text", theme = "light")

## ---- echo = FALSE, include = FALSE-------------------------------------------
options(crayon.enabled = TRUE)
knitr::knit_hooks$set(output = function(x, options){
  paste0(
    '<pre class="r-output"><code>',
    fansi::sgr_to_html(x = htmltools::htmlEscape(x), warn = FALSE),
    '</code></pre>'
  )
})

num_colors <- function(forget = TRUE) 256
library(crayon)
assignInNamespace("num_colors", num_colors, pos = "package:crayon")

## ---- echo = TRUE-------------------------------------------------------------
tab(forcats::gss_cat, marital, race)

## ---- echo = TRUE-------------------------------------------------------------
data <- forcats::gss_cat %>% 
  dplyr::filter(year %in% c(2000, 2006, 2012), !marital %in% c("No answer", "Widowed"))
gss  <- "Source: General social survey 2000-2014"
gss2 <- "Source: General social survey 2000, 2006 and 2012"
tab(data, race, marital, year, subtext = gss2, pct = "row", color = "diff")

## ---- echo = TRUE-------------------------------------------------------------
tab(dplyr::storms, category, status, sup_cols = c("pressure", "wind"))

## ---- echo = TRUE-------------------------------------------------------------
tab(data, race, marital, year, subtext = gss2, pct = "row", color = "diff", comp = "all")

## ---- echo = TRUE-------------------------------------------------------------
data <- data %>% dplyr::mutate(year = as.factor(year))
tab(data, year, marital, race, pct = "row", color = "diff", diff = "first", tot = "col",
    totaltab = "table")

## ---- echo = TRUE-------------------------------------------------------------
tab(forcats::gss_cat, race, marital, pct = "row", ci = "cell")

## ---- echo = TRUE-------------------------------------------------------------
tab(forcats::gss_cat, race, marital, pct = "row", color = "diff_ci")

## ---- echo = TRUE-------------------------------------------------------------
tab(forcats::gss_cat, race, marital, subtext = gss, pct = "row", color = "after_ci")

## ---- echo = TRUE-------------------------------------------------------------
tab(forcats::gss_cat, race, marital, chi2 = TRUE)

## ---- echo = TRUE-------------------------------------------------------------
tab(forcats::gss_cat, race, marital, color = "contrib")

## ---- echo = TRUE-------------------------------------------------------------
library(dplyr)
first_lvs <- c("Married", "$25000 or more", "Strong republican", "Protestant")
data <- forcats::gss_cat %>% mutate(across(
  where(is.factor),
  ~ forcats::fct_relevel(., first_lvs[first_lvs %in% levels(.)])
))
tabs <- tab_many(data, race, c(marital, rincome, partyid, relig, age, tvhours),
         levels = "first", pct = "row", chi2 = TRUE, color = "auto")
tabs

## ---- echo = TRUE-------------------------------------------------------------
tabs %>% tab_kable()


