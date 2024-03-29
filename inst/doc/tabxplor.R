## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tabxplor)
set_color_style(type = "text", theme = "light")

## ----echo = FALSE, include = FALSE--------------------------------------------
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

## ----echo = TRUE--------------------------------------------------------------
tab(forcats::gss_cat, marital, race)

## ----include = FALSE----------------------------------------------------------
options(tabxplor.output_kable = TRUE)

## ----echo = TRUE--------------------------------------------------------------
data <- forcats::gss_cat %>% 
  dplyr::filter(year %in% c(2000, 2006, 2012), !marital %in% c("No answer", "Widowed"))
gss  <- "Source: General social survey 2000-2014"
gss2 <- "Source: General social survey 2000, 2006 and 2012"
tab(data, race, marital, year, subtext = gss2, pct = "row", color = "diff")

## ----echo = TRUE--------------------------------------------------------------
tab(dplyr::storms, category, status, sup_cols = c("pressure", "wind"))

## ----echo = TRUE--------------------------------------------------------------
tab(data, race, marital, year, subtext = gss2, pct = "row", color = "diff", comp = "all")

## ----echo = TRUE--------------------------------------------------------------
data <- data %>% dplyr::mutate(year = as.factor(year))
tab(data, year, marital, race, pct = "row", color = "diff", diff = "first", tot = "col",
    totaltab = "table")

## ----echo = TRUE--------------------------------------------------------------
tab(data, year, marital, race, pct = "row", color = "diff", diff = 3)

## ----echo = TRUE--------------------------------------------------------------
tab(data, year, marital, race, pct = "col", tot = "row", color = "diff", diff = "Married")

## ----echo = TRUE--------------------------------------------------------------
tab(forcats::gss_cat, race, marital, pct = "row", ci = "cell")

## ----echo = TRUE--------------------------------------------------------------
tab(forcats::gss_cat, race, marital, pct = "row", color = "diff_ci")

## ----echo = TRUE--------------------------------------------------------------
tab(forcats::gss_cat, race, marital, subtext = gss, pct = "row", color = "after_ci")

## ----echo = TRUE--------------------------------------------------------------
tab(forcats::gss_cat, race, marital, chi2 = TRUE)

## ----echo = TRUE--------------------------------------------------------------
tab(forcats::gss_cat, race, marital, color = "contrib")

## ----include = FALSE----------------------------------------------------------
options(tabxplor.output_kable = FALSE)

## ----echo = TRUE, message = FALSE---------------------------------------------
library(dplyr)
first_lvs <- c("Married", "$25000 or more", "Strong republican", "Protestant")
data <- forcats::gss_cat %>% mutate(across(
  where(is.factor),
  ~ forcats::fct_relevel(., first_lvs[first_lvs %in% levels(.)])
))
tabs <- tab_many(data, race, c(marital, rincome, partyid, relig, age, tvhours),
         levels = "first", pct = "row", chi2 = TRUE, color = "auto")
tabs

## ----echo = TRUE--------------------------------------------------------------
tabs %>% tab_kable()

## -----------------------------------------------------------------------------
tabs <- tab(data, race, marital, year, pct = "row")
tabs %>% mutate(across(where(is_fmt), get_num))

## -----------------------------------------------------------------------------
vctrs::vec_data(tabs$Married)

## ----echo = TRUE, message = FALSE---------------------------------------------
tab_num(data, race, c(age, tvhours), marital, digits = 1L, comp = "all") |>
  dplyr::mutate(dplyr::across( #Mutate over the whole table.
    c(age, tvhours),
    ~ dplyr::mutate(., #Mutate over each fmt vector's underlying data.frame.
                    var     = sqrt(var), 
                    display = "var", 
                    digits  = 2L) |> 
      set_color("no"),
    .names = "{.col}_sd"
  ))

## ----echo = TRUE, message = FALSE---------------------------------------------
tab(data, race, marital, year, pct = "row") %>%
  dplyr::mutate(across( 
    where(is_fmt),
    ~ dplyr::if_else(is_totrow(.), 
                true  = mutate(., digits = 1L), 
                false = mutate(., digits = 2L))
  ))

## ----echo = TRUE, message = FALSE---------------------------------------------
tab(data, race, marital, year, pct = "row") %>%
  mutate(across(where(is_totcol), ~ mutate(., display = "n") ))

