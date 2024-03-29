% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab.R
\name{tab_pct}
\alias{tab_pct}
\title{Add percentages and diffs to a \code{\link[tabxplor]{tab}}}
\usage{
tab_pct(
  tabs,
  pct = "row",
  digits = NULL,
  diff = c("tot", "first", "no"),
  comp = NULL,
  color = FALSE,
  just_diff = FALSE
)
}
\arguments{
\item{tabs}{A \code{tibble} of class \code{tab} made with \code{\link{tab_plain}} or
\code{\link{tab_many}}.}

\item{pct}{The type of percentages to calculate. \code{"row"} draw row percentages.
Set to \code{"col"} for column percentages. Set to \code{"all"} for frequencies
(based on each subtable/group if \code{tab_vars} is provided).
Set to \code{"all_tabs"} to calculate frequencies based on the whole (set of) table(s).}

\item{digits}{The number of digits to print for percentages. As a single integer,
or an integer vector the same length than \code{col_vars}.}

\item{diff}{By default, with \code{pct = "row"}, differences from total rows are
calculated, and with \code{pct = "col"} differences from total columns.
Set to \code{diff = "first"} to calculate differences with the first cell
of the row/col (useful to color temporal developments). When not using diffs for colors,
set to \code{diff = "no"} to gain calculation time. Diffs are also calculated for
mean columns (made from numeric variables).}

\item{comp}{Comparison level. When \code{tab_vars} are present, should the row
differences be calculated for each subtable/group (by default \code{comp = "tab"} :
comparison of each cell to the relative total row) ?
Should they be calculated for the whole table (\code{comp = "all"} :
comparison of each cell to the total row of the total table) ?
When \code{comp = "all"} and \code{diff = "first"}, cells are compared to the first
cell of the total table instead.
This parameter doesn't affect column percentages.
\code{comp} must be set once and for all the first time you use \code{\link{tab_chi2}},
\code{\link{tab_pct}} with rows, or \code{\link{tab_ci}}.}

\item{color}{Set to \code{TRUE} to color the resulting tab based on differences (from
totals or from the first cell).}

\item{just_diff}{If percentages are already calculated and you just want
to recalculate differences.}
}
\value{
A \code{tibble} of class \code{tab}, with percentages displayed, possibly
colored based on differences from totals or first cell.
}
\description{
Add percentages and diffs to a \code{\link[tabxplor]{tab}}
}
