% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{cfa_dat2}
\alias{cfa_dat2}
\title{Sample Data: A CFA Model with an Influential Case}
\format{
A data frame with 100 rows
and 7 variables:
\describe{
\item{case_id}{Case ID. Character.}
\item{x1}{Indicator. Numeric.}
\item{x2}{Indicator. Numeric.}
\item{x3}{Indicator. Numeric.}
\item{x4}{Indicator. Numeric.}
\item{x5}{Indicator. Numeric.}
\item{x6}{Indicator. Numeric.}
}
}
\usage{
cfa_dat2
}
\description{
A six-variable dataset with 100 cases, with
one influential case.
}
\examples{
library(lavaan)
data(cfa_dat2)
mod <-
"
f1 =~  x1 + x2 + x3
f2 =~  x4 + x5 + x6
"
fit <- cfa(mod, cfa_dat2)
summary(fit)
inf_out <- influence_stat(fit)
gcd_plot(inf_out)
}
\keyword{datasets}
