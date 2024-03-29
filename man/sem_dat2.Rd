% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{sem_dat2}
\alias{sem_dat2}
\title{Sample Data: A Latent Variable Structural Model
With an Influential Case}
\format{
A data frame with 200 rows
and 10 variables:
\describe{
\item{case_id}{Case ID. Character.}
\item{x1}{Indicator. Numeric.}
\item{x2}{Indicator. Numeric.}
\item{x3}{Indicator. Numeric.}
\item{x4}{Indicator. Numeric.}
\item{x5}{Indicator. Numeric.}
\item{x6}{Indicator. Numeric.}
\item{x7}{Indicator. Numeric.}
\item{x8}{Indicator. Numeric.}
\item{x9}{Indicator. Numeric.}
}
}
\usage{
sem_dat2
}
\description{
A ten-variable dataset with 200 cases, with
one influential case.
}
\examples{
library(lavaan)
data(sem_dat2)
mod <-
"
f1 =~  x1 + x2 + x3
f2 =~  x4 + x5 + x6
f3 =~  x7 + x8 + x9
f2 ~ a * f1
f3 ~ b * f2
ab := a * b
"
fit <- sem(mod, sem_dat2)
summary(fit)
inf_out <- influence_stat(fit)
gcd_plot(inf_out)
}
\keyword{datasets}
