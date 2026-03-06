# Compatibility Check for the '\_approx' Functions

Gets a 'lavaan' output and checks whether it is supported by the
functions using the approximate approach.

## Usage

``` r
approx_check(
  fit,
  print_messages = TRUE,
  multiple_group = TRUE,
  equality_constraint = TRUE
)
```

## Arguments

- fit:

  The output from `lavaan`, such as
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) and
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html).

- print_messages:

  Logical. If `TRUE`, will print messages about the check. If `FALSE`,
  the messages will be attached to the return value as an attribute.
  Default is `TRUE`.

- multiple_group:

  Logical. Whether multiple-group models are supported. If yes, the
  check for multiple-groups models will be skipped. Default is `TRUE`.

- equality_constraint:

  Logical. Whether models with equality constraints are supported. If
  yes, the check for equality constraints will be skipped. Default is
  `TRUE`.

## Value

A single-element vector. If confirmed to be supported, will return 0. If
not confirmed to be support but may still work, return 1. If confirmed
to be not yet supported, will return a negative number, the value of
this number without the negative sign is the number of tests failed.

## Details

This function is not supposed to be used by users. It is called by
functions such as
[`est_change_approx()`](https://sfcheung.github.io/semfindr/reference/est_change_approx.md)
to see if the analysis passed to it is supported. If not, messages will
be printed to indicate why.

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>.

## Examples

``` r
dat <- cfa_dat

mod <-
"
f1 =~ x4 + x5 + x6
"
dat_gp <- dat
dat$gp <- rep(c("gp1", "gp2"), length.out = nrow(dat_gp))

fit01 <- lavaan::sem(mod, dat)
# If supported, returns a zero
approx_check(fit01)
#> [1] 0

fit05 <- lavaan::cfa(mod, dat, group = "gp")
# If not supported, returns a negative number
approx_check(fit05)
#> [1] 0
```
