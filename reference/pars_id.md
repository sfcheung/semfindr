# Convert Parameter Syntax to Position or Row Numbers in the Parameter Vector or Table

Converts a vector of lavaan syntax to the ids of parameters in the
vector of free parameters or the row numbers in the parameter table.

## Usage

``` r
pars_id(pars, fit, where = c("coef", "partable"), free_only = TRUE)
```

## Arguments

- pars:

  A character vector of parameters specified in lavaan syntax, e.g.,
  `"y ~ x"` and `f1 =~ x3`. For multisample models, if only the
  parameters in some groups are needed, use the modifier for labeling
  parameters and use `NA` to denote parameters to be requested. E.g.,
  `f1 =~ c(NA, 0, NA, NA) * x2` denotes the loadings of `x2` on `f1` in
  the first, third, and fourth groups.

- fit:

  A `lavaan`-class object. This object is used to determine the number
  of groups and the parameters in the model. Only parameters in `pars`
  that appear in this model will be considered.

- where:

  Where the values are to be found. Can be "partable" (parameter table)
  or "coef" (coefficient vector). Default is "coef".

- free_only:

  Whether only free parameters will be kept. Default is `TRUE`.

## Value

A numeric vector of the ids. If `where` is `"partable"`, the ids are row
numbers. If `where` is `"coef"`, the ids are the positions in the
vector.

## Details

It supports the following ways to specify the parameters to be included.

- `lavaan` syntax

  - For example, `"y ~ x"` denotes the regression coefficient regression
    `y` on `x`. It uses
    [`lavaan::lavaanify()`](https://rdrr.io/pkg/lavaan/man/model.syntax.html)
    to parse the syntax strings.

- Operator

  - For example, `"~"` denotes all regression coefficients.

  - It also supports `:=`, which can be used to select user-defined
    parameters.

- Label

  - For example, `"ab"` denotes all parameters with this labels defined
    in model syntax. It can be used to select user-defined parameters,
    such as `"ab := a*b"`.

It is used by functions such as
[`est_change()`](https://sfcheung.github.io/semfindr/reference/est_change.md).

### Multisample model

If a model has more than one group, a specification specified as in a
single sample model denotes the same parameters in all group.

- For example, `"f1 =~ x2"` denotes the factor loading of `x2` on `f1`
  in all groups. `"~~"` denotes covariances and error covariances in all
  groups.

There are two ways to select parameters only in selected groups. First,
the syntax to fix parameter values can be used, with `NA` denoting
parameters to be selected.

- For example, `"f2 =~ c(NA, 1, NA) * x5"` selects the factor loadings
  of `x5` on `f2` in the first and third groups.

Users can also add ".grouplabel" to a specification, `grouplabel` being
the group label of a group (the one appears in
[`summary()`](https://rdrr.io/r/base/summary.html), not the one of the
form `".g2"`, `"g3"`, etc.).

- For example, `"f2 =~ x5.Alpha"` denotes the factor loading of `x5` on
  `f2` in the group `"Alpha"`.

- This method can be used for operators. For example, `"=~.Alpha"`
  denotes all factors loadings in the group `"Alpha"`.

Though not recommended, users can use labels such as `".g2"` and `".g3"`
to denote the parameter in a specific group. These are the labels appear
in the output of some functions of `lavaan`. Although `lavaan` does not
label the parameters in the first group by `".g1"`, this can still be
used in `pars_id()`.

- For example, `"f2 =~ x5.g2"` denotes the factor loading of `x5` on
  `f2` in the second group. `"y ~ x.g1"` denotes the regression
  coefficient from `x` to `y` in the first group.

- This method can also be used for operators. For example, `"=~.g2"`
  denotes all factors loadings in the second group.

However, this method is not as reliable as using `grouplabel` because
the numbering of groups depends on the order they appear in the data
set.

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r

dat <- sem_dat

library(lavaan)
sem_model <-
"
f1 =~  x1 + x2 + x3
f2 =~  x4 + x5 + x6
f3 =~  x7 + x8 + x9
f2 ~   f1
f3 ~   f2
"

fit_ng <- sem(sem_model, dat)

pars <- c("f1 =~ x2", "f2 =~ x5", "f2 ~ f1")
tmp <- pars_id(pars, fit = fit_ng)
coef(fit_ng)[tmp]
#>    f1=~x2    f2=~x5     f2~f1 
#> 0.5895183 0.7299536 1.1145114 
tmp <- pars_id(pars, fit = fit_ng, where = "partable")
parameterTable(fit_ng)[tmp, ]
#>    id lhs op rhs user block group free ustart exo label plabel start   est
#> 2   2  f1 =~  x2    1     1     1    1     NA   0         .p2. 0.684 0.590
#> 5   5  f2 =~  x5    1     1     1    3     NA   0         .p5. 0.978 0.730
#> 10 10  f2  ~  f1    1     1     1    7     NA   0        .p10. 0.000 1.115
#>       se
#> 2  0.145
#> 5  0.099
#> 10 0.233

# Multiple-group models

dat <- sem_dat
set.seed(64264)
dat$gp <- sample(c("Alpha", "Beta", "Gamma"),
                 nrow(dat),
                 replace = TRUE)

library(lavaan)
sem_model <-
"
f1 =~  x1 + x2 + x3
f2 =~  x4 + x5 + x6
f3 =~  x7 + x8 + x9
f2 ~   f1
f3 ~   f2
"

fit_ng <- sem(sem_model, dat)
fit_gp <- sem(sem_model, dat, group = "gp")

pars <- c("f1 =~ x2", "f2 =~ x5", "f2 ~ f1")
tmp <- pars_id(pars, fit = fit_ng)
coef(fit_ng)[tmp]
#>    f1=~x2    f2=~x5     f2~f1 
#> 0.5895183 0.7299536 1.1145114 
tmp <- pars_id(pars, fit = fit_ng, where = "partable")
parameterTable(fit_ng)[tmp, ]
#>    id lhs op rhs user block group free ustart exo label plabel start   est
#> 2   2  f1 =~  x2    1     1     1    1     NA   0         .p2. 0.684 0.590
#> 5   5  f2 =~  x5    1     1     1    3     NA   0         .p5. 0.978 0.730
#> 10 10  f2  ~  f1    1     1     1    7     NA   0        .p10. 0.000 1.115
#>       se
#> 2  0.145
#> 5  0.099
#> 10 0.233

pars <- c("f1 =~ x2", "f2 =~ c(NA, 1, NA) * x5")
tmp <- pars_id(pars, fit = fit_gp)
coef(fit_gp)[tmp]
#>    f1=~x2    f2=~x5 f1=~x2.g2 f1=~x2.g3 f2=~x5.g3 
#> 0.4503424 0.8397750 0.6869992 1.6257211 0.4857651 
tmp <- pars_id(pars, fit = fit_gp, where = "partable")
parameterTable(fit_gp)[tmp, ]
#>    id lhs op rhs user block group free ustart exo label plabel start   est
#> 2   2  f1 =~  x2    1     1     1    1     NA   0         .p2. 0.453 0.450
#> 5   5  f2 =~  x5    1     1     1    3     NA   0         .p5. 0.578 0.840
#> 37 37  f1 =~  x2    1     2     2   30     NA   0        .p37. 0.538 0.687
#> 72 72  f1 =~  x2    1     3     3   59     NA   0        .p72. 1.703 1.626
#> 75 75  f2 =~  x5    1     3     3   61     NA   0        .p75. 1.210 0.486
#>       se
#> 2  0.158
#> 5  0.137
#> 37 0.218
#> 72 1.114
#> 75 0.155

pars2 <- c("f1 =~ x2", "~~.Beta", "f2 =~ x5.Gamma")
tmp <- pars_id(pars2, fit = fit_gp)
coef(fit_gp)[tmp]
#>     f1=~x2     x1~~x1     x2~~x2     x3~~x3     x4~~x4     x5~~x5     x6~~x6 
#> 0.45034240 1.14251961 0.96797675 0.74202425 0.63334211 0.72245573 0.98337279 
#>     x7~~x7     x8~~x8     x9~~x9     f1~~f1     f2~~f2     f3~~f3  f1=~x2.g2 
#> 1.15545837 1.12337264 1.06251947 1.01524121 0.66420675 0.08588945 0.68699918 
#>  f1=~x2.g3  f2=~x5.g3 
#> 1.62572111 0.48576515 
tmp <- pars_id(pars2, fit = fit_gp, where = "partable")
parameterTable(fit_gp)[tmp, ]
#>    id lhs op rhs user block group free ustart exo label plabel start   est
#> 2   2  f1 =~  x2    1     1     1    1     NA   0         .p2. 0.453 0.450
#> 12 12  x1 ~~  x1    0     1     1    9     NA   0        .p12. 1.079 1.143
#> 13 13  x2 ~~  x2    0     1     1   10     NA   0        .p13. 0.587 0.968
#> 14 14  x3 ~~  x3    0     1     1   11     NA   0        .p14. 0.656 0.742
#> 15 15  x4 ~~  x4    0     1     1   12     NA   0        .p15. 1.122 0.633
#> 16 16  x5 ~~  x5    0     1     1   13     NA   0        .p16. 0.929 0.722
#> 17 17  x6 ~~  x6    0     1     1   14     NA   0        .p17. 0.630 0.983
#> 18 18  x7 ~~  x7    0     1     1   15     NA   0        .p18. 0.639 1.155
#> 19 19  x8 ~~  x8    0     1     1   16     NA   0        .p19. 0.926 1.123
#> 20 20  x9 ~~  x9    0     1     1   17     NA   0        .p20. 1.012 1.063
#> 21 21  f1 ~~  f1    0     1     1   18     NA   0        .p21. 0.050 1.015
#> 22 22  f2 ~~  f2    0     1     1   19     NA   0        .p22. 0.050 0.664
#> 23 23  f3 ~~  f3    0     1     1   20     NA   0        .p23. 0.050 0.086
#> 37 37  f1 =~  x2    1     2     2   30     NA   0        .p37. 0.538 0.687
#> 72 72  f1 =~  x2    1     3     3   59     NA   0        .p72. 1.703 1.626
#> 75 75  f2 =~  x5    1     3     3   61     NA   0        .p75. 1.210 0.486
#>       se
#> 2  0.158
#> 12 0.289
#> 13 0.175
#> 14 0.175
#> 15 0.225
#> 16 0.188
#> 17 0.173
#> 18 0.204
#> 19 0.303
#> 20 0.357
#> 21 0.375
#> 22 0.293
#> 23 0.080
#> 37 0.218
#> 72 1.114
#> 75 0.155
# Note that group 1 is "Beta", not "Alpha"
lavInspect(fit_gp, "group.label")
#> [1] "Beta"  "Alpha" "Gamma"

```
