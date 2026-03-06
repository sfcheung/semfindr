# Sample Data: A CFA Model with a Heywood Case

A six-variable dataset with 60 cases, with one case resulting in
negative variance if not removed.

## Usage

``` r
cfa_dat_heywood
```

## Format

A data frame with 60 rows and 6 variables:

- x1:

  Indicator. Numeric.

- x2:

  Indicator. Numeric.

- x3:

  Indicator. Numeric.

- x4:

  Indicator. Numeric.

- x5:

  Indicator. Numeric.

- x6:

  Indicator. Numeric.

## Examples

``` r
library(lavaan)
data(cfa_dat_heywood)
mod <-
"
f1 =~  x1 + x2 + x3
f2 =~  x4 + x5 + x6
"
# The following will result in a warning
fit <- cfa(mod, cfa_dat_heywood)
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
# One variance is negative
parameterEstimates(fit, output = "text")
#> 
#> Latent Variables:
#>                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
#>   f1 =~                                                                 
#>     x1                1.000                               1.000    1.000
#>     x2                0.148    0.191    0.776    0.438   -0.226    0.523
#>     x3                0.416    0.369    1.126    0.260   -0.308    1.140
#>   f2 =~                                                                 
#>     x4                1.000                               1.000    1.000
#>     x5                0.702    0.299    2.346    0.019    0.115    1.288
#>     x6                0.504    0.236    2.140    0.032    0.042    0.966
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
#>   f1 ~~                                                                 
#>     f2                0.402    0.191    2.100    0.036    0.027    0.777
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
#>    .x1               -0.004    1.263   -0.004    0.997   -2.479    2.470
#>    .x2                1.902    0.348    5.460    0.000    1.220    2.585
#>    .x3                1.296    0.322    4.024    0.000    0.665    1.928
#>    .x4                0.569    0.354    1.605    0.108   -0.126    1.263
#>    .x5                1.000    0.251    3.977    0.000    0.507    1.492
#>    .x6                1.044    0.215    4.863    0.000    0.624    1.465
#>     f1                1.504    1.292    1.164    0.245   -1.029    4.036
#>     f2                0.872    0.416    2.095    0.036    0.056    1.688
#> 
# Fit the model with the first case removed
fit_no_case_1 <- cfa(mod, cfa_dat_heywood[-1, ])
# Results admissible
parameterEstimates(fit_no_case_1, output = "text")
#> 
#> Latent Variables:
#>                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
#>   f1 =~                                                                 
#>     x1                1.000                               1.000    1.000
#>     x2                0.552    0.307    1.800    0.072   -0.049    1.154
#>     x3                0.860    0.436    1.971    0.049    0.005    1.715
#>   f2 =~                                                                 
#>     x4                1.000                               1.000    1.000
#>     x5                0.887    0.373    2.377    0.017    0.156    1.619
#>     x6                0.626    0.275    2.276    0.023    0.087    1.165
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
#>   f1 ~~                                                                 
#>     f2                0.252    0.164    1.536    0.125   -0.070    0.574
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|) ci.lower ci.upper
#>    .x1                0.810    0.369    2.196    0.028    0.087    1.534
#>    .x2                1.360    0.278    4.886    0.000    0.815    1.906
#>    .x3                0.947    0.306    3.090    0.002    0.346    1.548
#>    .x4                0.765    0.310    2.466    0.014    0.157    1.373
#>    .x5                0.904    0.276    3.277    0.001    0.363    1.445
#>    .x6                1.014    0.221    4.592    0.000    0.581    1.446
#>     f1                0.689    0.410    1.683    0.092   -0.114    1.493
#>     f2                0.695    0.359    1.936    0.053   -0.009    1.399
#> 
```
