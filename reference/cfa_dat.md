# Sample Data: A CFA Model

A six-variable dataset with 100 cases.

## Usage

``` r
cfa_dat
```

## Format

A data frame with 100 rows and 6 variables:

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
#> This is lavaan 0.6-21
#> lavaan is FREE software! Please report any bugs.
data(cfa_dat)
mod <-
"
f1 =~  x1 + x2 + x3
f2 =~  x4 + x5 + x6
"
fit <- cfa(mod, cfa_dat)
summary(fit)
#> lavaan 0.6-21 ended normally after 37 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        13
#> 
#>   Number of observations                           100
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                12.027
#>   Degrees of freedom                                 8
#>   P-value (Chi-square)                           0.150
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Latent Variables:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   f1 =~                                               
#>     x1                1.000                           
#>     x2                0.767    0.225    3.405    0.001
#>     x3                1.047    0.296    3.542    0.000
#>   f2 =~                                               
#>     x4                1.000                           
#>     x5                2.114    0.869    2.431    0.015
#>     x6                0.992    0.377    2.635    0.008
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   f1 ~~                                               
#>     f2                0.171    0.091    1.884    0.060
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .x1                0.841    0.221    3.802    0.000
#>    .x2                1.214    0.208    5.823    0.000
#>    .x3                1.018    0.251    4.064    0.000
#>    .x4                1.103    0.186    5.918    0.000
#>    .x5                0.993    0.437    2.270    0.023
#>    .x6                0.882    0.158    5.575    0.000
#>     f1                0.708    0.262    2.703    0.007
#>     f2                0.250    0.151    1.659    0.097
#> 
```
