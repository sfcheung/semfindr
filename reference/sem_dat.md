# Sample Data: A Latent Variable Structural Model

A nine-variable dataset with 200 cases.

## Usage

``` r
sem_dat
```

## Format

A data frame with 200 rows and 9 variables:

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

- x7:

  Indicator. Numeric.

- x8:

  Indicator. Numeric.

- x9:

  Indicator. Numeric.

## Examples

``` r
library(lavaan)
data(sem_dat)
mod <-
"
f1 =~  x1 + x2 + x3
f2 =~  x4 + x5 + x6
f3 =~  x7 + x8 + x9
f2 ~ a * f1
f3 ~ b * f2
ab := a * b
"
fit <- sem(mod, sem_dat)
summary(fit)
#> lavaan 0.7-2 ended normally after 37 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        20
#> 
#>   Number of observations                           200
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                41.768
#>   Degrees of freedom                                25
#>   P-value (Chi-square)                           0.019
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
#>     x2                0.590    0.145    4.054    0.000
#>     x3                0.808    0.168    4.812    0.000
#>   f2 =~                                               
#>     x4                1.000                           
#>     x5                0.730    0.099    7.400    0.000
#>     x6                0.429    0.083    5.166    0.000
#>   f3 =~                                               
#>     x7                1.000                           
#>     x8                2.019    0.589    3.426    0.001
#>     x9                2.747    0.788    3.486    0.000
#> 
#> Regressions:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   f2 ~                                                
#>     f1         (a)    1.115    0.233    4.788    0.000
#>   f3 ~                                                
#>     f2         (b)    0.206    0.061    3.394    0.001
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .x1                1.183    0.173    6.831    0.000
#>    .x2                1.129    0.127    8.909    0.000
#>    .x3                1.027    0.134    7.667    0.000
#>    .x4                0.833    0.173    4.812    0.000
#>    .x5                1.078    0.140    7.714    0.000
#>    .x6                1.234    0.132    9.367    0.000
#>    .x7                1.056    0.112    9.428    0.000
#>    .x8                1.042    0.139    7.478    0.000
#>    .x9                1.077    0.197    5.470    0.000
#>     f1                0.658    0.190    3.474    0.001
#>    .f2                0.647    0.215    3.010    0.003
#>    .f3                0.062    0.035    1.771    0.077
#> 
#> Defined Parameters:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     ab                0.230    0.079    2.895    0.004
#> 
#> lavaan NOTE:  
#>    Standard errors and confidence intervals of the (nonlinear) defined (:=) 
#>    parameters are based on the first-order delta method; for strongly 
#>    nonlinear definitions, se.def = "mc" (Monte Carlo) or se = "bootstrap" may 
#>    be more accurate.
```
