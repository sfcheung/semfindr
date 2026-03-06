# Sample Data: A Multiple-Group CFA Model with an Influential Case

A six-variable dataset with 100 cases.

## Usage

``` r
cfa_dat_mg
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

- gp:

  Group variable. Character. Either "GroupA" or "GroupB".

## Examples

``` r
library(lavaan)
data(cfa_dat_mg)
mod <-
"
f1 =~  x1 + x2 + x3
f2 =~  x4 + x5 + x6
"
fit1 <- cfa(mod, cfa_dat_mg, group = "gp")
fit2 <- cfa(mod, cfa_dat_mg, group = "gp",
            group.equal = "loadings")
fit3 <- cfa(mod, cfa_dat_mg, group = "gp",
            group.equal = c("loadings", "intercepts"))
lavTestLRT(fit1, fit2, fit3)
#> 
#> Chi-Squared Difference Test
#> 
#>      Df    AIC    BIC  Chisq Chisq diff RMSEA Df diff Pr(>Chisq)
#> fit1 16 1878.3 1977.3 30.454                                    
#> fit2 20 1871.7 1960.3 31.904      1.450     0       4     0.8355
#> fit3 24 1866.2 1944.3 34.357      2.453     0       4     0.6531
lavTestLRT(fit1, fit3)
#> 
#> Chi-Squared Difference Test
#> 
#>      Df    AIC    BIC  Chisq Chisq diff RMSEA Df diff Pr(>Chisq)
#> fit1 16 1878.3 1977.3 30.454                                    
#> fit3 24 1866.2 1944.3 34.357      3.903     0       8     0.8658

# Drop the first case
cfa_dat_mgb <- cfa_dat_mg[-1, ]
fit1b <- cfa(mod, cfa_dat_mgb, group = "gp")
fit2b <- cfa(mod, cfa_dat_mgb, group = "gp",
             group.equal = "loadings")
fit3b <- cfa(mod, cfa_dat_mgb, group = "gp",
             group.equal = c("loadings", "intercepts"))
lavTestLRT(fit1b, fit2b, fit3b)
#> 
#> Chi-Squared Difference Test
#> 
#>       Df    AIC    BIC  Chisq Chisq diff RMSEA Df diff Pr(>Chisq)
#> fit1b 16 1861.0 1959.6 29.392                                    
#> fit2b 20 1854.4 1942.6 30.731     1.3391     0       4     0.8547
#> fit3b 24 1849.3 1927.2 33.714     2.9826     0       4     0.5607
lavTestLRT(fit1b, fit3b)
#> 
#> Chi-Squared Difference Test
#> 
#>       Df    AIC    BIC  Chisq Chisq diff RMSEA Df diff Pr(>Chisq)
#> fit1b 16 1861.0 1959.6 29.392                                    
#> fit3b 24 1849.3 1927.2 33.714     4.3217     0       8      0.827
```
