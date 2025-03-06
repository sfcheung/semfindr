# Adapted from https://www.kloppenborg.ca/2021/06/long-running-vignettes/

base_dir <- getwd()

setwd("vignettes/")
knitr::knit("semfindr.Rmd.original", output = "semfindr.Rmd")

setwd(base_dir)

# For articles

base_dir <- getwd()

setwd("vignettes/articles")
knitr::knit("user_function.Rmd.original", output = "user_function.Rmd")

setwd(base_dir)
