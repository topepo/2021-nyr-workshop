# Machine Learning course for NYR 2021

## Requirements

A relatively recent version of R and the current version of the tidymodels packages. 

You can access the R packages in two different ways.

## Local Install

To install:

```r
install.packages(
  c("tidymodels", "glmnet", "lubridate", "Cubist", "rules", "DALEXtra"), 
  repos = "http://cran.rstudio.com"
)
```

Optionally, you can also use the new `shinymodels` package. Install it using the `remotes` package:

```r
install.packages(c("remotes"), repos = "http://cran.rstudio.com")

remotes::install_github("adhikars11/shinymodels")
```

## RStudio Cloud

Max will have a link if you would like to use RStudio Cloud (at no cost but probably requires an  account). 
