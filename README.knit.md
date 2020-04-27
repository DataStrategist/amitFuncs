---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->


# amitFuncs

Random convenience functions from a lazy person

## Installation

You can install the released version of amitFuncs from github by using:


```r
# remotes::install_github("DataStrategist/amitFuncs")
```

## Exported Functions

### left, mid, right

Excel like convenience functions:


```r
library(amitFuncs)
left("boom", 1)
#> [1] "b"
right("boom", 1)
#> [1] "m"
amitFuncs::mid("boom", 2, 2)
#> [1] "oo"
```

### gitLister

List all git repos within a certain folder. For example, to list all the repos in the folder above your current `getwd()`, type:


```r
tail(gitLister(paff = ".."), 5)
#>                      name  git
#> 173 Wordcloud.from.folder TRUE
#> 174             wordtreer TRUE
#> 175       WorldMapNetwork   NA
#> 176      xgboost examples TRUE
#> 177         Youtube stuff TRUE
```
 

### libraryFinder

Finds all the libraries used in files modified within `x` days ago. For example, to list the libraries required for the current `getwd()` that have been modified in the past 2 weeks, type:


```r
libraryFinder(parentFolder = ".", howLong = 14, whatLib = "c:/temp/")
#> ___________________________________________
#> 
#> INSTALL THESE CRAN PACKAGES: 
#> 
#> 
#> install.packages(c("dplyr","purrr","rvest","stringr","tibble","tidyr","xml2","remotes"), lib = "c:/temp/")
#> ___________________________________________
#> NOT ON CRAN 
#> 
#>  ` | amitFuncs
```

This will output a command that you can copypaste to install all those libraries into a `c:/temp`.
