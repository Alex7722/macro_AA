Functions use for cleaning text strings
================
Aurélien Goutsmedt
/ Last compiled on 2021-03-28

  - [1 What is this script for?](#what-is-this-script-for)
  - [2 Simple shortcut functions for cleaning
    strings](#simple-shortcut-functions-for-cleaning-strings)

# 1 What is this script for?

This script lists all the functions built for simplifying the cleaning
of text strings. The functiosn here are used in:

  - [new\_script\_cleaning\_scopus\_data.R](/EER_Paper/scopus_data/new_script_cleaning_scopus_data.R)
    \# Loading packages

`docstring` is used to see the documentation of the functions load in
the global environment.

``` r
if ("docstring" %in% installed.packages() == FALSE) {
  install.packages("docstring", dependencies = TRUE)
}
library(docstring)
```

# 2 Simple shortcut functions for cleaning strings

``` r
remove_space <- function(x, replacement = "") str_replace_all(x, " ",replacement) # function to remove white space
remove_punct <- function(x) str_replace_all(x, "[:punct:]","") # function to remove punctuation
extract_year <- function(x) str_remove_all(str_extract(x, "\\([:digit:]{4}\\)"), "[\\(\\)]") # extracting the year of a reference when the year is between brackets, then removing the brackets.
```
