#' ---
#' title: "Functions use for cleaning text strings"
#' author: "Aurélien Goutsmedt"
#' date: "/ Last compiled on `r format(Sys.Date())`"
#' output: 
#'   github_document:
#'     toc: true
#'     number_sections: true
#' ---

#+ r setup, include = FALSE
knitr::opts_chunk$set(eval = FALSE)

#' # What is this script for?
#' 
#' This script lists all the functions built for simplifying the cleaning of text strings. The functiosn here are used in:
#' 
#' - [new_script_cleaning_scopus_data.R](/EER_Paper/scopus_data/new_script_cleaning_scopus_data.R)


#' # Loading packages
#' 
#' `docstring` is used to see the documentation of the functions load in the global environment.
if ("docstring" %in% installed.packages() == FALSE) {
  install.packages("docstring", dependencies = TRUE)
}
library(docstring)

#' # Simple shortcut functions for cleaning strings
#' 

remove_space <- function(x, replacement = "") str_replace_all(x, " ",replacement) # function to remove white space
remove_punct <- function(x) str_replace_all(x, "[:punct:]","") # function to remove punctuation
extract_year <- function(x) str_remove_all(str_extract(x, "\\([:digit:]{4}\\)"), "[\\(\\)]") # extracting the year of a reference when the year is between brackets, then removing the brackets.
