#' ---
#' title: "Script for building the EER corpus"
#' author: "Aurélien Goutsmedt and Alexandre Truc"
#' date: "/ Last compiled on `r format(Sys.Date())`"
#' output: 
#'   github_document:
#'     toc: true
#'     number_sections: true
#' ---
#' 
#' # What is this script for?
#' 
#' This script aims at extracting all the articles of the EER, the references, the list of authors
#' and their affiliations. It also extracts the same data for the four missing years (1970-1973).
#' 
#' 
#' > WARNING: This script still needs a lot of cleaning
#' 
#' 
#+ r setup, include = FALSE
knitr::opts_chunk$set(eval = FALSE)