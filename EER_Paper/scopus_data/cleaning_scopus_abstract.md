Script for extracting abstracts of EER articles from scopus
================
Aurélien Goutsmedt
/ Last compiled on 2021-04-01

  - [1 What is this script for?](#what-is-this-script-for)
  - [2 Loading packages, paths and
    data](#loading-packages-paths-and-data)
  - [3 Cleaning scopus data](#cleaning-scopus-data)
      - [3.1 Cleaning the .txt file](#cleaning-the-txt-file)
          - [3.1.1 removing useless lines.](#removing-useless-lines)
          - [3.1.2 identifying the relevant information to put them in a
            data
            frame](#identifying-the-relevant-information-to-put-them-in-a-data-frame)
      - [3.2 Cleaning the corpus](#cleaning-the-corpus)
          - [3.2.1 Cleaning `author`](#cleaning-author)
          - [3.2.2 Cleaning `scopus_art`](#cleaning-scopus_art)

# 1 What is this script for?

This script aims at extracting the data of all EER articles from scopus,
notably the abstract. The resulting data table will be merged with the
corpus extracted from WoS, in order to use textual analysis on
abstracts.

# 2 Loading packages, paths and data

``` r
library(tidyverse)
library(data.table)
eer_data <- "/projects/data/macro_AA/EER/Corpus_EER/"
source("~/macro_AA/functions/functions_for_cleaning_strings.R")
```

We import the list of articles extracted from SCOPUS

``` r
scopus <- read_delim(paste0(eer_data,"EER_scopus_abstract.txt"), "\t", escape_double = FALSE, trim_ws = TRUE) %>% data.table()
```

# 3 Cleaning scopus data

## 3.1 Cleaning the .txt file

We need first to extract the relevant lines to put them in a data frame
(authors, title, volume, number, pages, year, abstracts).

### 3.1.1 removing useless lines.

This step is not absolutely necessary, but it helps us to have a cleaner
text, and to focus on the relevant information

``` r
remove_lines <- c("ISSN",
                  "LANGUAGE OF",
                  "ABBREVIATED SOURCE",
                  "DOCUMENT TYPE",
                  "PUBLICATION STAGE",
                  "DOI",
                  "SOURCE",
                  "OPEN ACCESS",
                  "https:",
                  "CORRESPONDENCE ADDRESS",
                  "FUNDING ",
                  "INDEX KEYWORDS")

delete <- which(str_detect(scopus$Scopus, pattern = paste0(remove_lines, collapse = "|")))
scopus <- scopus[-delete]
```

### 3.1.2 identifying the relevant information to put them in a data frame

This data frame will be the list of our scopus articles for the four
missing years.

#### 3.1.2.1 Identifying ids

``` r
scopus$id <- str_detect(scopus$Scopus, pattern = "^[:digit:]{8,}") # We need this to identify the name then
```

#### 3.1.2.2 Identifying authors

``` r
name <- which(scopus$id == TRUE) - 1 # this saves the position in the text of authors names (one line above the id)

scopus$author <- FALSE # useful later for abstracts
scopus[name]$author <- TRUE
```

#### 3.1.2.3 identifying title

``` r
titre <- which(scopus$id == TRUE) + 1
```

#### 3.1.2.4 Identifying informations on the article (journal, volume, etc.)

``` r
info <- which(scopus$id == TRUE) + 2
```

#### 3.1.2.5 Extracting abstracts

All the abstracts are one unique line, which makes it simple. But the
problem is that not all articles have an abstract. We should thus detect
article with and without abstract. We only keep the lines of authors and
the line of abstract. The strategy is simple: we take a line with
authors and if the next line is not an abstract (meaning that is rather
the authors of the next article), it means that the article has no
abstract and we register that. At the end with have a data table with
the name of authors of articles with an abstract, and the corresponding
abstract.

``` r
abstract <- which(str_detect(scopus$Scopus, pattern = "ABSTRACT"))
scopus[abstract, abstract_line := TRUE]
scopus[is.na(abstract_line)]$abstract_line <- FALSE

extract_abstract <- scopus[abstract_line == TRUE | author == TRUE, -c("id")]
extract_abstract$abstract_check <- NA

for(i in seq_along(extract_abstract$abstract_line)){
  if(extract_abstract$author[i] == TRUE){
    if(extract_abstract$abstract_line[i+1] == TRUE){
      extract_abstract$abstract_check[i] <- TRUE
    } else {
      extract_abstract$abstract_check[i] <- FALSE
    }
  }
}

abstract_list <- data.table("author" = extract_abstract[abstract_check == TRUE]$Scopus,
                            "abstract" = extract_abstract[abstract_line == TRUE]$Scopus)
```

#### 3.1.2.6 Creating the data frame of articles

We extract the information we need for each article and merge with the
abstracts. As we are interest in abstract content, we don’t take
articles without abstracts.

``` r
scopus_art <- data.table("temp_id" = 1:length(scopus[id == TRUE]$Scopus),
                         "author" = scopus[name]$Scopus, 
                         "title" = scopus[titre]$Scopus,
                         "info" = scopus[info]$Scopus)

scopus_art <- merge(scopus_art,abstract_list, by = "author")
```

We remove the lines which are articles from the “European Economic
Review of Economic History”, and not from the EER.

``` r
scopus_art <- scopus_art[-which(str_detect(info, "Economic History"))]
```

## 3.2 Cleaning the corpus

Now, we just have to clean a bit the data. We need to keep the first
author and put it in the WoS format (SURNAME-F), to clean journal and
volume info.

### 3.2.1 Cleaning `author`

We need to separate surnames and initials for each authors.

The first step is to calculate the maximum number of authors for an
article. We use the number of comma in a row to calculate this (one
comma, separating surname and initial means 1 author, 3 commas mean 2
authors, etc.). Then we can create the different columns to put the
surnames and initials in.

``` r
nb_authors <- max((str_count(scopus_art$author, ",") + 1)/2)
name_column <- paste0(c("surname_","initial_"),rep(1:nb_authors, each = 2))

scopus_art  <- scopus_art %>% 
  separate(author, name_column, ",")
```

We want to avoid NA initials and we want to clean this as soon as
possible, thus we check if we have missing values

``` r
test  <-  scopus_art %>% 
  filter(!is.na(surname_1) & is.na(initial_1)) %>% 
  select(surname_1)
if(length(test$surname_1) > 0){
  message("Missing values for initials")
} else {
  message("No missing values for initials")
}
```

we now want to create authors column like in WoS

``` r
# cleaning by removing space, punctuation, and fusioning surnames and initials
scopus_art <- scopus_art %>% 
  select(temp_id, title, info, surname_1, initial_1, info, abstract) %>%
  mutate(across(contains("surname"), ~ remove_space(.x, replacement = "-")),
         across(contains("initial"), ~ remove_punct(remove_space(.x))),
         across(contains("surname"), ~ str_replace(.x,"^-",""))) %>% 
  .[, author := paste0(surname_1,"-",initial_1)] %>% 
  .[, -..name_column[1:2]]
```

### 3.2.2 Cleaning `scopus_art`

We need to put the year, the Review (which is the same in our case), the
volume, the number, and the pages in separated columns, and delete the
number of citations.

``` r
scopus_art[, `:=` (title = toupper(title), # useful for later
                   Annee_Bibliographique = extract_year(info),
                   Journal = "European Economic Review",
                   info = str_remove(str_remove(info, ".*Review, "), ". Cited .*"))] %>% 
  .[, `:=` (Volume = str_extract(info, "^.[:digit:]{1,2}"),
            Issue = str_remove_all(str_extract(info, "\\([:digit:]{1,2}\\)|\\([:digit:]{1}-[:digit:]{1}\\)"),"[\\(\\)]"),
            Pages = str_remove(str_remove(str_extract(info, "pp.*"), "pp. "), "\\."))] # We first extract the number following pp., then we remove pp. and the final point.
```

The `scopus_art` is now clean, and we can save it \!

``` r
saveRDS(scopus_art[,c("temp_id","author","Annee_Bibliographique","Journal","Volume","Issue","Pages","abstract")], paste0(eer_data,"scopus_abstract.RDS"))
```
