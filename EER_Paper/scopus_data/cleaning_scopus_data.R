#' ---
#' title: " Script for cleaning the missing articles and references extracting in Scopus"
#' author: "Aurélien Goutsmedt"
#' date: "/ Last compiled on `r format(Sys.Date())`"
#' output: 
#'   github_document:
#'     toc: true
#'     number_sections: true
#' ---
#' 
#' # What is this script for?
#' 
#' This script aims at cleaning scopus data. The input is a .txt with articles metadata and their references
#' extracted from scopus. It aims at producing a data.frame that we will merge with WoS data.
#' 
#' 
#' 
#+ r setup, include = FALSE
knitr::opts_chunk$set(eval = FALSE)

#' # Loading packages, paths and data
#' 

library(tidyverse)
library(data.table)
eer_data <- "/projects/data/macro_AA/EER/Corpus_EER/"
source("~/macro_AA/functions/functions_for_cleaning_strings.R")

#' We import the list of articles extracted from SCOPUS

scopus <- read_delim(paste0(eer_data,"EER_scopus.txt"), "\t", escape_double = FALSE, trim_ws = TRUE) %>% data.table()

#' # Cleaning scopus data
#' 
#' ## Cleaning the .txt file
#' We need first to extract the relevant lines to put them in a data frame (authors, id, affiliations, title, journal, etc...). We also needs to save the references 
#' for each article in a list object. At the end of this part, we will have a list of the articles from the missing years, and a direct citation
#' data frame with all the references cited by the articles.
#' 
#' ### removing useless lines.
#' 
#' This step is not absolutely necessary, but it helps us to have a cleaner text, and to focus on the relevant information
remove_lines <- c("ISSN",
                  "LANGUAGE OF",
                  "ABBREVIATED SOURCE",
                  "DOCUMENT TYPE",
                  "PUBLICATION STAGE",
                  "DOI",
                  "ABSTRACT",
                  "SOURCE",
                  "OPEN ACCESS",
                  "https:",
                  "CORRESPONDENCE ADDRESS",
                  "FUNDING ",
                  "INDEX KEYWORDS")

delete <- which(str_detect(scopus$Scopus, pattern = paste0(remove_lines, collapse = "|")))
scopus <- scopus[-delete]

#' ### identifying the relevant information to put them in a data frame
#' 
#' This data frame will be the list of our scopus articles for the four missing years.
#' 
#' #### Identifying ids
scopus$id <- str_detect(scopus$Scopus, pattern = "^[:digit:]{8,}")

#' #### Identifying authors
name <- which(scopus$id == TRUE) - 1 # this saves the position in the text of authors names (one line above the id)

scopus$author <- FALSE # useful later for affiliations
scopus[name]$author <- TRUE

#' #### identifying title
titre <- which(scopus$id == TRUE) + 1

#' #### Identifying informations on the article (journal, volume, etc.)
info <- which(scopus$id == TRUE) + 2

#' #### Identifying authors's affiliations
#' 
#' The problem here is that some affiliations are on two lines when you have several authors or when
#' one author has several affiliations. N.B.: in the case of several authors, we are unable to 
#' know which affiliation is corresponding to each author.

scopus$start_ref <- str_detect(scopus$Scopus, pattern = "REFERENCES:") # identifying the first line of the references. We will also need this later
scopus$affiliation <- str_detect(scopus$Scopus, pattern = "AFFILIATIONS:") # finding the line of affiliation
aff <- which(scopus$affiliation == TRUE) + 1 # taking the next line
scopus[aff][author != TRUE & start_ref != TRUE]$affiliation  <- TRUE
aff <- which(scopus$affiliation == TRUE) 
affiliation  <- scopus[aff]$Scopus # we take all the line with affiliations

# when affiliation is in two lines we merge the two lines (no better method find)
for(i in 1:length(affiliation)){
  if(!str_detect(affiliation[i], pattern = "AFFILIATIONS")){
    affiliation[i] <- paste0(affiliation[i-1],affiliation[i])
    affiliation[i-1] <- NA
  }
}
affiliation  <- as.data.table(affiliation)[!is.na(affiliation)]

# we put NA for articles without affiliations
affiliation <-  rbind(affiliation,
                  rep(NA,length(scopus[id == TRUE]$Scopus) - length(affiliation$affiliation)),
                  use.names = FALSE) # filling missing values with NA

#' #### Creating the data frame of articles

scopus_art <- data.table("temp_id" = 1:length(scopus[id == TRUE]$Scopus),
           "id" = scopus[id == TRUE]$Scopus, 
           "author" = scopus[name]$Scopus, 
           "title" = scopus[titre]$Scopus,
           "info" = scopus[info]$Scopus,
           "affiliation" = affiliation)


#' ### Identifying references 
#' 
#' This task is a bit harder as they are on several lines. What we do is to 
#' associate each reference of an article to the `temp_id` of the article.  

start_ref <- which(scopus$start_ref == TRUE)
end_ref <- which(scopus$id == TRUE) - 2 # identifying the last line of the references (two lines above the id)
end_ref <- c(end_ref[-1], length(scopus$Scopus)) # removing the first value which is logically 0 (for the first article) and adding the last line of the text which is the last reference of the last article

#' We delete each line which corresponds to an article without references. Indeed, we have a `end_ref` value, but no `start_ref`
#' as there is no reference in this article. We also do it for the names, as it will be needed later to match each set of references
#' with their corresponding article.

for(i in 1:1000){
  name <- name[-min(which(start_ref > end_ref))]
  end_ref <- end_ref[-min(which(start_ref > end_ref))]
  
  if(length((which(start_ref > end_ref))) == 0){
    break
  }
}

#' We create the data table which associates each `temp_id` of articles with at least one reference, to the references they cite. 
#' That is a direct citation data frame that we now need to clean.
scopus_ref  <- data.table("temp_id" = c(),
                          "references" = c())

for(i in 1:length(start_ref)){
ref_lines  <- start_ref[i]:end_ref[i]
ref_year <- data.table("temp_id" = scopus_art[author %in% scopus[name]$Scopus]$temp_id[i],
                       "references" = scopus[ref_lines]$Scopus)
scopus_ref  <- rbind(scopus_ref,ref_year)
}

#' We create the data table which associates each `temp_id` of articles with at least one reference, to the references they cite. 
#' That is a direct citation data frame that we now need to clean.
scopus_ref  <- data.table("temp_id" = c(),
                          "references" = c())

for(i in 1:length(start_ref)){
  ref_lines  <- start_ref[i]:end_ref[i]
  ref_year <- data.table("temp_id" = scopus_art[author %in% scopus[name]$Scopus]$temp_id[i],
                         "references" = scopus[ref_lines]$Scopus)
  scopus_ref  <- rbind(scopus_ref,ref_year)
}


#' ## Cleaning the corpus
#' 
#' We first tackle the data table with the list of articles. WE need to clean the id,
#' giving names corresponding to the WoS format (SURNAME-F), creating as many lines as author,
#' cleaning journal and volume info, as well as affiliations.
#' 

#' ### Cleaning `id`
#' 
#' We only keep the first id, before the first semicolon

scopus_art  <- scopus_art[, id:= str_replace(id, pattern = ";.*", "")]

#' ### Cleaning `author`
#' 
#' We need to separate surnames and initials for each authors.
#' 
#' The first step is to calculate the maximum number of authors for an article. We use the number
#' of comma in a row to calculate this (one comma, separating surname and initial means 1 author,
#' 3 commas mean 2 authors, etc.). Then we can create the different columns to put the surnames 
#' and initials in.

nb_authors <- max((str_count(scopus_art$author, ",") + 1)/2)
name_column <- paste0(c("surname_","initial_"),rep(1:nb_authors, each = 2))

scopus_art  <- scopus_art %>% 
separate(author, name_column, ",")

#' We want to avoid NA initials and we want to clean this as soon as possible, thus we check if we have 
#' missing values

test  <-  scopus_art %>% 
  filter(!is.na(surname_1) & is.na(initial_1)) %>% 
  select(surname_1)
if(length(test$surname_1) > 0){
  message("Missing values for initials")
} else {
    message("No missing values for initials")
  }

scopus_art %>% 
  filter(!is.na(surname_2) & is.na(initial_2)) %>% 
  select(surname_2)
if(length(test$surname_2) > 0){
  message("Missing values for initials")
} else {
  message("No missing values for initials")
}

#' we now want to create authors column like in WoS
# cleaning by removing space, punctuation, and fusioning surnames and initials
scopus_art %<>% 
  mutate(across(contains("surname"), ~ remove_space(.x, replacement = "-")),
         across(contains("initial"), ~ remove_punct(remove_space(.x))),
         across(contains("surname"), ~ str_replace(.x,"^-",""))) %>% 
  .[, `:=` (author_1 = paste0(surname_1,"-",initial_1),
            author_2 = paste0(surname_2,"-",initial_2),
            author_3 = paste0(surname_3,"-",initial_3),
            author_4 = paste0(surname_4,"-",initial_4),
            author_5 = paste0(surname_5,"-",initial_5))] %>% 
  .[, -..name_column]

#' we now wants to have all the authors in one column.

scopus_art %<>% 
  pivot_longer(cols = starts_with("author"), 
               names_to = c("order"), 
               values_to = "author")  %>% 
  filter(author != "NA-NA") %>% 
  mutate(order = str_extract(order, "[:digit:]")) %>% # just keeping the number in order, giving us the order of authors in the paper (as in WoS)
  as.data.table()

#' we save the list of authors per article in a separate dataframe. In the main dataframe, we will only keep the 
#' rows with the first author. We also keep the affiliations in a separated dataframe
#' 
#' 

scopus_inst <- scopus_art[order == 1][,c("temp_id","affiliation.affiliation")]
scopus_art <- scopus_art[order == 1][, -c("id","order","affiliation.affiliation")]
 
#' ### Cleaning `scopus_art`
#' 
#' We need to put the year, the Review (which is the same in our case), the volume, the number, and the 
#' pages in separated columns, and delete the number of citations.

scopus_art[, `:=` (title = toupper(title), # useful for later
                   Annee_Bibliographique = extract_year(info),
                   Journal = "European Economic Review",
                   info = str_remove(str_remove(info, ".*Review, "), ". Cited .*"))] %>% 
  .[, `:=` (Volume = str_extract(info, "^."),
            Number = str_extract(info, "([:digit:]{1})"),
            Pages = str_remove(str_remove(str_extract(info, "pp.*"), "pp. "), "\\."))] # We first extract the number following pp., then we remove pp. and the final point.

# removing doublons for 1970 (actually they are in WoS)
scopus_art_1970 <- scopus_art[Annee_Bibliographique == 1970]$temp_id
scopus_art <- scopus_art[Annee_Bibliographique != 1970]
#' The `scopus_art` is now clean, and we can save it !
saveRDS(scopus_art[,-"info"], paste0(eer_data,"scopus_articles.RDS"))

#' ### Cleaning `affiliation` (`scopus_inst`)
#' 
#' We first have to separate the rows where there are two affiliations, and put them in a 
#' "long" format. 

affiliation_column <- paste0("affiliation_",1:(max(str_count(scopus_inst$affiliation.affiliation,";"), na.rm = TRUE) + 1))

scopus_inst %<>% 
  rename("affiliation" = affiliation.affiliation) %>% 
  mutate(affiliation = str_remove(affiliation, "AFFILIATIONS: ")) %>% 
  separate(affiliation, all_of(affiliation_column), ";") %>% 
  pivot_longer(all_of(affiliation_column), names_to = "order_inst", values_to = "affiliation") %>% 
  mutate(order_inst = str_remove(order_inst, "affiliation_")) %>% 
  filter(!is.na(affiliation))

#' We can now clean the affiliations:
#' 
#' - we first extract the last part, which is in general the country
#' - we clean the country by hand
#' - we split the remaining part, the first one being the institution, the second
#' one the city.
#' 

scopus_inst <- scopus_inst %>% 
  mutate(country = str_extract(affiliation, ",[a-zA-ZÀ-ÿ-'\\- ]{1,}$") %>% 
           str_remove(",|, ") %>% 
           str_trim("both")) %>% 
  mutate(country = replace(country, country == "Beograd", "Yoguslavia")) # hand cleaning

#' To take into account of missing countries, we delete the last part of affiliation for 
#' affiliations that contain the name of an identified country
#' 
countries <- unique(scopus_inst$country)

scopus_inst <- scopus_inst %>% 
  mutate(affiliation = str_remove(affiliation, paste0(countries,collapse = "|")) %>% 
           str_trim("right") %>% 
           str_remove(",$")) 

#' We can now tackle the remaining part of the affiliation. We count the number of 
#' commas before to split `affiliation`, in order to create the needed number of columns
#' and then to name properly the column (it should be `institution`,`city`,`state`, the latter
#' being only for the US).
affiliation_column <- paste0("affiliation_",1:(max(str_count(scopus_inst$affiliation,","), na.rm = TRUE) + 1))

scopus_inst <- scopus_inst %>% 
  separate(affiliation, all_of(affiliation_column), ",") %>% 
  mutate(across(contains("affiliation"), ~ str_trim(.x, "both"))) 

#' We identify states and cities:
scopus_inst <- scopus_inst %>% 
  mutate(city = !str_detect(affiliation_2, " |\\."), # string without space or point are cities
         state = str_extract(affiliation_3, "[A-Z]{2}")) %>% # two upper letters string are US states
  as.data.table()

#' We build the list of cities and remove them from `affiliation_2`: 
cities <- scopus_inst[city == TRUE | affiliation_2 == "The Hague", 
                      c("temp_id","order_inst","affiliation_2")] # We build the list of cities, adding "The Hague" we did not fit in the criteria above (no space, no point)
setnames(cities,"affiliation_2","city")
scopus_inst <- merge(scopus_inst[,-c("city")], 
                     cities, by = c("temp_id","order_inst"), all.x = TRUE) 
scopus_inst[affiliation_2 == city]$affiliation_2 <- NA # we remove affiliation_2 values that are cities

#' We are doing the same for states and `affiliation_3`
states <- unique(scopus_inst$state)
scopus_inst[str_detect(affiliation_3,paste0(c(states,"Netherlands"), collapse ="|"))]$affiliation_3 <- NA # removing already identified values

#' We add the only value of affiliation_4 which is a state
scopus_inst[!is.na(affiliation_4)]$state <- scopus_inst[!is.na(affiliation_4)]$affiliation_4 

#' We now tackle the corrections of small problems and errors:
#' 
scopus_inst[affiliation_1 == "Central Planning Bureau"]$country <- "Netherlands"
scopus_inst[affiliation_3 == "Washington"]$city <- scopus_inst[affiliation_3 == "Washington"]$affiliation_3  

add_ref <- data.table(temp_id = c(scopus_inst[affiliation_2 == "Ecole des Hautes Etudes"]$temp_id,
                                  scopus_inst[affiliation_3 == "and Université Libre de Bruxelles"]$temp_id),
                      order_inst = c(2,2),
                      affiliation_1 = c("EHESS","Université Libre de Bruxelles"),
                      city = c("Paris","Brussels"),
                      country = c("France","Belgium"))

scopus_inst[affiliation_1 == "University of Geneva"]$affiliation_2 <- NA
scopus_inst[affiliation_1 == "University of Geneva"]$affiliation_3 <- NA
scopus_inst[affiliation_1 == "University of Geneva"]$city <- "Geneva"
scopus_inst[affiliation_3 == "and Université Libre de Bruxelles"]$affiliation_3  <- NA

scopus_inst <- rbind(scopus_inst, add_ref, fill = TRUE)

scopus_inst[str_detect(affiliation_1, "CORE")]$city <- "Louvain"

#' We now remove the two useless columns (3 and 4) and merge the two remaining ones (1 and 2)
#' 
scopus_inst <- scopus_inst %>% 
  select(-c(affiliation_3, affiliation_4)) %>% 
  unite("institution",affiliation_1:affiliation_2, sep = ", ") %>% 
  mutate(institution = str_remove(institution, ", NA"))

#' We add the missing cities when they are in the list of cities we already have plus those
#' we can identify by looking at `institution`:
#' 
cities <- c(unique(scopus_inst[!is.na(city)]$city),
            "Leyden",
            "Tel-Aviv",
            "Piraeus",
            "Montréal",
            "Bowling Green",
            "Budapest",
            "Lund",
            "Naples",
            "Yale",
            "Cambridge")
scopus_inst <- scopus_inst %>%
  mutate(city_sup = str_extract(institution, paste0(cities, collapse = "|"))) %>% 
  as.data.table()
scopus_inst[is.na(city)]$city <- scopus_inst[is.na(city)]$city_sup

#' If we have the city of an institution, we use this city for replacing NA value in `city`
#' for the same institutions:
#' 
known_city <- scopus_inst[str_detect(institution,paste0(scopus_inst[is.na(city)]$institution, collapse = "|")), c("institution","city")][!is.na(city)]
for(i in seq_along(known_city$city)){
scopus_inst[institution == known_city$institution[i]]$city <- known_city$city[i]
}

#' We can now save the affiliations data
#'
saveRDS(scopus_inst[,-"city_sup"], paste0(eer_data,"scopus_institutions.RDS"))

#' ## Cleaning the references
#' 
#' We now tackle the direct citation data frame. We need to extract the different information in each references

#' ### Removing the term "REFERENCES"
#' 
scopus_ref[, references:= str_remove(references, "REFERENCES: ")]

#' ### Dealing with several references on line and cleaning
#' 
#' First problem, we sometimes have two references on one line. One way to detect this is to spot the lines
#' where the semi-colon is followed by at least one character. We thus need to split these lines in two and
#' retransforme the table in a long format.

# just to check, detecting if there are two reference on one line
if(length(which(str_detect(scopus_ref$references, ";."))) > 0){
  message("There are lines with two references. Need for splitting.")
}

scopus_ref %<>% 
  separate(references, c("references","ref2"), ";") %>% 
  pivot_longer(cols = c("references","ref2"),
               names_to = "order",
               values_to = "references") %>% 
  select(-order) %>% 
  filter(references != "" & !is.na(references))

#' We can now give an idea to each reference
#' 

setDT(scopus_ref)[, temp_idref := 1:.N]

#' We can clean the references by removing first and last space
#' 

scopus_ref[, references := str_trim(references, "both")]

#' A first additional cleaning we can do is to remove the references beginning by a year between brackets. In each case
#' either it is a very particular reference (a European report for instance) or it is the reference where one reference 
#' (the one in the line above) has been reprinted. That is not perfect to remove that but we don't need this information
#' and it will simplify our data and our work.
#' 

scopus_ref <- scopus_ref[!str_detect(scopus_ref$references, "^\\([:digit:]{4}\\)")]

#' Lastly, we correct some things that will cause problems later.
#' 

scopus_ref[, references := str_replace(references, "Kronsjo¨","Kronsjo")]
scopus_ref[, references := str_remove(references, "et al.,")]

  
#' ### Determining the type of references
#' 
#' We need first to know if a reference is an article (title is before year), or if it
#' is a book or report or similar. 
#' 

scopus_ref[, article := str_detect(references, "^[a-zA-ZÀ-ÿ-'\\- ]{1,}, [a-zA-ZÀ-ÿ-]{1,}")]

#' Article also included chapters of books. Thus, the title will be the title of the chapter
#' and the `Journal` will be the title of the book. Other info on the book will be in `journal_info`
#' (see below).

#' ### Extracting the Year
#' 
#' That is the simplest task as we just need to extract the numbers between brackets. 
#' We will keep it in `references` as it will help us to identify the title. 
#' 
#' We will delete the references without a year. Either they are references to unpublished
#' conference paper, or to not yet published article, or they are the second line in a 
#' reference, which designate where the work (in the line above) has been reprinted. That's
#' a simplification that we can abandon later, but for now, we delete reference with no value
#' as a `Year`.
#' 

scopus_ref[, Year := extract_year(references)]
scopus_ref <- scopus_ref[!is.na(Year)]

#' ### Dealing with articles
#' 
#' For articles, we have the name of the authors, sometimes the first name, and the title of the article before the year between
#' brackets. First, we will extract the information before the bracket to put it in a separate column. We extract the year 
#' between brackets to be sure that we don't extract the text before a bracket in the title, and then remove the year and the bracket.
#' 

scopus_ref[article == TRUE, 
           article_info := str_remove(str_extract(references, ".*\\([:digit:]{4}\\)"),
                                      "\\([:digit:]{4}\\)")]

#' The second step is to try to identify when you have the first name of an author, or several authors.
#' 

# Extracting first author and removing it from article_info
scopus_ref[article == TRUE, `:=` (author_1 = str_remove(str_extract(article_info, "^[a-zA-ZÀ-ÿ-'\\- ]{2,},"), ","), # we need special characters and space (for two parts name)
                                  article_info = str_remove(article_info, "^[a-zA-ZÀ-ÿ-]{2,},"))]

# Then we count the number of commas on the remaining article info
scopus_ref[article == TRUE, comma_count := str_count(article_info, ",")]

# if 0 comma, it means we have the title of article
article_title <- scopus_ref[article == TRUE & comma_count == 0, c("temp_idref","article_info")] 
setnames(article_title, "article_info", "article_title")
scopus_ref <- merge(scopus_ref, article_title, by = "temp_idref", all.x = TRUE)

#' We can now split every article info with at least one comma. We create the number 
#' of columns corresponding to the number of splits (3 commas mean 4 splits). Then, we 
#' create a new object with the splitted information, in order to identify if each splits is
#' an author or a part of the title.
#' 
#' > warning: this is not an automatic step. It implies to look at the `article_info_1` column
#' and check if our code is able to separate authors from part of the titles. It involves a bit
#' of manual cleaning, by removing some word from authors-selection, or by adding some names 
#' (de la Vallée Poussin here).

info_title <- paste0("article_info_", 1:(max(scopus_ref$comma_count, na.rm = TRUE)+1)) 

working_title <- scopus_ref %>% 
  filter(comma_count > 0 & article == TRUE) %>% 
  separate(article_info, all_of(info_title), ",") %>% 
  mutate(across(.cols = info_title, ~str_trim(.x, side = "both"))) %>% # removing first and last whitespace if any
  select(temp_idref, info_title, comma_count) 

working_title <- working_title %>% 
  mutate(across(.cols = info_title, ~str_length(.x), .names = "{col}_length"))

working_title[!str_detect(article_info_1, " ") &
              !article_info_1 %in% c("Dimension","Industry-wide","Qualities") |  # need a bit of manual cleaning
               article_info_1 %in% c("de la Vallée Poussin","De Leeuw","de Wolff","de la Vinelle","van Praag"),  
              second_author := TRUE]

#' We can merge second author with the main table. When it is not second author, it means
#' it is part of the title (it cannot be the title as there was at least one comma, so something
#' is following), and we should merge it with the following columns. As we want to avoid merging
#' the name of the second author, we should replace it by an empty string.
#' 

scopus_ref <- merge(scopus_ref, 
                    working_title[second_author == TRUE, c("temp_idref","article_info_1")], 
                    by = "temp_idref", 
                    all.x = TRUE)
setnames(scopus_ref,"article_info_1","author_2")

working_title[second_author == TRUE]$article_info_1 <- "" 
working_title <- working_title %>% 
  unite("article_info_12",article_info_1:article_info_2, sep=" ") %>% 
  mutate(article_info_12_length = str_length(article_info_12)) %>%  # to repeat the same process
  mutate(article_info_12 = str_trim(article_info_12, side = "left"))
#' Before to repeat the same process, we can see that some raws for the new column created are empty. 
#' In other words, there is nothing after the second author. After checking, it appears that it is
#' books with two authors. We can thus set their `article` value as "FALSE". For these references, 
#' we also create a new column to remind us that we have already identified the two authors.

scopus_ref[temp_idref %in% working_title[article_info_12 == ""]$temp_idref, book_multi_authors := TRUE] 
scopus_ref[book_multi_authors == TRUE]$article <- FALSE 

#' We remove these books from the `working_title` dt.
#' 
working_title <- working_title[article_info_12 != ""]

#' we can now repeat the same process as above but with the new united column
#' 

working_title[!str_detect(article_info_12, " ") &
              !article_info_12 %in% c("Uncertainty","Profits") &
              article_info_12_length < 15  |  # need a bit of manual cleaning
              article_info_12 == "de la ValleéPoussin ",
              third_author := TRUE]

#' We repeat the merging
#' 
scopus_ref <- merge(scopus_ref, 
                    working_title[third_author == TRUE, c("temp_idref","article_info_12")], 
                    by = "temp_idref", 
                    all.x = TRUE)
setnames(scopus_ref,"article_info_12","author_3")

working_title[third_author == TRUE]$article_info_12 <- "" 
working_title <- working_title %>% 
  unite("article_info_123",article_info_12:article_info_3, sep=" ") %>% 
  mutate(article_info_123_length = str_length(article_info_123)) %>%  # to repeat the same process
  mutate(article_info_123 = str_trim(article_info_123, side = "left"))

#' We search again for books, now with 3 authors:

scopus_ref[temp_idref %in% working_title[article_info_123 == ""]$temp_idref, book_multi_authors := TRUE] 
scopus_ref[book_multi_authors == TRUE]$article <- FALSE 

#' We remove these books from the `working_title` dt.
#' 
working_title <- working_title[article_info_123 != ""]

#' We repeat a last time the process to identify a fourth author
#' 

working_title[!str_detect(article_info_123, " "),
              fourth_author := TRUE]

#' We repeat the merging
#' 
scopus_ref <- merge(scopus_ref, 
                    working_title[fourth_author == TRUE, c("temp_idref","article_info_123")], 
                    by = "temp_idref", 
                    all.x = TRUE)
setnames(scopus_ref,"article_info_123","author_4")

working_title[fourth_author == TRUE]$article_info_123 <- "" 
working_title <- working_title %>% 
  unite("article_info_1234", article_info_123:article_info_4, sep=" ") %>% 
  mutate(article_info_1234 = str_trim(article_info_1234, side = "left"))

#' We search again for books, now with 4 authors:

scopus_ref[temp_idref %in% working_title[article_info_1234 == ""]$temp_idref, book_multi_authors := TRUE] 
scopus_ref[book_multi_authors == TRUE]$article <- FALSE 

#' We remove these books from the `working_title` dt.
#' 
working_title <- working_title[article_info_1234 != ""]

#' We can now clean the title by removing the NA and merge them as titles
#' in the main dt.
#' 

working_title[, article_title_2 := str_remove(str_remove(article_info_1234, " NA$"), " NA$")] %>% 
  .[, article_title_2 := toupper(article_title_2)]

scopus_ref <- merge(scopus_ref, 
                    working_title[,c("temp_idref","article_title_2")], 
                    by = "temp_idref", 
                    all.x = TRUE)
scopus_ref[is.na(article_title) & 
             !is.na(article_title_2)]$article_title <- scopus_ref[is.na(article_title) & 
                                                                    !is.na(article_title_2)]$article_title_2

#' We still need to identify the journal, volume, numero, pages, etc... But we will do that later, as it will
#' be easier as soos as we have finished the work for books.

#' ### Dealing with books
#' 
#' We already have identified the authors of books when there are several authors.
#' We just need to identify the authors when there is only one.
#' 

scopus_ref[article == FALSE & 
           is.na(book_multi_authors),
           author_1 := str_extract(references, "^.*\\([:digit:]{4}\\)") %>% 
             str_remove(" \\([:digit:]{4}\\)") %>% 
             str_remove_all(",")]

#' We now need to identify the titles of the book. We will extract all the information after
#' the year between brackets, what will also be of used for identifying publication information
#' of articles later (journal, volume, etc.)

scopus_ref[, remaining_info := str_trim(str_remove(references, ".*\\([:digit:]{4}\\)"), "both")]
scopus_ref[article == FALSE, book_title := remaining_info]

scopus_ref[, book_title := str_trim(book_title, "both") %>% 
             str_remove_all(" ,")] %>% 
  .[, `:=` (book_info = str_remove(book_title, "^[a-zA-ZÀ-ÿ0-9':\\.\\-? ]{2,},"),
            book_title = str_remove(str_extract(book_title, "^[a-zA-ZÀ-ÿ0-9':\\.\\-? ]{2,},"),","))]

#' ### Back on articles: extracting Journal and other infos
#' 
#' We first need to extract the journal, that is the information before the comma.
#' When there is no comma in the string, it means that's the name of the journal.
#' 
scopus_ref[article == TRUE, comma_count := str_count(remaining_info, ",")] %>% 
  .[comma_count == 0, journal := remaining_info] %>% 
  .[comma_count > 0, `:=` (journal = str_remove(str_extract(remaining_info, "^[a-zA-ZÀ-ÿ:' ]{2,},"), ","),
                           journal_info = str_trim(str_remove(remaining_info, "^[a-zA-ZÀ-ÿ:' ]{2,},"), "both"))] %>% 
  .[!is.na(journal_info), `:=` (volume = str_extract(journal_info, "^[0-9]{1,3}"),
                                numero = str_remove_all(str_extract(journal_info, "\\([0-9]{1,3}\\)"), "[\\(\\)]"),
                                pages = str_remove_all(str_extract(journal_info, "p. [0-9].*"),"^p. |., .*| ff"))]

#' ### Passing in long format because of multiple authors
#' 
#' We will delete useless column, and pass the multiple authors in rows, meaning that a reference will have
#' as many lines as its number of authors.
#' 

scopus_ref <- scopus_ref[,-c("article_info",
                             "comma_count",
                             "book_multi_authors",
                             "article_title_2",
                             "remaining_info",
                             "book_info",
                             "journal_info")]

#' We merge the title column
scopus_ref[, title := article_title]
scopus_ref[!is.na(book_title)]$title <- scopus_ref[!is.na(book_title)]$book_title
scopus_ref <- scopus_ref[, -c("article_title","book_title")]

#' We now pass the table in long format with authors in rows and not in columns
#' 
scopus_ref <- scopus_ref %>%
  pivot_longer(cols = contains("author"), names_to = "order", values_to = "author") %>% 
  mutate(order = str_remove(order, "author_")) %>% 
  filter(!is.na(author) | order == 1) %>% # we want to avoid removing rows where we have no author (not the case here)
  select (temp_idref, temp_id, author, Year, title, journal, volume, numero, pages, order, article, references) %>% # We want to reorder a bit
  as.data.table()

#' ### Correcting errors
#' 

scopus_ref[author == "Belassa"]$author <- "Balassa"

#' ### saving the result
#' 
#' We save two versions:
#' 
#' - One with all the authors (just in case)
#' - One with just the first author, as we have in WoS.

scopus_ref <- scopus_ref[! temp_id %in% scopus_art_1970]
saveRDS(scopus_ref, paste0(eer_data,"scopus_references_extended.RDS"))
saveRDS(scopus_ref[order == 1], paste0(eer_data,"scopus_references.RDS"))
