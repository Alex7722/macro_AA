#' ---
#' title: "Script for cleaning the affiliations data"
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
#' This script aims at cleaning scopus data for affiliations. It mainly aims at
#' avoiding doublons with different names.
#' 
#' In the last part of the script, we categorize institutions (Academic, International Organization,
#' Private, _etc._)
#'
#+ r setup, include = FALSE
knitr::opts_chunk$set(eval = FALSE)

#' # Loading packages, paths and data
#'

source("~/macro_AA/functions/functions_for_cleaning_strings.R")
source("~/macro_AA/EER_Paper/Script_paths_and_basic_objects_EER.R")
Institutions <- readRDS(paste0(data_path, "EER/1_Corpus_Prepped_and_Merged/Institutions.rds"))
Corpus <- readRDS(paste0(data_path, "EER/1_Corpus_Prepped_and_Merged/Corpus.rds"))


#' # Cleaning institutions name
#' 
#' ## Cleaning Scopus data
#' 
#' #' We clean before by hand some institutions for which there are problem that will prevent them 
#' for being automatically merged with WoS institutions.

Institutions[str_detect(Institution, "UNIVERSITY OF CALIFORNIA")]$Institution <- c("UNIV-CALIF-DAVIS","UNIV-CALIF-BERKELEY") # checked manually
Institutions[Pays == "UNITED STATES"]$Pays <- "USA"
#' The first thing to do is to transform scopus institutions names in a way that allow them
#' to be merged and cleaned with the WoS institutions. It involves some hands cleaning
#' for some institution. 

#Institutions[str_detect(Institution, "WORLD BANK")]$Institution <- "WORLD-BANK"

scopus_to_clean <- data.table("Institution" = Institutions[str_detect(ID_Art, "S") & !Institution %in% unique(Institutions[!str_detect(ID_Art, "S")]$Institution)]$Institution)
scopus_to_clean[, clean_name := str_replace_all(Institution, " ","-")]
scopus_to_clean[, clean_name := str_replace_all(clean_name, "UNIVERSITY|UNIVERSITÉ","UNIV")]
scopus_to_clean[, clean_name := str_replace_all(clean_name, "-OF-|-DE-","-")]

#' Here is a bit of hand cleaning for some scopus institutions
scopus_to_clean[str_detect(clean_name, "TEL-AVIV")]$clean_name <- "TEL-AVIV-UNIV"
scopus_to_clean[str_detect(clean_name, "CENTER-PLANNING-AND-ECONOMIC-RESEARCH")]$clean_name <- "CTR-PLANNING-&-ECON-RES"
scopus_to_clean[str_detect(clean_name, "CENTRAL-PLANNING-BUREAU")]$clean_name <- "CENT-PLANNING-BUR"
scopus_to_clean[str_detect(clean_name, "BOWLING-GREEN-")]$clean_name <- "BOWLING-GREEN-STATE-UNIV"
scopus_to_clean[str_detect(clean_name, "ECONOMIC-AND-SOCIAL-RESEARCH-INSTITUTE")]$clean_name <- "ECON-&-SOCIAL-RES-INST"
scopus_to_clean[str_detect(clean_name, "HUNGARIAN-ACADEMY-SCIENCES-BUDAPEST")]$clean_name <- "HUNGARIAN-ACAD-SCI"
scopus_to_clean[str_detect(clean_name, "INDUSTRIAL-INSTITUTE-SOCIAL-AND-ECONOMIC-RESEARCH")]$clean_name <- "IND-INST-ECON-&-SOCIAL-RES"
scopus_to_clean[str_detect(clean_name, "RESOURCES-FOR-THE-FUTURE,-INC")]$clean_name <- "RESOURCES-FUTURE-INC"
scopus_to_clean[str_detect(clean_name, "INSTITUTE-ECONOMIC-STUDIES")]$clean_name <- "INST-ECON-STUDIES"
scopus_to_clean[str_detect(clean_name, "ASPIRANT-F.N.R.S.")]$clean_name <- "FNRS"
scopus_to_clean[str_detect(clean_name, "SCHOOL-ECONOMICS")]$clean_name <- "ERASMUS-UNIV-ROTTERDAM"

#' We now merge with the main data table for each value
for(i in seq_along(scopus_to_clean$Institution)){
  Institutions[Institution == scopus_to_clean$Institution[i]]$Institution <- scopus_to_clean$clean_name[i] 
}

#' ## Counting institutions
#' 
#' The first thing we can do is to count the number of times an institution and to spot problems:
#' `unique(Institutions[, n := .N, by = "Institution"][n > 1, c("Institution","n")][order(-n)])`
#' A first interesting thing is when you are keeping the countries: you see that the CEPR, for instance,
#' is associated to different countries, because it is often the second affiliation of an author,
#' and what is kept is his/her country. For the rest of the analysis, we remove `Pays`.
#' 
#' ## Unifying the names of institutions
#'
#' Some simplifications are employed in what follows. Many times, it appears that some affiliations
#' are actually two different institutions. In a first approximation, it will be simplified by
#' selecting one of the two institutions, but perhaps if will be refined later.
#'
#' A tricky case we know: Louvain. The university of Louvain has been splitted in 
#' two in 1968 (hopefully before the beginning of
#' our period). Louvain is used for the French-speaking university, Leuven for the Dutch-language
#' one. The problem is that we have many different names, we thus need to use two unique names
#' (one for Louvain, one for Leuven).
#' 
#' We also transform all the different names of the CORE center in UC-LOUVIN

Institutions[str_detect(Institution, "LOUVAIN|^CORE$|CTR-OPERAT") &
               !str_detect(Institution, "MANAGEMENT")]$Institution <- "UNIV-LOUVAIN"
Institutions[str_detect(Institution, "LEUVEN|KUL")]$Institution <- "UNIV-LEUVEN"

#' There is a similar problem in Brussels, with the Université libre de Bruxelles and the Vrije
#' Universiteit Brussel. In the database, we have the ULB and the Free-Univ-Brussels. All the cases 
#' we have checked for the second ambiguous name are from the ULB. In doubt, we will use the 
#' English name. We put the Vrije university with them.
#' 
#' We also merge the "ECARE" research center which is in ULB

Institutions[str_detect(Institution, "BRUSSELS|BRUXELLES|^ECARE$") &
               !str_detect(Institution, "ST-LOUIS")]$Institution <- "FREE-UNIV-BRUSSELS"

#' There are different names for the NBER and CEPR
#' 

Institutions[str_detect(Institution, "NATL-BUR-ECON-RES")]$Institution <- "NBER"
Institutions[str_detect(Institution, 
                        "CTR-ECON-POLICY-RES|CTR-ECON-POLICY-&-RES")]$Institution <- "CEPR"

#' Same for the London School of Economics

Institutions[str_detect(Institution, 
                        "UNIV-LONDON-LONDON-SCH-ECON-&-POLIT-SCI|LONDONS-SCH-ECON|LONDON-SCH-ECON")]$Institution <- "LSE"

#' Same for the IZA institution

Institutions[str_detect(Institution, 
                        "^IZA|INST-LABOR-ECON|INST-STUDY-LABOR")]$Institution <- "IZA"

#' STATEC is the name of the Luxemburg's "INSEE" so we exclude it and we harmonize INSEE's names
Institutions[str_detect(Institution, "INSEE|INST-NATL-STAT") &
                          !str_detect(Institution, "STATEC")]$Institution  <- "INSEE" 

#' We investigate the issue of the EHESS and ENS. One problem is the DELTA, which is recurrent, but is the merging
#' of the EHESS and ENS. By convention, we will classify it as EHESS, except when there is a precision (like 
#' "DELTA-ENS")

Institutions[str_detect(Institution, 
                        "EHESS|DELTA|GREMAQ|ECOLE-HAUTES-ETUD-SCI-SOCIALES|CTR-ECON-QUANTITAT-&-COMPARAT|INST-ADV-STUDIES-SOCIAL-SCI|MAISON-SCI-HOMME|ECOLE-HAUTES-ETUDE-SCI-SOCIALES") &
               !str_detect(Institution, "ENS$")]$Institution <- "EHESS"
Institutions[str_detect(Institution, "DELTA-ENS|ENS-ULM|ECOLE-NORMALE-SUPER")]$Institution <- "ENS"
Institutions[str_detect(Institution, "ECOLE-HAUTES-ETUDES-COMMERCIALES|-HEC$|HEC-PARIS")]$Institution <- "HEC"
Institutions[str_detect(Institution, "OFCE-")]$Institution <- "OFCE"
Institutions[str_detect(Institution, "^SCI-PO")]$Institution <- "SCIENCES-PO"

#' We also simply different institutions:
Institutions[str_detect(Institution, "CTR-ETUD-PROSPECT-ECON-")]$Institution <- "CEPREMAP"
Institutions[str_detect(Institution, "ENSAI")]$Institution <- "ENSAI"
Institutions[str_detect(Institution, "CREST-")]$Institution <- "CREST"
Institutions[str_detect(Institution, "COPENHAGEN-") &
               str_detect(Institution, "BUSINESS")]$Institution <- "COPENHAGEN-BUSINESS-SCH"
Institutions[str_detect(Institution, "TELECOM|^ENST")]$Institution <- "TELECOM-PARISTECH"
Institutions[str_detect(Institution, "PSE|PARIS-SCH-ECON")]$Institution <- "PARIS-SCH-ECON"
Institutions[str_detect(Institution, "^S-BANK|POLYTECH-S-BANK")]$Institution <- "S-BANK-UNIV"
Institutions[str_detect(Institution, "EUROPEAN-INVESTMENT-BANK|EUROPEAN-INVEST-BANK")]$Institution <- "EUROPEAN-INVESTMENT-BANK"
Institutions[str_detect(Institution, "INTERAMER-DEV-BANK|INTER-AMER-DEV-BANK")]$Institution <- "INTERAMER-DEV-BANK"
Institutions[str_detect(Institution, "NATL-BANK-NEW-ZEALAND")]$Institution <- "NATL-BANK-NEW-ZEALAND"
Institutions[str_detect(Institution, "BANK-INT-SETTLEMENTS")]$Institution <- "BANK-INT-SETTLEMENTS"
Institutions[str_detect(Institution, "CESIFO|CES-IFO")]$Institution <- "CESIFO"
Institutions[str_detect(Institution, "EUROPEAN-BANK-RECONSTRUCT-&-DEV")]$Institution <- "EBRD"
Institutions[str_detect(Institution, "AUSTRIAN-INST-ECON-RES")]$Institution <- "AUSTRIAN-INST-ECON-RES"
Institutions[str_detect(Institution, "NETHERLANDS-BUR-ECON-POLICY-ANAL")]$Institution <- "CENT-PLANNING-BUR"
Institutions[str_detect(Institution, "-ZEW")]$Institution <- "ZEW"
Institutions[str_detect(Institution, "DIW-|-DIW")]$Institution <- "DIW"
Institutions[str_detect(Institution, "ECOLE-NATL-STAT-")]$Institution <- "ENSAE"
Institutions[str_detect(Institution, "^ENPC$|ECOLE-NATL-PONTS-&-CHAUSSEE")]$Institution <- "ECOLE-NATL-PONTS-&-CHAUSSEES"
Institutions[str_detect(Institution, "^IFO-")]$Institution <- "IFO-INST"
Institutions[str_detect(Institution, "^IFS$")]$Institution <- "INST-FISCAL-STUDIES"
Institutions[str_detect(Institution, "INST-EMPLOYMENT-RES|^IAB$|^IAB-")]$Institution <- "INST-EMPLOYMENT-RES-IAB"
Institutions[str_detect(Institution, "INST-WORLD-ECON")]$Institution <- "KIEL-INST-WORLD-ECON"
Institutions[str_detect(Institution, "ORG-ECON-COOPERAT-&-DEV|OECD-")]$Institution <- "OECD"
Institutions[str_detect(Institution, "RES-INST-IND-ECON")]$Institution <- "RES-INST-IND-ECON-IFN"
Institutions[str_detect(Institution, "INST-ECON-RES")]$Institution <- "WIFO"
Institutions[str_detect(Institution, "WZB-|WISSENSCHAFTSZENTRUM|SOCIAL-SCI-RES-CTR-BERLIN|WISSENSCH-")]$Institution <- "WZB"
Institutions[str_detect(Institution, "GATT-|GEN-AGREEMENT")]$Institution <- "GATT"
Institutions[str_detect(Institution, "GOLDMAN-SACHS")]$Institution <- "GOLDMAN-SACHS"
Institutions[str_detect(Institution, "INT-MONETARY-FUND|INTERNATIONAL-MONETARY-FUND")]$Institution <- "IMF"
Institutions[str_detect(Institution, "PIRAEUS-")]$Institution <- "PIRAEUS-GRAD-SCH-IND-STUDIES"

#' We now want to clean the central banks names. We first list the names of central banks (it will
#' be useful later to classify them), and check in the list when there are multiple names for one
#' institution

cb <- c("FED-RESERVE",
        "CENT-BANK",
        "FED-RES-BANK",
        "FEDERAL-RESERVE",
        "BANK-ENGLAND",
        "BANK-ITALY",
        "BANK-CANADA",
        "BANK-ISRAEL",
        "RESERVE-BANK",
        "BANK-JAPAN",
        "^BANK-GREECE",  # add this to avoid the agricultural Bank "AGR-BANK-GREECE"
        "BANK-FINLAND",
        "BANK-SPAIN",
        "NEDERLANDSCHE-BANK",
        "BANK-BELGIUM",
        "SVERIGES-RIKSBANK",
        "STERIGES-RIKSBANK", # coquille?
        "SWISS-NATL-BANK",
        "BANCO-MEXICO",
        "BANCO-ESPANA",
        "BANCO-PORTUGAL",
        "BANCA-ITALIA",
        "FRB-",
        "BUNDESBANK",
        "DEUTSCHE-BUDESBANK", # coquille
        "NORGES-BANK",
        "BOARD-GOVERNORS",
        "BANQUE-FRANCE",
        "CZECH-NATL-BANK",
        "BANK-FRANCE",
        "BANK-PORTUGAL",
        "SUFTUNG-SCHWEIZERISCHEN-NATL-BANK", # BC suisse
        "BANK-CHILE",
        "NATL-BANK-HUNGARY",
        "OESTERREICH-NATL-BANK",
        "BANK-NORWAY",
        "BANK-THAILAND",
        "-NATIONALBANK", 
        "DANMARKS-NATL-BANK",
        "MONETARY-AUTHOR-") # For Singapore and Hong Kong 

#' To check the potential doublons, we run 
#' `unique(Institutions[str_detect(Institution, paste0(cb, collapse = "|"))]$Institution)`. Then
#' we can make corrections.

Institutions[str_detect(Institution, "BANQUE-FRANCE|BANK-FRANCE")]$Institution <- "BANQUE-FRANCE"
Institutions[str_detect(Institution, "BANCO-ESPANA|BANK-SPAIN")]$Institution <- "BANCO-ESPANA"
Institutions[str_detect(Institution, "BANCA-ITALIA|BANK-ITALY")]$Institution <- "BANCA-ITALIA"
Institutions[str_detect(Institution, "BANCO-PORTUGAL|BANK-PORTUGAL")]$Institution <- "BANCO-PORTUGAL"
Institutions[str_detect(Institution, "NEDERLANDSCHE-BANK")]$Institution <- "NEDERLANDSCHE-BANK"
Institutions[str_detect(Institution, "FED-RESERVE-SYST|FED-RESERVE-BOARD")]$Institution <- "FED-RESERVE-BOARD"
Institutions[str_detect(Institution, "SWISS-NATL-BANK|SUFTUNG-SCHWEIZERISCHEN-NATL-BANK")]$Institution <- "SWISS-NATL-BANK"
Institutions[str_detect(Institution, "-BUNDESBANK|BUDESBANK")]$Institution <- "DEUTSCHE-BUNDESBANK"
Institutions[str_detect(Institution, "BANK-BELGIUM")]$Institution <- "BANK-BELGIUM"
Institutions[str_detect(Institution, "STERIGES-RIKSBANK")]$Institution <- "SVERIGES-RIKSBANK"
Institutions[str_detect(Institution, "BANK-CHILE")]$Institution <- "CENT-BANK-CHILE"
Institutions[str_detect(Institution, "EUROPEAN-CENT-BANK")]$Institution <- "EUROPEAN-CENT-BANK"
Institutions[str_detect(Institution, "BANK-NORWAY")]$Institution <- "NORGES-BANK"
Institutions[str_detect(Institution, "OESTERREICH-NAT")]$Institution <- "OESTERREICH-NATIONALBANK"
Institutions[str_detect(Institution, "DANMARKS-NATL-BANK")]$Institution <- "DANMARKS-NATIONALBANK"

#' We simplify and harmonise FRB central Banks name:
#' 
Institutions <- Institutions %>% 
  mutate(Institution = str_replace(Institution,"FED-RESERVE-BANK-","FRB-"))

Institutions[str_detect(Institution, "FED-RESERVE-BANK")]$Institution <- "FRB-NEW-YORK" # Manual check

#' We now clean british colleges in university. First, we take the affiliation where you have both
#' the university and the college. It allows us to have a list of the colleges per university.
#' We can identify the list of affiliations with a college here, and spot the universities with several
#' college.
#' 
colleges <- unique(Institutions[str_detect(Institution, "-COLL")]$Institution)
uk_univ <- c("UNIV-LONDON",  # we add a dash as it is necessary in our function to extract the college name
             "UNIV-OXFORD",
             "UNIV-CAMBRIDGE")
list_uk_univ <- list()
for(i in uk_univ){
  list_uk_univ[[i]] <- extract_college(colleges, paste0(i,"-"))
}

#' We now remove the name of the colleges. This is a big choice for the London University as it is very vast
#' and we use to rather refer to "Queen Mary" or "Birkbeck". But as sometimes you just have "UNIV-LONDON", it
#' seems the better approximation for now.
Institutions[str_detect(Institution, "OXFORD")]$Institution <- "UNIV-OXFORD"
Institutions[str_detect(Institution, "CAMBRIDGE")]$Institution <- "UNIV-CAMBRIDGE"
Institutions[str_detect(Institution, "UNIV-LONDON")]$Institution <- "UNIV-LONDON" 

#' We reextract the list of colleges, now that we have cleaned the name of universities,
#' using `unique(Institutions[str_detect(Institution, "-COLL")]$Institution) %>% sort()`
#' and we try to identify the remaining college names with the list we have identified 
#' above.
#'
#' We can do a bit of cleaning on this basis
#' 
Institutions[str_detect(Institution, "UNIV-COLL-LONDON")]$Institution <- "UNIV-LONDON"
Institutions[str_detect(Institution, "TRINITY-COLL|UNIV-COLL-DUBLIN") &
               str_detect(Institution, "DUBLIN")]$Institution <- "UNIV-DUBLIN"
Institutions[str_detect(Institution, "SWANSEA")]$Institution <- "UNIV-SWANSEA"
Institutions[str_detect(Institution, "COLUMBIA-UNIV")]$Institution <- "COLUMBIA-UNIV"

#' We add the missing colleges in the list
list_uk_univ[["UNIV-OXFORD"]] <- append(list_uk_univ[["UNIV-OXFORD"]], c("BALLIOL-COLL","HARRIS-MANCHESTER-COLL"))
list_uk_univ[["UNIV-LONDON"]] <- append(list_uk_univ[["UNIV-LONDON"]], c("IMPERIAL-COLL-LONDON","KINGS-COLL-LONDON"))
list_uk_univ[["UNIV-CAMBRIDGE"]] <- append(list_uk_univ[["UNIV-CAMBRIDGE"]], c("MAGDALENE-COLL"))

#' There is one doublon in Oxford and Cambridge, "Saint-Johns college" that we remove and inspect.

Institutions[Institution == "ST-JOHNS-COLL"]$Institution <- "UNIV-CAMBRIDGE" # check manually

#' We recompute the colleges after the cleaning and we match the remaining names when they are from
#' one of the big UK university
colleges <- unique(Institutions[str_detect(Institution, "-COLL")]$Institution) %>% sort()

for(i in seq_along(list_uk_univ)){
Institutions[str_detect(Institution, 
                        paste0(colleges[colleges %in% list_uk_univ[[i]]], collapse = "|"))]$Institution <- names(list_uk_univ)[i]
}

#' ## Cleaning different university names which are spelled differently
#' 
Institutions[str_detect(Institution, "STOCKHOLM-SCH-ECON")]$Institution <- "STOCKHOLM-SCH-ECON"
Institutions[str_detect(Institution, "STOCKHOLM") &
               str_detect(Institution, "UNIV")]$Institution <- "UNIV-STOCKHOLM"
Institutions[str_detect(Institution, "FREE|VRIJE") &
               str_detect(Institution, "AMSTERDAM")]$Institution <- "FREE-UNIV-AMSTERDAM"
Institutions[str_detect(Institution, "BOCCONI|IGIER")]$Institution <- "UNIV-BOCCONI"
Institutions[str_detect(Institution, "LEIDEN|LEYDEN")]$Institution <- "UNIV-LEIDEN"
Institutions[(str_detect(Institution, "TOULOUSE") &
               str_detect(Institution, "CAPITOLE|IDEI")) |
               str_detect(Institution, "^IDEI$")]$Institution <- "UNIV-TOULOUSE-1"
Institutions[str_detect(Institution, "ERASMUS-")]$Institution <- "ERASMUS-UNIV-ROTTERDAM"
Institutions[str_detect(Institution, "^GEP$")]$Institution <- "UNIV-NOTTINGHAM"
Institutions[str_detect(Institution, "GREQAM")]$Institution <- "AIX-MARSEILLE-UNIV" # That is an arbitrary choice as it could have been the EHESS too
Institutions[str_detect(Institution, "INST-INT-ECON-STUDIES")]$Institution <- "UNIV-STOCKHOLM"
Institutions[str_detect(Institution, "IOWA-STATE-UNIV")]$Institution <- "IOWA-STATE-UNIV"
Institutions[str_detect(Institution, "LUDWIG-MAXIMILIAN")]$Institution <- "LUDWIG-MAXIMILIAN-UNIV-MUNICH"
Institutions[str_detect(Institution, "NORWEGIAN-SCH-ECON")]$Institution <- "NORWEGIAN-SCH-ECON"
Institutions[str_detect(Institution, "RAGNAR-FRISCH-CTR-ECON-RES")]$Institution <- "UNIV-OSLO"
Institutions[str_detect(Institution, "RAGNER-FRISCH-CTR-ECON-RES")]$Institution <- "UNIV-OSLO" # Typo
Institutions[str_detect(Institution, "^ETH$")]$Institution <- "SWISS-FED-INST-TECHNOL"
Institutions[str_detect(Institution, "^THEMA^")]$Institution <- "UNIV-CERGY-PONTOISE"
Institutions[str_detect(Institution, "VIENNA-UNIV-ECON-&-BUSINESS")]$Institution <- "VIENNA-UNIV-ECON-&-BUSINESS"
Institutions[str_detect(Institution, "UQAM")]$Institution <- "UNIV-QUEBEC"
Institutions[str_detect(Institution, "^EUI$")]$Institution <- "EUROPEAN-UNIV-INST"
Institutions[str_detect(Institution, "BARCELONA-GSE")]$Institution <- "UNIV-POMPEU-FABRA"
Institutions[str_detect(Institution, "U-AUTONOMA-BARCELONA")]$Institution <- "UNIV-AUTONOMA-BARCELONA"
Institutions[str_detect(Institution, "UNIV-OREGON")]$Institution <- "UNIV-OREGON"

#' We have spotted that sometimes "UNIV" is at the beginning or at the end. The strategy is:
#' 
#' - listing all the university with the pattern "UNIV-NAME" or "NAME-UNIV"
#' - extracting the NAME and rewriting as "UNIV-NAME"
#' - checking when the original name is different from the one we have rebuilt.
#' - checking for potential conflicts (like YORK-UNIV for Canada and UNIV-YORK for England, already
#' problematic in the database)
#' - changing names in the data table.
correct_univ <- data.table("Institution" = c(unique(Institutions[str_detect(Institution, "^UNIV-[A-z]{1,}$")]$Institution),
                                           unique(Institutions[str_detect(Institution, "^[A-z]{1,}-UNIV$")]$Institution)))
correct_univ[, correct_name := paste0("UNIV-",str_remove(Institution, "UNIV-|-UNIV"))]
correct_univ <- correct_univ[Institution != correct_name] %>% unique()

#' We check for the correct names that were already in the database to have an idea of potential problems.
check_inst <- data.table("ID_Art" = c(),"Institution" = c(), "Pays" = c())
for(i in seq_along(correct_univ$Institution)){
inst <- Institutions[Institution == correct_univ$correct_name[i]]
inst$iteration <- i
check_inst <- rbind(check_inst, inst)
}

#' The only conflict is with the two university of York, and we distinguish the one in Canada.
Institutions[Institution == "YORK-UNIV" & Pays == "CANADA"]$Institution <- "YORK-UNIV-TORONTO"


for(i in seq_along(correct_univ$Institution)){
  Institutions[Institution == correct_univ$Institution[i]]$Institution <- correct_univ$correct_name[i] 
}

#' # Cleaning countries and by country
#' 
#' We have spotted some problems with some names which are linked to different countries. We thus 
#' do a bit of cleaning on them, to distinguish the institutions depending on the country.
Institutions[Institution == "IND-INST-ECON-&-SOCIAL-RES"]$Pays <- "SWEDEN"
Institutions[Pays == "YOGUSLAVIA"]$Pays <- "YUGOSLAVIA"
Institutions[Institution == "INST-ECON" &
               Pays == "YUGOSLAVIA"]$Institution <- "INST-ECON-STUDIES"
Institutions[Institution == "UCL" &
               Pays == "ENGLAND"]$Institution <- "UNIV-LONDON" # This is due to the fact that we have put all the London colleges together
Institutions[Institution == "UCL" &
               Pays == "BELGIUM"]$Institution <- "UNIV-LOUVAIN"


#' # Classifying institutions type
#' 

inst_type <- data.table("Institution" = unique(Institutions$Institution),
                        "Type" = rep(x = "NA", times = length(unique(Institutions$Institution))))

#' We list first all the universities without the "UNIV" part in their names, and then we classify
#' them as universities, with the institutions with "UNIV".
university <- c("MIT",
                "NYU",
                "CNRS",
                "FNRS",
                "CSIC",
                "PARIS-SCH-ECON",
                "EHESS",
                "ENS",
                "TELECOM-PARISTECH",
                "LSE",
                "CREST",
                "ECOLE-NATL-PONTS-&-CHAUSSEES",
                "SWISS-FED-INST-TECHNOL",
                "UQAM",
                "SCIENCES-PO",
                "POLYTEC",
                "VIRGINIA-TECH",
                "US-NAVAL-ACAD",
                "CALTECH",
                "CONSERVATOIRE-NATL-ARTS-&-METIERS",
                "CERGE-EI",
                "SUNY-ALBANY")

inst_type[str_detect(Institution, "UNIV") | Institution %in% university]$Type <- "Academic"
inst_type[str_detect(Institution, "COLL|ECOLE|COLEGIO|FAC-|ACAD-") &
            !str_detect(Institution, "INST")]$Type <- "Academic"

#' Same here but for all the networks and institutions without "INST" in their name and then
network <- c("CEPR", 
             "NBER",
             "IZA",
             "CESIFO",
             "INSEAD",
             "INSEE",
             "CERAS",
             "ENSAE",
             "ENSAI",
             "CEPREMAP",
             "CEMFI",
             "INRA",
             "CEPII",
             "ZEW",
             "DIW",
             "FDN-MATTEI",
             "FEDEA",
             "IFN",
             "IVIE",
             "WIFO",
             "WZB",
             "CTR-EUROPEAN-POLICY-STUDIES",
             "LONDON-ENVIRONM-ECON-CTR",
             "OFCE",
             "CIRANO",
             "NESTPAR",
             "RAND-CORP")

inst_type[(str_detect(Institution, "INST|FDN-|-FDN") |
            Institution %in% network) &
            Type != "Academic"]$Type <- "Institute & Network"

#' We identify the business schools:

bs <- c("STOCKHOLM-SCH-ECON",
        "ATHENS-SCH-ECON-&-BUSINESS-SCI",
        "NORWEGIAN-SCH-ECON",
        "ESSEC",
        "HEC")
inst_type[(str_detect(Institution, "MANAGEMENT|BUSINESS") &
             str_detect(Institution, "SCH")) |
             Institution %in% bs]$Type <- "Business School"

#' We try to put non yet attributed institution in the university category
inst_type[(str_detect(Institution, "ECON|GRAD") & 
            str_detect(Institution, "SCH")) &
             Type != "Business School"]$Type <- "Academic"
inst_type[(str_detect(Institution, "ECON|CTR|CENTER") & 
             str_detect(Institution, "RES|")) &
            Type == "NA"]$Type <- "Academic"

#' We identify the central banks:
#' 

inst_type[str_detect(Institution, paste0(cb, collapse = "|"))]$Type <- "Central Bank"

#' We identify the international organisations
io <- c("WORLD-BANK",  # We list here all the international organisations
        "IMF",
        "BANK-INT-SETTLEMENTS",
        "EBRD",
        "EUROPEAN-INVESTMENT-BANK",
        "INT-BANK-RECONSTRUCTION-&-DEV",
        "ASIAN-DEV-BANK",
        "CAF-DEV-BANK-LATIN-AMER",
        "INTERAMER-DEV-BANK",
        "OECD",
        "COMMISS-EUROPEAN-COMMUNITIES",
        "EEC-COMMISS",
        "EUROPEAN-COMMISS",
        "GATT",
        "INT-LABOR-ORG",
        "UN-ECON-COMMISS-EUROPE",
        "UNCTAD",
        "UNITED-NATIONS-IND-DEV-ORG")

inst_type[Institution %in% io]$Type <- "International Organization"

#' We identify national public organization
#' 

npo <- c("SWISS-FED-BANKING-COMMISS",
         "CENT-PLANNING-BUR",
         "ISRAEL-CENT-BUR-STAT",
         "SERV-ETUD-ECON-&-FINANCIERES",
         "ECON-PLANNING",
         "FED-PLANNING",
         "NATL-PLANNING",
         "GERMAN-FED-EMPLOYMENT-AGCY-BA",
         "GOVT-ONTARIO",
         "STAT-CANADA",
         "STAT-NORWAY",
         "CENT-BUR-STAT")

inst_type[str_detect(Institution, "MINIST|TREASURY|US-BUR-|US-DEPT-") | Institution %in% npo]$Type <- "National Public Organization"

#' We now tackles the private firms, and first the banks that remain after 
#' identifying central banks

private <- c("GOLDMAN-SACHS",
             "COOPERS-&-LYBRAND",
             "EMERGING-MARKETS-GRP",
             "GIBBS-QUANTITAT-RES-&-CONSULTING",
             "HAGUE-CONSULTING-GRP",
             "GOOGLE-INC",
             "JP-MORGAN-SECUR-LTD",
             "KPMG-PEAT-MARWICK-LLP",
             "MCKINSEY-&-CO",
             "WHARTON-ECON-FORECASTING-ASSOC",
             "SHEARSON-LOEB-RHOADES-INC")
inst_type[(str_detect(Institution, "BANK|MANAGEMENT") | Institution %in% private) & Type == "NA"]$Type <- "Private"

#' We now merge the types we have identified with the main data table.
#' 
inst_type[Institution == "NULL"]$Institution <- "NA"
Institutions <- merge(Institutions,inst_type, by = "Institution")

#' Some cleaning on `countries_grouped`
#' 
Institutions[Pays == "SWEDEN",]$Countries_grouped <- "Europe"

#' We finally save the new database under a different name
#' 
saveRDS(Institutions,paste0(data_path, "EER/1_Corpus_Prepped_and_Merged/Institutions_cleaned.rds"))
