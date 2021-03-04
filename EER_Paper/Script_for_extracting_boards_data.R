# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################## LOADING PACKAGES, PATHS AND OBJECTS ####################################--------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

source("./EER_Paper/Script_paths_and_basic_objects_EER.R")


# Loading the .txt of editorial Boards (EB)----------
list_txt <- list.files(path = boards_path)

EB <- readtext(paste0(boards_path, list_txt))
EB <- EB %>% mutate(Year = str_extract(doc_id, "[0-9]{4}"))

# crating an empty table to put the data at the end of the loop
Members_EB <- data.table("Name" = c(), "Initials" = c(), "Surname" = c(), "Institution" = c(), "Country" = c(), "Year" = c())

# loop for 1 to 5

for (i in 1:5) {
  # Taking the text
  text <- EB$text[i]

  # Separating Lines
  text <- strsplit(text, "\n")

  # Deleting the titles of the individuals at the beginning of the text
  text <- gsub("^MR", "", text[[1]])
  text <- gsub("^DR", "", text)
  text <- gsub("^PROF", "", text)
  text <- gsub("^Prof", "", text)

  # Deleting space and points at the beginning of the text
  text <- gsub("^\\.", "", text)
  text <- gsub("^ ", "", text)

  # Changing names of countries with two words, to one word
  text <- gsub("The Netherlands", "Netherlands", text)
  text <- gsub("Tie Netherlands", "Netherlands", text)
  text <- gsub("United Kingdom", "UK", text)

  # Reestablishing standard brackets
  text <- gsub("\\{", "\\(", text)
  text <- gsub("\\}", "\\)", text)

  # Creating a table to put all the relevant information
  # extracting first initial, putting the year
  data_text <- data.table("Initials_1" = str_extract(text, "^\\w{1,}\\."), "text" = gsub("^\\w{1,}\\.", "", text), "Year" = EB$Year[i])
  data_text <- data_text[, text := gsub("^ ", "", text)]
  data_text <- data_text[, text := gsub(" $", "", text)]

  # extracting the following initials
  data_text <- data_text[, `:=`(Initials_2 = str_extract(text, "^\\w{1,}\\."), text = gsub("^\\w{1,}\\.", "", text))][, text := gsub("^ ", "", text)]
  data_text <- data_text[, `:=`(Initials_3 = str_extract(text, "^\\w{1,}\\."), text = gsub("^\\w{1,}\\.", "", text))][, text := gsub("^ ", "", text)]

  # extracting the surname, the country, and what is lefting is the institution
  data_text <- data_text[, `:=`(Surname = gsub("[.,].*", "", text), text = str_extract(text, "[.,].*"))][, text := gsub("[.,]", "", text)][, text := gsub("^ ", "", text)]
  data_text <- data_text[, `:=`(Surname = gsub("([\\(\\<]\\w+)\\)$", "", Surname), Country = str_extract(text, "(\\(\\w+)\\)$"), Institution = gsub("(\\(\\w+)\\)$", "", text))]

  # concatening initials
  data_text <- data_text[, Initials := Initials_1][Initials_2 != "NA", Initials := paste(Initials, Initials_2)][Initials_3 != "NA", Initials := paste(Initials, Initials_3)][, Initials := gsub("\\.", "", Initials)][, Initials := gsub(" ", "", Initials)]

  # reorganizing table and cleaning
  data_text <- data_text[, Name := paste(Surname, "-", Initials)][, c("Name", "Initials", "Surname", "Institution", "Country", "Year")][, Name := gsub(" ", "", Name)]
  data_text <- data_text[, Country := gsub("[\\(\\)\\{\\}]", "", Country)]

  # adding to the table created before
  Members_EB <- rbind(Members_EB, data_text)
}


# loop for 6 to 17
for (i in c(6:17, 19, 23, 26:28)) {
  # Taking the text
  text <- EB$text[i]

  # Separating Lines
  text <- strsplit(text, "\n")

  # Deleting the titles of the individuals at the beginning of the text
  text <- gsub("^MR", "", text[[1]])
  text <- gsub("^DR", "", text)
  text <- gsub("^PROF", "", text)
  text <- gsub("^Prof", "", text)

  # Deleting space and points at the beginning of the text
  text <- gsub("^\\.", "", text)
  text <- gsub("^ ", "", text)

  # Changing names of countries with two words, to one word
  text <- gsub("The Netherlands", "Netherlands", text)
  text <- gsub("Tie Netherlands", "Netherlands", text)
  text <- gsub("United Kingdom", "UK", text)
  text <- gsub("U K", "UK", text)
  text <- gsub("United States", "US", text)

  # Reestablishing standard brackets
  text <- gsub("\\{", "\\(", text)
  text <- gsub("\\}", "\\)", text)


  # Creating a table to put all the relevant information
  # extracting first initial, putting the year
  data_text <- data.table("Initials_1" = str_extract(text, "^\\w{1,}\\."), "text" = gsub("^\\w{1,}\\.", "", text), "Year" = EB$Year[i])
  data_text <- data_text[, text := gsub("^ ", "", text)]

  # extracting the following initials
  data_text <- data_text[, `:=`(Initials_2 = str_extract(text, "^\\w{1,}\\."), text = gsub("^\\w{1,}\\.", "", text))][, text := gsub("^ ", "", text)]
  data_text <- data_text[, `:=`(Initials_3 = str_extract(text, "^\\w{1,}\\."), text = gsub("^\\w{1,}\\.", "", text))][, text := gsub("^ ", "", text)]

  # extracting surnames and countries
  data_text <- data_text[, `:=`(Surname = gsub("([\\(\\<]\\w+)[\\)\\}]$", "", text), Country = str_extract(text, "([\\(\\<]\\w+)[\\)\\}]$"))][, Surname := gsub(" ", "", Surname)]

  # concatening initials and cleaning
  data_text <- data_text[, Initials := Initials_1][Initials_2 != "NA", Initials := paste(Initials, Initials_2)][Initials_3 != "NA", Initials := paste(Initials, Initials_3)][, Initials := gsub("\\.", "", Initials)][, Initials := gsub(" ", "", Initials)]
  data_text$Institution <- "NA" # adding an institution column for compatibility with other samples
  data_text <- data_text[, Name := paste(Surname, "-", Initials)][, c("Name", "Initials", "Surname", "Institution", "Country", "Year")][, Name := gsub(" ", "", Name)]
  data_text <- data_text[, Country := gsub("[\\(\\)\\{\\}]", "", Country)]

  # adding to the table created before
  Members_EB <- rbind(Members_EB, data_text)
}

# loop for 18 to
for (i in c(18, 20:22, 24:25, 29:36)) {
  # Taking the text
  text <- EB$text[i]

  # Separating Lines
  text <- strsplit(text, "\n")

  # Deleting the titles of the individuals at the beginning of the text
  text <- gsub("^MR", "", text[[1]])
  text <- gsub("^DR", "", text)
  text <- gsub("^PROF", "", text)
  text <- gsub("^Prof", "", text)

  # Deleting space and points at the beginning of the text
  text <- gsub("^\\.", "", text)
  text <- gsub("^ ", "", text)

  # Changing names of countries with two words, to one word
  text <- gsub("The Netherlands", "Netherlands", text)
  text <- gsub("Tie Netherlands", "Netherlands", text)
  text <- gsub("United Kingdom", "UK", text)
  text <- gsub("U K", "UK", text)
  text <- gsub("United States", "US", text)

  # Reestablishing standard brackets
  text <- gsub("\\{", "\\(", text)
  text <- gsub("\\}", "\\)", text)

  # Creating a table to put all the relevant information
  # extracting first initial, putting the year
  data_text <- data.table("Initials_1" = str_extract(text, "^\\w{1,}\\."), "text" = gsub("^\\w{1,}\\.", "", text), "Year" = EB$Year[i])
  data_text <- data_text[, text := gsub("^ ", "", text)]
  data_text <- data_text[, text := gsub(" $", "", text)]

  # extracting the following initials
  data_text <- data_text[, `:=`(Initials_2 = str_extract(text, "^\\w{1,}\\."), text = gsub("^\\w{1,}\\.", "", text))][, text := gsub("^ ", "", text)]
  data_text <- data_text[, `:=`(Initials_3 = str_extract(text, "^\\w{1,}\\."), text = gsub("^\\w{1,}\\.", "", text))][, text := gsub("^ ", "", text)]

  # extracting the surname, the country, and what is lefting is the institution
  data_text <- data_text[, `:=`(Surname = gsub("[.,].*", "", text), text = str_extract(text, "[.,].*"))][, text := gsub("[.,]", "", text)][, text := gsub("^ ", "", text)]
  data_text <- data_text[, `:=`(Country = str_extract(text, "(\\w+)$"), Institution = gsub("(\\w+)$", "", text))]

  # concatening initials
  data_text <- data_text[, Initials := Initials_1][Initials_2 != "NA", Initials := paste(Initials, Initials_2)][Initials_3 != "NA", Initials := paste(Initials, Initials_3)][, Initials := gsub("\\.", "", Initials)][, Initials := gsub(" ", "", Initials)]

  # reorganizing table and cleaning
  data_text <- data_text[, Name := paste(Surname, "-", Initials)][, c("Name", "Initials", "Surname", "Institution", "Country", "Year")][, Name := gsub(" ", "", Name)]
  data_text <- data_text[, Country := gsub("[\\(\\)\\{\\}]", "", Country)]

  # adding to the table created before
  Members_EB <- rbind(Members_EB, data_text)
}

Members_EB$Type <- "Editorial Board"

write_csv(Members_EB, paste0(boards_path, "Members_EB.csv"))

#########  Cleaning the data ############
Members_EB <- read_csv(paste0(boards_path, "Members_EB.csv")) %>% as.data.table()


# Cleaning countries
wrong_country <- c(
  "Beigium", "Beleium", "Belgiui", "Cambridge", "Czechosiovakia",
  "Eaglana", "England", "Engiand", "Englarid", "Eire",
  "Franc$", "Francs", "Frencs", "FRG",
  "Holland", "hungary", "israel", "italy",
  "^SA$", "US$", "Stockholm", "TheNetherlands", "Yougoslavia"
)
right_country <- c(
  "Belgium", "Belgium", "Belgium", "UK", "Czechoslovakia",
  "UK", "UK", "UK", "UK", "Ireland",
  "France", "France", "France", "Germany",
  "Netherlands", "Hungary", "Israel", "Italy",
  "USA", "USA", "Sweden", "Netherlands", "Yugoslavia"
)

for (i in 1:length(right_country)) {
  Members_EB <- Members_EB[, Country := gsub(wrong_country[i], right_country[i], Country)]
}

# Cleaning Name
Members_EB <- Members_EB[, Name := toupper(Name)][, Name := gsub("É", "E", Name)][, Name := gsub("È", "E", Name)][, Name := gsub("É", "E", Name)][, Name := gsub("Ö", "O", Name)]

Members_EB$Year[which(Members_EB$Name == "RUTSTRÖM-EE")]


wrong_names <- c(
  "VONWEISZÄCKER-CC", "VONWEIZSACKER-CC", "VONWEIZSAECKER-CC", "ZIMMERMANN-K$",
  "0PPENLAENDER-KH", "ASHEIM-G$", "BARLEVI-G", "BARTEN-A$",
  "BÉNARD-J", "BESSIÉRE-F", "BESSIÈRE-F", "BOMBACH-R", "COURBIS-A",
  "DASSEI-E", "DASSEL-E", "DEATON-A$", "FCNTELA-E", "FONTELA-F",
  "FOURGEAUD-CI", "GEHRIT:-G", "GINSBURGH-Y", "GIEJSER-H", "GLEISER-H",
  "GRUSG\"!-C", "HORVAT-B", "LASUAN-JR", "LISLE-C", "LISLE-L", "KIAS-A",
  "MCFARGUHAR-AMM", "NEWBERY-D", "PAELINCK-JHF", "ROTHSCHILD-W",
  "RUTSTRÖM-EE", "RUTSTRÖM-EE", "RUTSTRÖM-EE", "WAELBROECKCK-J", "ZIMMERMANN-K$"
)
right_names <- c(
  "VONWEISZACKER-CC", "VONWEISZACKER-CC", "VONWEISZACKER-CC", "ZIMMERMANN-KF",
  "OPPENLAENDER-KH", "ASHEIM-GB", "BARLEVY-G", "BARTEN-AP",
  "BENARD-J", "BESSIERE-F", "BESSIERE-F", "BOMBACH-G", "COURBIS-R",
  "DASSEL-FE", "DASSEL-FE", "DEATON-AS", "FONTELA-E", "FONTELA-E",
  "FOURGEAUD-C", "GEHRIG-G", "GINSBURGH-V", "GLEJSER-H", "GLEJSER-H",
  "GRUSON-C", "HORVAT-BR", "LASUEN-JR", "LISLE-E", "LISLE-E", "KLAS-A",
  "MCFARQUHAR-AMM", "NEWBERY-D", "PAELINCK-JHP", "ROTHSCHILD-KW",
  "RUTSTROM-EE", "RUTSTROM-EE", "RUTSTROM-EE", "WAELBROECK-J"
)

for (i in 1:length(right_country)) {
  Members_EB <- Members_EB[, Name := gsub(wrong_names[i], right_names[i], Name)]
}

Members_EB <- Members_EB[, `:=`(Surname = gsub("-.*", "", Members_EB$Name), Initials = gsub(".*-", "", Members_EB$Name))]


# Cleaning Institution
Members_EB <- Members_EB[, Institution := toupper(Institution)]
Members_EB <- Members_EB[, Institution := gsub("É", "E", Institution)][, Institution := gsub("Ö", "O", Institution)]

Institutions <- as.data.table(sort(unique(Members_EB$Institution)))
Institutions <- Institutions[, Institution_ID := 1:.N]

write_csv(Institutions, paste0(boards_path, "Institutions.csv"))

# integrating identifiers in the table
Members_EB <- merge(Members_EB, Institutions, by.x = "Institution", by.y = "V1", all.x = TRUE)

# Loading cleaned institutions and merging
Institutions <- fread(paste0(boards_path, "Institutions_cleaned.csv")) %>% as.data.table()
Members_EB <- merge(Members_EB, Institutions, by = "Institution_ID", all.x = TRUE)

Members_EB <- Members_EB[, c("Name", "Initials", "Surname", "Institution_name", "Institution_Label", "University_name", "Country", "Year", "Type", "Pays_bis")]
write_csv(Members_EB, paste0(boards_path, "Members_EB.csv"))

# Completing missing countries
Members_EB <- read_csv(paste0(boards_path, "Members_EB.csv")) %>% as.data.table()
Members_EB <- Members_EB[Pays_bis != "", Country := Pays_bis][, -"Pays_bis"]

# Completing missing Institutions
# extracting institutions per individuals to see if they have several
Individuals_institution <- unique(Members_EB[order(Name), c("Name", "Institution_name", "Institution_Label", "University_name")])
Individuals_institution <- Individuals_institution[order(Name) & !is.na(Institution_name)]
doublons <- unique(Individuals_institution[c(which(duplicated(Individuals_institution$Name)), which(duplicated(Individuals_institution$Name)) - 1)])
doublons <- doublons[order(Name)]
Unique_individuals_institution <- Individuals_institution[!Name %in% doublons$Name]

Members_EB <- merge(Members_EB, Unique_individuals_institution, by = "Name", all.x = TRUE)
Members_EB <- Members_EB[is.na(Institution_name.x), `:=`(Institution_name.x = Institution_name.y, Institution_Label.x = Institution_Label.y, University_name.x = University_name.y, Check = "Yes")]

# Saving to clean in excel
write_csv(Members_EB, paste0(boards_path, "Members_EB.csv"))
