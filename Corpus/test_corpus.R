# Elements for a function to test the doublons in nodes

# loading the relevant data
test_names <- coauthorship_nodes_1990s %>%
  select(citing_author) %>%
  rename(Id = citing_author) %>%
  as.data.table()

# Isolating surname, initials, and first initial
test_names <- test_names[order(Id)]
test_names <- test_names[, surname:=  gsub("-.*","",Id)][, initials:=  gsub(".*-","",Id)]
test_names <- test_names[, length_initial:=  nchar(initials), by = "Id"]
test_names <- test_names[, initial_1:=  gsub("(?<!^).", "", initials, perl=TRUE)]

# Only keep the surname + first initial that occurs more than one time
test_names <- test_names[, N := .N, by = c("surname","initial_1")][N > 1]
