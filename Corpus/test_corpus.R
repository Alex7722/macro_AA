# loading the graph if necessary
test_names <- coauthorship_nodes_1990s %>%
  select(citing_author) %>%
  rename(Id = citing_author) %>%
  as.data.table()

test_names <- test_names[order(Id)]
test_names <- test_names[, surname:=  gsub("-.*","",Id)][, initials:=  gsub(".*-","",Id)]

test_names <- test_names[, length_initial:=  nchar(initials), by = "Id"]
test_names <- test_names[, initial_1:=  gsub("(?<!^).", "", initials, perl=TRUE)]
test_names <- test_names[, N := .N, by = c("surname","initial_1")][N > 1]
