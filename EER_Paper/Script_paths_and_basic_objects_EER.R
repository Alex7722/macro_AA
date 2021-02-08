#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
############################## LOADING PACKAGES, PATHS AND OBJECTS ####################################--------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

##################### Packages ############################################--------------

package_list <-  c("data.table","magrittr","tidyverse", "ggnewscale", "igraph",
                   "tm","quanteda","tidytext","ggraph","tidygraph", "ggrepel", "vite",
                   "reticulate","leiden","reshape2","scales","scico",
                   "ggforce","directlabels","patchwork","DescTools","DT","grid","ggdendro",
                   "ggalluvial","knitr","readtext")
for(p in package_list){
  if (p %in% installed.packages()==FALSE){install.packages(p,dependencies = TRUE)}
  library(p,character.only=TRUE)
}

#py_install("python-igraph")
#py_install("leidenalg", forge = TRUE)

######################### Paths and data ##########################################------------

boards_path <- "/projects/data/macro_AA/EER/editorial_boards/"
produced_data_path <- "/projects/data/macro_AA/EER/"
picture_path <- "/home/aurelien/macro_AA/EER_Paper/Pictures/"
