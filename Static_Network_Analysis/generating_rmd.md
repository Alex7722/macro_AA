Script for automaticlaly generating html for different periods
================
Aurélien Goutsmedt and Alexandre Truc
/ Last compiled on 2021-03-11

# What is this script for?

This simple script render
[Static\_Network\_Analysis.Rmd](/Static_Network_Analysis/Static_Network_Analysis.Rmd)
for different values of `i`, meaning for different subperiods. It avoids
knitting six times the .Rmd.

# Rendering multiple .html

We load the entire
[Script\_paths\_and\_basic\_objects.R](/Static_Network_Analysis/Script_paths_and_basic_objects.R)
in case we have changed the sub-periods.

``` r
source(path.expand("~/macro_AA/Static_Network_Analysis/Script_paths_and_basic_objects.R"))

i  <- 1:6

for (i in 1:6){
  rmarkdown::render("~/macro_AA/Static_Network_Analysis/Static_Network_Analysis.Rmd",
                    output_file = paste0('Static_Network_Analysis_',
                                         start_date[i],"-",end_date[i],'.html'))    
}
```
