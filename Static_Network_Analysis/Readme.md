Readme of the `Static_Network_Analysis` directory
================
Aurélien Goutsmedt and Alexandre Truc
/ Last compiled on 2021-03-11

## What will you find in this directory?

This directory contains all the scripts of the first part of the
project: we build different networks (coupling, co-citation, coupling by
authors, coupling by institutions, co-authorship) for six different
sub-periods (1970-1976, 1977-1983, 1984-1990, 1991-1996, 1997-2002,
2003-2008). It helps us to refine our methods, to control the relevance
of our results and to have a first big picture of the evolution of
macroeconomics since the 1970s.

This directory contains:

### Script\_paths\_and\_basic\_objects

As in other directories, there is a
[Script\_paths\_and\_basic\_objects.R](/Static_Network_Analysis/Script_paths_and_basic_objects.R)
with all the packages needed in the directory, and essential paths and
data. This script is loaded in other scripts.

### 1\_Script\_for\_building\_networks

The script
[1\_Script\_for\_building\_networks](/dynamic_networks/1_Script_for_building_networks.md)
creates all the networks for the six sub-periods and save them for use
in the following scripts.

### 2\_Script\_Static\_Network\_Analysis

The script
[2\_Script\_Static\_Network\_Analysis](/dynamic_networks/2_Script_Static_Network_Analysis.md)
takes as an input all the networks saved in the previous script. The
script finds communities using the [Leiden
algorithm](https://www.nature.com/articles/s41598-019-41695-z) and
calculates coordinates using [Force
Atlas 2](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0098679)
layout. It allows to project the graphs and to save the projections (for
them to be reused in Rmarkdown files).

### 3\_Script\_for\_tf-idf\_analysis

The script
[3\_Script\_for\_tf-idf\_analysis](/dynamic_networks/3_Script_for_tf-idf_analysis.md)
takes as an input the list of nodes and their communities saved in the
former script, and find the terms with the highest
[tf-idf](https://en.wikipedia.org/wiki/Tf%E2%80%93idf) values for each
community. We also saved pre-formatted plots to be displayed in
[“Static\_Network\_Analysis”](/dynamic_networks/Static_Network_Analysis.Rmd).

### 4\_Script\_community\_detection

The script
[4\_Script\_community\_detection](/dynamic_networks/4_Script_community_detection.md)
runs the Leiden algorithm for different resolution levels. We build
Sankey diagrams to see how communities identified at a higher level are
merged together when we decrease the Leiden resolution (and thus the
number of communities).

### Static\_Network\_Analysis

The Rmarkdown file
[Static\_Network\_Analysis](/dynamic_networks/Static_Network_Analysis.Rmd)
uses the objects and pictures saved in the scripts to produce six .html
(for each subperiod), offering a general portrait of macroeconomics. The
different html documents are automatically computed thanks to
[generating\_rmd](/Static_Network_Analysis/generating_rmd.md).
