# PBIS Master File
# By Casey Tsui
# Format: R
# Last updated: 3/31/11


# Author's Comments:
# This file sources all other files in this project
#   Calls the functions defined in pbis_clean.r to clean the data.
#   Calls the functions defined in pbis_func.r to perform the analysis and
#   produces charts and tables.

sink(file="/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/results/pbis_analysis_output.txt") #send output to file


source("pbis_load_v3")
setwd("/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/coding")
source("pbis_clean_v8.r")
setwd("/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/coding")
source("pbis_func.r")

source("pbis_do_v8.r")


sink()
