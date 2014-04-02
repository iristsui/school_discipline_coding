# Read in CDS Code List from CDE Data
# Used for matching CDS codes to NCES IDs
# By Casey Tsui
# Format: R
# Last updated: 4/26/11


cds_nces <- read.table("/Users/HumanImpactPartners/Desktop/pubschls.txt", header=TRUE, sep=";", dec=".", quote="\"", nrows=16830, fill=TRUE, colClasses="character")

cds_nces <- cds_nces[which(cds_nces$District == "Oakland Unified" | cds_nces$District == "Los Angeles Unified"), ]

substr(school_ncesschoolid, 8, 12)
