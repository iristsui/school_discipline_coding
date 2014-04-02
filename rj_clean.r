# RJ Cleaning File
# By Casey Tsui
# Format: R
# Last updated: 10/12/10

# Author's Comment:
#


setwd("")
source("rj_load.r")


# Set missing values
rj.data[""] <- NA


# Reshape data from wide to long format
rj.data <- reshape(file=rj.data,
                   varying=,
                   times=,
                   sep=".")


