setwd("/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/data/pbis_data/Gottlieb Revised Data Files 9-16-2010")
#sink(file="/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/results/pbis_output.txt") #send output to file

# PBIS Loading File
# By Casey Tsui
# Format: R
# Last updated: 10/19/10


# Author Comment: This file loads in PBIS school discipline data from .csv files
#   then merges them into two files
#   1) master = schools + EBS and SET scores
#   2) odr = all odr data


# READ IN FILES ################################################################
schools <- read.csv("ECS_PrivateAltCodes.csv", header=TRUE, sep=",")

# EBS Data
ebs.3yr <- read.csv("SAS-SurveyData_200405to200607.csv", header=TRUE, sep=",")
ebs.3yr$i.ebs.3yr <- 1
ebs.3yr$i.ebs.4yr <- NA
ebs.3yr$i.set.3yr <- NA
ebs.3yr$i.set.4yr <- NA
ebs.4yr <- read.csv("SAS-SurveyData_200405to200708.csv", header=TRUE, sep=",")
ebs.4yr$i.ebs.3yr <- NA
ebs.4yr$i.ebs.4yr <- 1
ebs.4yr$i.set.3yr <- NA
ebs.4yr$i.set.4yr <- NA
ebs <- unique(rbind(ebs.3yr, ebs.4yr))
rm(ebs.3yr, ebs.4yr)

# SET Data
set.3yr <- read.csv("SET-SurveyData_200405to200607.csv", header=TRUE, sep=",")
set.3yr$i.ebs.3yr <- NA
set.3yr$i.ebs.4yr <- NA
set.3yr$i.set.3yr <- 1
set.3yr$i.set.4yr <- NA
set.4yr <- read.csv("SET-SurveyData_200405to200708.csv", header=TRUE, sep=",")
set.4yr$i.ebs.3yr <- NA
set.4yr$i.ebs.4yr <- NA
set.4yr$i.set.3yr <- NA
set.4yr$i.set.4yr <- 1
set <- unique(rbind(set.3yr, set.4yr))
rm(set.3yr, set.4yr)

# Master data including all schools + EBS and SET scores
master <- merge(ebs, set, all=TRUE)
rm(ebs, set)

# ODR Data
odr.ebs.3yr <- read.csv("SAS-ODRData_200405to200607.csv", header=TRUE, sep=",")
odr.ebs.3yr$i.ebs.3yr <- 1
odr.ebs.3yr$i.ebs.4yr <- NA
odr.ebs.3yr$i.set.3yr <- NA
odr.ebs.3yr$i.set.4yr <- NA
odr.ebs.4yr <- read.csv("SAS-ODRData_200405to200708.csv", header=TRUE, sep=",")
odr.ebs.4yr$i.ebs.3yr <- NA
odr.ebs.4yr$i.ebs.4yr <- 1
odr.ebs.4yr$i.set.3yr <- NA
odr.ebs.4yr$i.set.4yr <- NA
odr.set.3yr <- read.csv("SET-ODRData_200405to200607.csv", header=TRUE, sep=",")
odr.set.3yr$i.ebs.3yr <- NA
odr.set.3yr$i.ebs.4yr <- NA
odr.set.3yr$i.set.3yr <- 1
odr.set.3yr$i.set.4yr <- NA
odr.set.4yr <- read.csv("SET-ODRData_200405to200708.csv", header=TRUE, sep=",")
odr.set.4yr$i.ebs.3yr <- NA
odr.set.4yr$i.ebs.4yr <- NA
odr.set.4yr$i.set.3yr <- NA
odr.set.4yr$i.set.4yr <- 1

# Concatenate all ODR data
odr <- unique(rbind(odr.ebs.3yr, odr.ebs.4yr, odr.set.3yr, odr.set.4yr))
rm(odr.ebs.3yr, odr.ebs.4yr, odr.set.3yr, odr.set.4yr)


setwd("/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/data/pbis_data/StudentIds_20101019")
s.ids.ebs.3yr <- read.csv("SAS-StudentId_200405to200607.csv",
                          header=TRUE, sep=",")
s.ids.ebs.4yr <- read.csv("SAS-StudentId_200405to200708.csv",
                          header=TRUE, sep=",")
s.ids.set.3yr <- read.csv("SET-StudentId_200405to200607.csv",
                          header=TRUE, sep=",")
s.ids.set.4yr <- read.csv("SET-StudentId_200405to200708.csv",
                          header=TRUE, sep=",")
s.ids <- rbind(s.ids.ebs.3yr, s.ids.ebs.4yr, s.ids.set.3yr, s.ids.set.4yr)
rm(s.ids.ebs.3yr, s.ids.ebs.4yr, s.ids.set.3yr, s.ids.set.4yr)



save.image("/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/data/workspaces/pbis_uncleaned_data.RData")
# Use instead of loops
#mymats <- list()
#for (i in 1:4) {
#   myname <- paste('mymatrix', i, sep='')
#   mymats[[myname]] <- matrix(x1)

#sink()



