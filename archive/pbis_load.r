setwd("/Users/HumanImpactPartners/Documents/hias/school_discipline_hia")
#sink(file="/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/results/pbis_output.txt") #send output to file

# PBIS Loading File
# By Casey Tsui
# Format: R
# Last updated: 10/18/10


# Author Comment: This file loads in PBIS school discipline data from .csv files


# READ IN FILES ################################################################
schools <- read.csv(file="~//Documents//hias//school_discipline_hia//data//pbis_data//Gottlieb Revised Data Files 9-16-2010//ECS_PrivateAltCodes.csv", header=TRUE, sep=",")

# Create lists of ebs and set datasets 
master.data <- list(read.csv(file="~//Documents//hias//school_discipline_hia//data//pbis_data//Gottlieb Revised Data Files 9-16-2010//SAS-SurveyData_200405to200607.csv", header=TRUE, sep=","),
                    read.csv(file="~//Documents//hias//school_discipline_hia//data//pbis_data//Gottlieb Revised Data Files 9-16-2010//SAS-ODRData_200405to200607.csv", header=TRUE, sep=","),
                    read.csv(file="~//Documents//hias//school_discipline_hia//data//pbis_data//Gottlieb Revised Data Files 9-16-2010//SAS-SurveyData_200405to200708.csv", header=TRUE, sep=","),
                    read.csv(file="~//Documents//hias//school_discipline_hia//data//pbis_data//Gottlieb Revised Data Files 9-16-2010//SAS-ODRData_200405to200708.csv", header=TRUE, sep=","),
                    read.csv(file="~//Documents//hias//school_discipline_hia//data//pbis_data//Gottlieb Revised Data Files 9-16-2010//SET-SurveyData_200405to200607.csv", header=TRUE, sep=","),
                    read.csv(file="~//Documents//hias//school_discipline_hia//data//pbis_data//Gottlieb Revised Data Files 9-16-2010//SET-ODRData_200405to200607.csv", header=TRUE, sep=","),
                    read.csv(file="~//Documents//hias//school_discipline_hia//data//pbis_data//Gottlieb Revised Data Files 9-16-2010//SET-SurveyData_200405to200708.csv", header=TRUE, sep=","),
                    read.csv(file="~//Documents//hias//school_discipline_hia//data//pbis_data//Gottlieb Revised Data Files 9-16-2010//SET-ODRData_200405to200708.csv", header=TRUE, sep=","))
names(master.data) <- c("ebs.a.3yr", "ebs.b.3yr", "ebs.a.4yr", "ebs.b.4yr", "set.a.3yr", "set.b.3yr", "set.a.4yr", "set.b.4yr")


# Use instead of loops
#mymats <- list()
#for (i in 1:4) {
#   myname <- paste('mymatrix', i, sep='')
#   mymats[[myname]] <- matrix(x1)

#sink()



data1 <- read.csv(file="~//Documents//hias//school_discipline_hia//data//pbis_data//Gottlieb Revised Data Files 9-16-2010//SAS-SurveyData_200405to200607.csv", header=TRUE, sep=",")
data2 <- read.csv(file="~//Documents//hias//school_discipline_hia//data//pbis_data//Gottlieb Revised Data Files 9-16-2010//SAS-ODRData_200405to200607.csv", header=TRUE, sep=",")
data3 <- read.csv(file="~//Documents//hias//school_discipline_hia//data//pbis_data//Gottlieb Revised Data Files 9-16-2010//SAS-SurveyData_200405to200708.csv", header=TRUE, sep=",")
data4 <- read.csv(file="~//Documents//hias//school_discipline_hia//data//pbis_data//Gottlieb Revised Data Files 9-16-2010//SAS-ODRData_200405to200708.csv", header=TRUE, sep=",")
data5 <- read.csv(file="~//Documents//hias//school_discipline_hia//data//pbis_data//Gottlieb Revised Data Files 9-16-2010//SET-SurveyData_200405to200607.csv", header=TRUE, sep=",")
data6 <- read.csv(file="~//Documents//hias//school_discipline_hia//data//pbis_data//Gottlieb Revised Data Files 9-16-2010//SET-ODRData_200405to200607.csv", header=TRUE, sep=",")
data7 <- read.csv(file="~//Documents//hias//school_discipline_hia//data//pbis_data//Gottlieb Revised Data Files 9-16-2010//SET-SurveyData_200405to200708.csv", header=TRUE, sep=",")
data8 <- read.csv(file="~//Documents//hias//school_discipline_hia//data//pbis_data//Gottlieb Revised Data Files 9-16-2010//SET-ODRData_200405to200708.csv", header=TRUE, sep=",")

ebs.a <- unique(rbind(data1, data3))
set.a <- unique(rbind(data5, data7))

names <- names(ebs.a[c(1:10, 108:length(ebs.a))])
master <- merge(ebs.a, set.a, all=TRUE)

odrs1 <- unique(rbind(data2, data4, data6, data8)
