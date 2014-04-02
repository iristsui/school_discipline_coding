# IMPORTING PBIS GRAD RATES AND TEST SCORES
# By Casey Tsui
# Format: R
# Last updated: 10/12/10


# Author's comment:
# Loads PBIS data that was downloaded by Research Assistant, which includes 
#   graduation rates and test scores for all PBIS schools in Oregon data.


pbis.download <- read.csv(file="", header=TRUE, sep=',')


# Names of time-varying variables
pbis.varying.vars <- c("sat.verbal.score",
                       "sat.math.score",
                       "sat.writing.score",
                       "enrollment.total",
                       "enrollment.male",
                       "enrollment.female",
                       "graduates.total")


# Reshape data from wide to long format to merge with main schoolwide data
pbis.reshaped <- reshape(pbis.download,
                         varying=pbis.varying.vars,
                         idvar=school_educationalorgid,
                         ids=,
                         times=c("200405", "200506", "200607", "200708"),
                         direction="long",
                         sep=".")

# Merge data with existing dataset
final.data <- merge(a, pbis.reshaped,
                    by=c("school_educationalorgid", "school_schoolyear"),
                    all=TRUE)
