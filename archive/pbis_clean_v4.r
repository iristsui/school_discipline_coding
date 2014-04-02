#sink(file="/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/results/pbis_cleaning_output.txt")  # send output to file

# PBIS Cleaning File
# By Casey Tsui
# Format: R
# Last updated: 11/17/10


# Author Comment: This file cleans the variables of interest.
#   Takes care of missing values, merging data frames, handling outliers. 
#   Creates the "lm.data" object that will be used in pbis_do.r for all analyses


#setwd("/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/coding")
#source("pbis_load.r")
library(Hmisc) # Hmisc needed to change variable names to lowercase
library(plyr)


# FUNCTION DEFINITIONS #########################################################
MakeStudentProportionA <- function(x) {
  # Divides the given data frame variable by the total enrollment at each school
  #   and returns teh proportion.
  #
  # Args:
  #   x: Data frame variable
  x1 <- x / a$nces_member
  return(x1)
}


FactorVariablesAB <- function(x) {
  # Factors variables that are found in both the master and odr data sets
  #
  # Args:
  #   x: Data frame variable
  x <- transform(x, school_schoolyear       = factor(school_schoolyear),
                    school_educationalorgid = factor(school_educationalorgid),
                    school_ncesschoolid     = factor(school_ncesschoolid),
                    school_districtid       = factor(school_districtid),
                    school_stateid          = factor(school_stateid))
}


FactorVariablesA <- function(x) {
  # Factors variables that are found in only the master data set
  #
  # Args:
  #   x: Data frame variable
  x <- transform(x,
                 school_lowgradeid      = factor(school_lowgradeid),
                 school_lowgradelabel   = factor(school_lowgradelabel),
                 school_highgradeid     = factor(school_highgradeid),
                 school_highgradelabel  = factor(school_highgradelabel),
                 school_schoolyear_nces = factor(school_schoolyear_nces),
                 nces_ncessch           = factor(nces_ncessch),
                 nces_type              = factor(nces_type),
                 nces_locale            = factor(nces_locale),
                 nces_ulocal            = factor(nces_ulocal),
                 nces_gslo              = factor(nces_gslo),
                 nces_gshi              = factor(nces_gshi),
                 nces_level             = factor(nces_level),
                 sasurvey_selfassessmentid = factor(sasurvey_selfassessmentid),
                 sasurvey_ebssurveyid   = factor(sasurvey_ebssurveyid),
                 sasurvey_schoolbeginyear  = factor(sasurvey_schoolbeginyear),
                 sasurvey_datecompleted = factor(sasurvey_datecompleted),
                 sasurvey_respondentoccupationid = factor(sasurvey_respondentoccupationid),
                 set_setid              = factor(set_setid))
}


MergeBySchoolAndYear <- function(x, y) {
  x1 <- x
  y1 <- y
  return(merge(x1, y1, 
               by=c("school_educationalorgid", "school_schoolyear"),
               all.x=TRUE))
}


# PRELIMINARY CLEANING AND RECODING ############################################
# Convert all variable names to lowercase and factor important variables
data <- list(master, odr)
rm(master, odr)
data <- lapply(data, upData, lowernames=TRUE)
data <- lapply(data, FactorVariablesAB)
data[[1]] <- FactorVariablesA(data[[1]])

# Shorten data frame names
a <- data[[1]]  # Master data
b <- data[[2]]  # ODR data
rm(data)


# Create indicator var for EBS and SET
a <- transform(a,
               i.ebs = ifelse(!is.na(sasurvey_selfassessmentid), 1, 0),
               i.set = ifelse(!is.na(set_setid), 1, 0))


a.names <- c("school_educationalorgid", "school_schoolyear", "nces_gslo",
           "nces_gshi", "i.ebs", "i.set")
a.unique <- unique(a[a.names])

a.ebs.count <- summarize(a.unique$i.ebs,
                         a.unique$school_educationalorgid,
                         sum, na.rm=TRUE)
a.set.count <- summarize(a.unique$i.set,
                         a.unique$school_educationalorgid,
                         sum, na.rm=TRUE)
a.unique <- cbind(a.ebs.count, a.set.count[, 2])
names(a.unique) <- c("school_educationalorgid", "num.ebs.years",
                     "num.set.years")


# SUBSETTING ###################################################################
a <- subset(a, nces_gslo == "06" | nces_gslo == "07" | nces_gslo == "08" |
               nces_gslo == "09")
a <- transform(a,
               nces_gslo = factor(nces_gslo),
               nces_gshi = factor(nces_gshi))


# Create 3-category grade type ID
a$grade.type <- ifelse((a$nces_gslo == "06" | a$nces_gslo == "07") &
                                a$nces_gshi == "8",
                                "MS",           # 1=all middle 6-8 or 7-8
                                ifelse(a$nces_gslo == "09" & a$nces_gshi == "12",
                                       "HS",    # 2=all high schools 9-12
                                       "Mix"))  # 3=mixed schools

a$grade.type <- factor(a$grade.type, levels=c("MS", "HS", "Mix"), ordered=TRUE)


# Keep only those schools that maintained grade type through all years
a.names.grade <- c("school_educationalorgid", "school_schoolyear", "grade.type")
a.unique.grade <- unique(a[a.names.grade])
g <- table(a.unique.grade$school_educationalorgid,
           a.unique.grade$grade.type)
rm(a.unique.grade)
g <- g[(g[, 1] >= 3 | g[, 2] >= 3 | g[, 3] >= 3), ]
a <- a[a$school_educationalorgid %in% row.names(g), ]
b <- b[b$school_educationalorgid %in% row.names(g), ]
g <- g[row.names(g) %in% b$school_educationalorgid, ]

# Look at the schools without ODR data and see if you want to exclude them
a <- a[a$school_educationalorgid %in% b$school_educationalorgid, ]


# Drop unused factor levels
a <- FactorVariablesAB(a)
a <- FactorVariablesA(a)
b <- FactorVariablesAB(b)


# DESCRIPTIVE VARIABLE CREATION ################################################
# Add State labels to dataset "a"
school_stateid      <- c("8",   "17",  "24",  "26",  "30",  "33",  "38",  "41")
school.statelabel   <- c("CO",  "IL",  "MD",  "MI",  "MT",  "NH",  "ND",  "OR")
school.instruc.days <- c(180,   176,   180,   186,   180,   180,   173,   150)
# 1080 hours = 180 days of instruction
# CO = 1080 h = 180 days
# IL = 176 days
# MD = 180 days
# MI = 1098 h = 183 days
# MT = 180 days
# NH = 180 days
# ND = 173 days
# OR 4-8 = 900 h = 150 days
# OR 9-12 = 990 h  (no 9-12 schools in data;
#   two OR schools had 3 years 6-8 & 1 year 6-12; the 6-12 years were dropped
state.labels <- data.frame(cbind(school_stateid, school.statelabel,
                                 school.instruc.days))
a <- merge(a, state.labels, by="school_stateid")
a <- a[-which(a$school.statelabel=="OR" & a$grade.type=="Mix"), ]

# Merge state labels to "a" to tabulate NCES IDs by state to give to Laura
#test.merge.data <- data.frame(unique(a.new[, c("school_educationalorgid",
#                                               "school_ncesschoolid",
#                                               "school_statelabel")]))
#schools1 <- schools
#schools1 <- merge(schools1, test.merge.data,
#                  by="school_educationalorgid",
#                  all.y=TRUE)
#write.csv(schools1, "nces_id_by_state.csv")


# Total Enrollment
# Create categories for enrollment
a$enrollment.cat <- ifelse(a$nces_member < 200,
                           "S",          # 1=small schools
                           ifelse(a$nces_member >= 200 & a$nces_member < 1000,
                                  "M",   # 2=medium schools
                                  "L"))  # 3=large schools
a$enrollment.cat <- factor(a$enrollment.cat, levels=c("S", "M", "L"))


a <- transform(a,
               enrollment.sq   = nces_member ^ 2,
               female.prop     = MakeStudentProportionA(nces_femaleenrollment),
               male.prop       = MakeStudentProportionA(nces_maleenrollment),
#               grade.06.prop   = MakeStudentProportionA(nces_g06),
#               grade.07.prop   = MakeStudentProportionA(nces_g07),
#               grade.08.prop   = MakeStudentProportionA(nces_g08),
#               grade.09.prop   = MakeStudentProportionA(nces_g09),
#               grade.10.prop   = MakeStudentProportionA(nces_g10),
#               grade.11.prop   = MakeStudentProportionA(nces_g11),
#               grade.12.prop   = MakeStudentProportionA(nces_g12),
               afram.prop      = MakeStudentProportionA(nces_black),
               aian.prop       = MakeStudentProportionA(nces_am),
               asian.prop      = MakeStudentProportionA(nces_asian),
               latino.prop     = MakeStudentProportionA(nces_hisp),
               white.prop      = MakeStudentProportionA(nces_white),
               fte.100.student = MakeStudentProportionA(nces_fte) * 100,
               frpm.prop       = MakeStudentProportionA(nces_totfrl) * 100,
               year            = as.numeric(school_schoolyear))
a <- transform(a, nonwhite.prop = 1 - white.prop)


# INDEPENDENT VARIABLE CREATION ################################################
# EBS ##########################################################################
a.ebs <- subset(a, i.ebs == 1)
gen.names <- c("school_educationalorgid", "school_schoolyear")
last.letter <- nchar(names(a))

ebs.names <- names(a[, substr(names(a), 1, 3) == "sas" &
                       (substr(names(a), last.letter, last.letter) == "s" |
                        substr(names(a), last.letter - 1, last.letter) == "id")])
ebs.names <- c(gen.names, ebs.names)
a.ebs <- a.ebs[, ebs.names]


# Create sum of EBS scores
sw.sub <- subset(a.ebs,
                 select=sasurvey_studentexpectationss:sasurvey_districtreports)
ncr.sub <- subset(a.ebs,
                  select=sasurvey_ncrbehaviorss:sasurvey_ncrallstaffs)
cr.sub <- subset(a.ebs,
                 select=sasurvey_crbehaviorsdefineds:sasurvey_crtransitionsefficients)
ind.sub <- subset(a.ebs,
                  select=sasurvey_indregularassessmentss:sasurvey_indbehaviormonitoreds)


a.ebs$sw.disc.sys  <- rowSums(sw.sub, na.rm=TRUE)
a.ebs$ncr.set      <- rowSums(ncr.sub, na.rm=TRUE)
a.ebs$cr.set       <- rowSums(cr.sub, na.rm=TRUE)
a.ebs$ind.stud.sys <- rowSums(ind.sub, na.rm=TRUE)

# Calculate percentage of EBS scores that are marked "In Place" - includes partial
# Schoolwide discipline system - percent "in place"
#a.ebs$perc.in.place.sw.disc.sys  <- rowSums(sw.sub, na.rm=TRUE) / (2*18)
#a.ebs$perc.in.place.ncr.set      <- rowSums(ncr.sub, na.rm=TRUE) / (2*9)
#a.ebs$perc.in.place.cr.set       <- rowSums(cr.sub, na.rm=TRUE) / (2*11)
#a.ebs$perc.in.place.ind.stud.sys <- rowSums(ind.sub, na.rm=TRUE) / (2*8)


# Validate additions by calculating the ranges of the aggregate variables
range(a.ebs$sw.disc.sys,  na.rm=TRUE)  # Should be 0-36
range(a.ebs$ncr.set,      na.rm=TRUE)  # Should be 0-18
range(a.ebs$cr.set,       na.rm=TRUE)  # Should be 0-22
range(a.ebs$ind.stud.sys, na.rm=TRUE)  # Should be 0-16


# Calculate percentage of EBS scores that are marked "In Place"
# Schoolwide discipline system - percent "in place"
a.ebs$perc.in.place.sw.disc.sys  <- rowSums(sw.sub  == 2, na.rm=TRUE) / 18
a.ebs$perc.in.place.ncr.set      <- rowSums(ncr.sub == 2, na.rm=TRUE) /  9
a.ebs$perc.in.place.cr.set       <- rowSums(cr.sub  == 2, na.rm=TRUE) / 11
a.ebs$perc.in.place.ind.stud.sys <- rowSums(ind.sub == 2, na.rm=TRUE) /  8


ebs.scores <- ddply(a.ebs, c("school_educationalorgid", "school_schoolyear"),
                    function(df)
                      c(perc.in.place.sw.disc.sys = mean(df$perc.in.place.sw.disc.sys, na.rm=TRUE),
                        perc.in.place.ncr.set = mean(df$perc.in.place.ncr.set, na.rm=TRUE),
                        perc.in.place.cr.set = mean(df$perc.in.place.cr.set, na.rm=TRUE),
                        perc.in.place.ind.stud.sys = mean(df$perc.in.place.ind.stud.sys, na.rm=TRUE)))
ebs.scores <- transform(ebs.scores,
                        ebs_implementationaverage = rowSums(perc.in.place.sw.disc.sys,
                                                           perc.in.place.ncr.set,
                                                           perc.in.place.cr.set,
                                                           perc.in.place.stud.sys,
                                                           na.rm=TRUE)
rm(a.ebs)


# Create 8080 and 5050 Score
ebs_criterion8080 <- ifelse(ebs.scores)





# SET ##########################################################################
a.set <- subset(a, i.set == 1)
set.names <- names(a[, substr(names(a), 1, 3) == "set"])
set.names <- c(gen.names, set.names)
a.set <- a.set[, set.names]
set.scores <- unique(a.set[, c(1:2, 8:14, 47:48)])
set.scores <- ddply(set.scores,
                    c("school_educationalorgid", "school_schoolyear"),
                    function(df)
                      c(set_expectationsdefined = mean(df$set_expectationsdefined, na.rm=TRUE),
                        set_expectationstaught = mean(df$set_expectationstaught, na.rm=TRUE),
                        set_rewardsystem = mean(df$set_rewardsystem, na.rm=TRUE),
                        set_violationssystem = mean(df$set_violationssystem, na.rm=TRUE),
                        set_monitoringevaluation = mean(df$set_monitoringevaluation, na.rm=TRUE),
                        set_leadership = mean(df$set_leadership, na.rm=TRUE),
                        set_districtsupport = mean(df$set_districtsupport, na.rm=TRUE),
                        set_implementationaverage = mean(df$set_implementationaverage, na.rm=TRUE),
                        set_criterion8080 = mean(df$set_criterion8080, na.rm=TRUE)))
rm(a.set)


# DEPENDENT VARIABLE CREATION ##################################################
# Calculate number of in-school-suspensions per school per year
iss.odr <- subset(b, odr_admindecisionid == "7")
iss <- ddply(iss.odr, c("school_educationalorgid", "school_schoolyear"),
             function(df) 
               c(iss = length(df$odr_admindecisionid)))

# Calculate number of out-of-school-suspensions per school per year
oss.odr <- subset(b, odr_admindecisionid == "8")
oss <- ddply(oss.odr, c("school_educationalorgid", "school_schoolyear"),
             function(df) 
               c(oss = length(df$odr_admindecisionid)))

# Calculate number of emesxpulsions per school per year
expul.odr <- subset(b, odr_admindecisionid == "10")
expul <- ddply(expul.odr, c("school_educationalorgid", "school_schoolyear"),
             function(df) 
               c(expul = length(df$odr_admindecisionid)))

# Calculate number of major ODRs per school per year
b$odr_problembehaviorismajor <- ifelse(b$odr_problembehaviorisminor == "0",
                                       1, 0)
major.odr <- subset(b, odr_problembehaviorismajor == 1)
major <- ddply(major.odr, c("school_educationalorgid", "school_schoolyear"),
               function(df) 
                 c(major = length(df$odr_problembehaviorismajor)))

# Calculate number of total (major+minor) ODRs per school per year
odr <- ddply(b, c("school_educationalorgid", "school_schoolyear"),
             function(df)
               c(odr = length(df$odr_problembehaviorismajor)))

# Counts recidivism (number of ODRs per student ID)
recid.odr <- ddply(b, c("school_educationalorgid", "school_schoolyear",
                        "student_studentid"),
               function(df) 
                 c(recid.odr = length(df$student_studentid)))
recid <- ddply(recid.odr, c("school_educationalorgid", "school_schoolyear"),
               function(df) 
                 c(recid = mean(df$recid.odr, na.rm=TRUE)))

# Counts number of students with at least one ODR
num.stud <- ddply(recid.odr, c("school_educationalorgid", "school_schoolyear"),
                  function(df) 
                    c(num.stud = length(df$student_studentid)))


# Calculate number of repeats* per school per year
#   *Students with ODRs in a given year that have ODRs in the following year
recid.odr.1 <- subset(recid.odr, school_schoolyear == 200405)
recid.odr.2 <- subset(recid.odr, school_schoolyear == 200506)
recid.odr.3 <- subset(recid.odr, school_schoolyear == 200607)
recid.odr.4 <- subset(recid.odr, school_schoolyear == 200708)
repeats.0405 <- recid.odr.1[recid.odr.1$student_studentid %in%
                            recid.odr.2$student_studentid, ]
repeats.0506 <- recid.odr.2[recid.odr.2$student_studentid %in%
                            recid.odr.3$student_studentid, ]
repeats.0607 <- recid.odr.3[recid.odr.3$student_studentid %in%
                            recid.odr.4$student_studentid, ]
num.repeats.0405 <- ddply(repeats.0405,
                          c("school_educationalorgid", "school_schoolyear"),
                          function(df) 
                            c(stud.repeats = length(df$student_studentid)))
num.repeats.0506 <- ddply(repeats.0506,
                          c("school_educationalorgid", "school_schoolyear"),
                          function(df) 
                            c(stud.repeats = length(df$student_studentid)))
num.repeats.0607 <- ddply(repeats.0607,
                          c("school_educationalorgid", "school_schoolyear"),
                          function(df) 
                            c(stud.repeats = length(df$student_studentid)))
num.repeats <- rbind(num.repeats.0405, num.repeats.0506, num.repeats.0607)


# Days suspended and expelled
days_odr <- ddply(b, c("school_educationalorgid", "school_schoolyear"),
                  function(df)
                    c(days_susp  = mean(df$odr_dayssuspended, na.rm=TRUE),
                      days_expul = mean(df$odr_daysexpelled, na.rm=TRUE)))


# Merge ODR statistics
odrs <- MergeBySchoolAndYear(iss, oss)
odrs <- MergeBySchoolAndYear(odrs, expul)
odrs <- MergeBySchoolAndYear(odrs, major)
odrs <- MergeBySchoolAndYear(odrs, odr)
odrs <- MergeBySchoolAndYear(odrs, recid)
odrs <- MergeBySchoolAndYear(odrs, num.stud)
odrs <- MergeBySchoolAndYear(odrs, num.repeats)
odrs <- MergeBySchoolAndYear(odrs, days_odr)


# MARYLAND
# Test scores
test.scores.md <- ddply(assessment.md,
                        c("school_educationalorgid", "school_schoolyear"),
                        function(df)
                          c(reading.score = mean(df$reading.score)

# Graduation rates
grad.rate.md <- ddply(grad.rate.md,
                      c("school_educationalorgid", "school_schoolyear"),
                        function(df)
                          c(grad.rate = mean(df$grad.rate)
##############




# ADDITIONAL VARIABLE CREATION #################################################
# Create total instruction time
#diff in num of ODRs pre-post multiplied by time factor found in Muscott article

# Create leadership/admin time
#diff in num of ODRs pre-post multiplied by time factor found in Muscott article

# Create teacher time
#diff in num of ODRs pre-post multiplied by time factor found in Muscott article


# MERGE DATA FOR MULTIVARIABLE ANALYSIS ########################################
lm.data <- unique(a[, c(1:44, 189:length(a))])
lm.data <- MergeBySchoolAndYear(lm.data, ebs.scores)
lm.data <- MergeBySchoolAndYear(lm.data, set.scores)
lm.data <- MergeBySchoolAndYear(lm.data, odrs)
lm.data <- unique(lm.data)


lm.data$susp <- rowSums(lm.data[, c("iss", "oss")], na.rm=TRUE)
lm.data$susp[is.na(lm.data$iss) & is.na(lm.data$oss)] <- NA
lm.data <- transform(lm.data,
                     susp.100         = susp         / nces_member * 100,
                     expul.100        = expul        / nces_member * 100,
                     major.100        = major        / nces_member * 100,
                     odr.100          = odr          / nces_member * 100,
                     perc.stud.odr    = num.stud     / nces_member * 100,
                     stud.repeats.100 = stud.repeats / nces_member * 100)
lm.data <- transform(lm.data,
                     susp.100.day     = susp.100     / school.instruc.days,
                     expul.100.day    = expul.100    / school.instruc.days,
                     major.100.day    = major.100    / school.instruc.days,
                     odr.100.day      = odr.100      / school.instruc.days)


# Urbanicity Coding
lm.data.urban <- unique(lm.data[, c(1, 15:16)])
lm.data.urban$nces_locale <- as.numeric(as.character(lm.data.urban$nces_locale))
lm.data.urban$nces_ulocal <- as.numeric(as.character(lm.data.urban$nces_ulocal))
lm.data.urban.a <- unique(lm.data.urban[, c(1,2)])
lm.data.urban.a <- lm.data.urban.a[!is.na(lm.data.urban.a$nces_locale), ]
lm.data.urban.b <- unique(lm.data.urban[, c(1,3)])
lm.data.urban.b <- lm.data.urban.b[!is.na(lm.data.urban.b$nces_ulocal), ]
lm.data.urban.x <- cbind(lm.data.urban.a, lm.data.urban.b)

addmargins(table(lm.data.urban.x[, 2], lm.data.urban.x[, 4]))

lm.data.urban.x <- lm.data.urban.x[, c(1, 4)]
names(lm.data.urban.x)[2] <- "urbanicity"


lm.data <- merge(lm.data, lm.data.urban.x,
                 by="school_educationalorgid", all=TRUE)

lm.data$urbanicity <- substr(as.character(lm.data$urbanicity), 1, 1)
lm.data$urbanicity <- factor(lm.data$urbanicity,
                             labels=c("City", "Suburb/Fringe", "Town", "Rural"))

rm(lm.data.urban.a, lm.data.urban.b, lm.data.urban.x)










lm.data.2004 <- subset(lm.data, year == 1)
lm.data.2005 <- subset(lm.data, year == 2)
lm.data.2006 <- subset(lm.data, year == 3)
lm.data.2007 <- subset(lm.data, year == 4)


save.image(file="/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/data/workspaces/pbis_cleaned.RData")

#sink()
