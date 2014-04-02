#sink(file="/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/results/pbis_cleaning_output.txt")  # send output to file

# PBIS Cleaning File
# By Casey Tsui
# Format: R
# Last updated: 2/28/11


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
  #   and returns the proportion.
  #
  # Args:
  #   x: Data frame variable
  x1 <- x / a$nces_member
  return(x1)
}


FactorVariablesAll <- function(x) {
  # Factors variables that are found in all 4 data sets
  #
  # Args:
  #   x: Data frame variable
  x <- transform(x, school_schoolyear       = factor(school_schoolyear,
                                                     ordered=TRUE),
                    school_educationalorgid = factor(school_educationalorgid,
                                                     ordered=TRUE),
                    school_ncesschoolid     = factor(school_ncesschoolid,
                                                     ordered=TRUE))
}


FactorVariablesMasterOnly <- function(x) {
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
                 nces_level             = factor(nces_level))
}

FactorVariablesEBSonly <- function(x) {
  x <- transform(x,
                 sasurvey_selfassessmentid = factor(sasurvey_selfassessmentid,
                                                    ordered=TRUE),
                 sasurvey_ebssurveyid   = factor(sasurvey_ebssurveyid,
                                                 ordered=TRUE),
                 sasurvey_schoolbeginyear  = factor(sasurvey_schoolbeginyear,
                                                    ordered=TRUE),
                 sasurvey_datecompleted = factor(sasurvey_datecompleted),
                 sasurvey_respondentoccupationid = factor(sasurvey_respondentoccupationid))
}

FactorVariablesSETonly <- function(x) {
  x <- transform(x, set_setid = factor(set_setid, ordered=TRUE))
}

DropSingleMixedYearOR <- function(x) {
  # Args:
  #   x: Data frame object
  x <- x[-which((x$school_educationalorgid == "327" |
               x$school_educationalorgid == "328")
               & x$school_schoolyear == "200405"), ]
}

MergeBySchoolAndYear <- function(x, y) {
  x1 <- x
  y1 <- y
  return(merge(x1, y1, 
               by=c("school_educationalorgid", "school_schoolyear"),
               all=TRUE))
}


CenterVar <- function(x) {
  # Args:
  #   x: Data frame variable
  overall.mean <- mean(x, na.rm=TRUE)
  centered.var <- ifelse(!is.na(x),
                         x - overall.mean,
                         NA)
  return(centered.var)
}


SubsetByNames <- function(x, y) {
  # Args:
  #   x: Data frame
  #   y: Name bounds
  var1 <- which(names(x) == y[1])
  var2 <- which(names(x) == y[2])
  return(x[, var1:var2])
}

RemoveAltSchools <- function(x) {
  # Args:
  #   x: Data frame
  alt.criteria <- which(schools$Filter_School_IsAltJJ_max==1)
  alts <- schools$School_EducationalOrgId[alt.criteria] 
  return(x[(!x$school_educationalorgid %in% alts), ])
}


# PRELIMINARY CLEANING AND RECODING ############################################
# Convert all variable names to lowercase and factor important variables
data <- list(master, ebs, set, odr)
rm(master, ebs, set, odr)
data <- lapply(data, upData, lowernames=TRUE)
data <- lapply(data, FactorVariablesAll)
data[[1]] <- FactorVariablesMasterOnly(data[[1]])
data[[2]] <- FactorVariablesEBSonly(data[[2]])
data[[3]] <- FactorVariablesSETonly(data[[3]])


# SUBSETTING ###################################################################
# Drop alternative schools
data <- lapply(data, RemoveAltSchools)

# Shorten data frame names
a <- data[[1]]  # School data
b <- data[[2]]  # EBS data
c <- data[[3]]  # SET data
d <- data[[4]]  # ODR data
rm(data)


# Create indicator vars for EBS and SET
b <- transform(b, i.ebs = ifelse(!is.na(sasurvey_selfassessmentid), 1, 0))
c <- transform(c, i.set = ifelse(!is.na(set_setid), 1, 0))


# Drop schools with non-standard grade types
a <- subset(a, nces_gslo == "06" | nces_gslo == "07" | nces_gslo == "08" |
               nces_gslo == "09")
a <- transform(a,
               nces_gslo = factor(nces_gslo, ordered=TRUE),
               nces_gshi = factor(nces_gshi, ordered=TRUE))


# Create 3-category grade type ID
a$grade.type <- ifelse((a$nces_gslo == "06" | a$nces_gslo == "07") &
                                a$nces_gshi == "8",
                                "MS",           # 1=all middle 6-8 or 7-8
                                ifelse(a$nces_gslo == "09" & a$nces_gshi == "12",
                                       "HS",    # 2=all high schools 9-12
                                       "Mix"))  # 3=mixed schools

a$grade.type <- factor(a$grade.type, levels=c("MS", "HS", "Mix"), ordered=TRUE)
a$high.school <- ifelse(a$grade.type == "HS", 1, 0)


# Keep only those schools that maintained grade type through all years
a.names.grade <- c("school_educationalorgid", "school_schoolyear", "grade.type")
a.unique.grade <- unique(a[a.names.grade])
g <- with(a.unique.grade, table(school_educationalorgid, grade.type))
rm(a.unique.grade)
g <- g[(g[, 1] >= 3 | g[, 2] >= 3 | g[, 3] >= 3), ]
a <- a[a$school_educationalorgid %in% row.names(g), ]
b <- b[b$school_educationalorgid %in% row.names(g), ]
c <- c[c$school_educationalorgid %in% row.names(g), ]
d <- d[d$school_educationalorgid %in% row.names(g), ]

# Drop schools that don't have ODR data
a <- a[a$school_educationalorgid %in% d$school_educationalorgid, ]
b <- b[b$school_educationalorgid %in% d$school_educationalorgid, ]
c <- c[c$school_educationalorgid %in% d$school_educationalorgid, ]


# Drop schools that have only 1 year of ODR data
d.sub <- unique(d[, c("school_educationalorgid", "school_schoolyear")])
d.sub <- ddply(d.sub, c("school_educationalorgid"),
                 function(df)
                   c(odr.number.years = length(df$school_schoolyear)))
d.sub <- subset(d.sub, odr.number.years >= 2, select = school_educationalorgid)
d <- d[d$school_educationalorgid %in% d.sub$school_educationalorgid, ]
rm(d.sub)
a <- a[a$school_educationalorgid %in% d$school_educationalorgid, ]
b <- b[b$school_educationalorgid %in% d$school_educationalorgid, ]
c <- c[c$school_educationalorgid %in% d$school_educationalorgid, ]


# Drop unused factor levels
data <- list(a, b, c, d)
data <- lapply(data, FactorVariablesAll)
a <- data[[1]]
b <- data[[2]]
c <- data[[3]]
d <- data[[4]]
rm(data)


# Drop the first year for two OR schools that started as Mixed, but then have 3
# years of being a MS from 2005-06 to 2007-08
a <- DropSingleMixedYearOR(a)
b <- DropSingleMixedYearOR(b)
c <- DropSingleMixedYearOR(c)
d <- DropSingleMixedYearOR(d)



# DESCRIPTIVE VARIABLE CREATION ################################################
# Add State labels to dataset "a"
school_stateid      <- c("8",  "17", "24", "30", "41")
school.statelabel   <- c("CO", "IL", "MD", "MT", "OR")
school.instruc.days <- c(180,  176,  180,  180,  150)
# 1080 hours = 180 days of instruction
# CO = 1080 h = 180 days # IL = 176 days
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
a <- merge(a, state.labels, all.x=TRUE)

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
a$enrollment.cat <- factor(a$enrollment.cat, levels=c("S", "M", "L"),
                           ordered=TRUE)


a <- transform(a,
               enrollment.sq   = nces_member ^ 2,
               female.prop     = MakeStudentProportionA(nces_femaleenrollment),
               male.prop       = MakeStudentProportionA(nces_maleenrollment),
               afram.prop      = MakeStudentProportionA(nces_black),
               aian.prop       = MakeStudentProportionA(nces_am),
               asian.prop      = MakeStudentProportionA(nces_asian),
               latino.prop     = MakeStudentProportionA(nces_hisp),
               white.prop      = MakeStudentProportionA(nces_white),
               fte.100.student = MakeStudentProportionA(nces_fte) * 100,
               frpm.prop       = MakeStudentProportionA(nces_totfrl) * 100,
               year            = as.numeric(school_schoolyear))


# INDEPENDENT VARIABLE CREATION ################################################
# EBS ##########################################################################
# Create sum of EBS scores
b <- b[, c(1:9, seq(10, 100, 2), 102:length(b))]

sw.sub  <- SubsetByNames(b, c("sasurvey_studentexpectationss",
                              "sasurvey_districtreports"))
ncr.sub <- SubsetByNames(b, c("sasurvey_ncrbehaviorss",
                              "sasurvey_ncrallstaffs"))
cr.sub  <- SubsetByNames(b, c("sasurvey_crbehaviorsdefineds",
                              "sasurvey_crtransitionsefficients"))
ind.sub <- SubsetByNames(b, c("sasurvey_indregularassessmentss",
                              "sasurvey_indbehaviormonitoreds"))
b$sw.disc.sys  <- rowSums(sw.sub, na.rm=TRUE)
b$ncr.set      <- rowSums(ncr.sub, na.rm=TRUE)
b$cr.set       <- rowSums(cr.sub, na.rm=TRUE)
b$ind.stud.sys <- rowSums(ind.sub, na.rm=TRUE)
rm(sw.sub, ncr.sub, cr.sub, ind.sub)


# Validate additions by calculating the ranges of the aggregate variables
r1 <- range(b$sw.disc.sys,  na.rm=TRUE)  # Should be 0-36
r2 <- range(b$ncr.set,      na.rm=TRUE)  # Should be 0-18
r3 <- range(b$cr.set,       na.rm=TRUE)  # Should be 0-22
r4 <- range(b$ind.stud.sys, na.rm=TRUE)  # Should be 0-16

# Calculate percentage of EBS scores that are marked "In Place"
#   Includes partial in place scores
b$perc.in.place.sw.disc.sys  <- b$sw.disc.sys  / r1[2]
b$perc.in.place.ncr.set      <- b$ncr.set      / r2[2]
b$perc.in.place.cr.set       <- b$cr.set       / r3[2]
b$perc.in.place.ind.stud.sys <- b$ind.stud.sys / r4[2]


ebs.scores <- ddply(b, c("school_educationalorgid", "school_schoolyear"),
                    function(df)
                      c(perc.in.place.sw.disc.sys = mean(df$perc.in.place.sw.disc.sys, na.rm=TRUE),
                        perc.in.place.ncr.set = mean(df$perc.in.place.ncr.set, na.rm=TRUE),
                        perc.in.place.cr.set = mean(df$perc.in.place.cr.set, na.rm=TRUE),
                        perc.in.place.ind.stud.sys = mean(df$perc.in.place.ind.stud.sys, na.rm=TRUE)))
ebs.scores <- transform(ebs.scores,
                ebs.implementationaverage = rowMeans(ebs.scores[, c(3:6)],
                                                     na.rm=TRUE),
                ebs.criterion8080 = ifelse(perc.in.place.sw.disc.sys >= 0.8 &
                                           perc.in.place.ncr.set >= 0.8,
                                           1,
                                           0))




# SET ##########################################################################
set.scores <- unique(c[, c(1, 3, 9:15, 48:49)])
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


# DEPENDENT VARIABLE CREATION ##################################################
# Calculate number of in-school-suspensions per school per year
iss.odr <- subset(d, odr_admindecisionid == "7")
iss <- ddply(iss.odr, c("school_educationalorgid", "school_schoolyear"),
             function(df) 
               c(iss = length(df$odr_admindecisionid)))
rm(iss.odr)

# Calculate number of out-of-school-suspensions per school per year
oss.odr <- subset(d, odr_admindecisionid == "8")
oss <- ddply(oss.odr, c("school_educationalorgid", "school_schoolyear"),
             function(df) 
               c(oss = length(df$odr_admindecisionid)))
rm(oss.odr)

# Calculate number of emesxpulsions per school per year
expul.odr <- subset(d, odr_admindecisionid == "10")
expul <- ddply(expul.odr, c("school_educationalorgid", "school_schoolyear"),
             function(df) 
               c(expul = length(df$odr_admindecisionid)))
rm(expul.odr)

# Calculate number of major ODRs per school per year
d$odr_problembehaviorismajor <- ifelse(d$odr_problembehaviorisminor == "0",
                                       1, 0)
major.odr <- subset(d, odr_problembehaviorismajor == 1)
major <- ddply(major.odr, c("school_educationalorgid", "school_schoolyear"),
               function(df) 
                 c(major = length(df$odr_problembehaviorismajor)))
rm(major.odr)

# Calculate number of total (major+minor) ODRs per school per year
odr <- ddply(d, c("school_educationalorgid", "school_schoolyear"),
             function(df)
               c(odr = length(df$odr_problembehaviorismajor)))

# Counts recidivism (number of ODRs per student ID)
recid.odr <- ddply(d, c("school_educationalorgid", "school_schoolyear",
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
days_odr <- ddply(d, c("school_educationalorgid", "school_schoolyear"),
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


# STATE-SPECIFIC ASSESSMENT AND DEMOGRAPHIC DATA ###############################
# COLORADO
assessments.co <- ddply(assessments.co,
                        c("school_ncesschoolid", "school_schoolyear"),
                        function(df)
                          c(reading.score = mean(df$reading.score),
                            math.score    = mean(df$math.score)))
colorado <- merge(assessments.co, demographics.co,
                  by=c("school_ncesschoolid", "school_schoolyear"),
                  all=TRUE)


# ILLINOIS
assessments.il <- ddply(assessments.il,
                        c("school_ncesschoolid", "school_schoolyear"),
                        function(df)
                          c(reading.score = mean(df$reading.score),
                            math.score    = mean(df$math.score)))
illinois <- merge(assessments.il, demographics.il,
                  by=c("school_ncesschoolid", "school_schoolyear"),
                  all=TRUE)



# MARYLAND
assessments.md <- ddply(assessments.md,
                        c("school_ncesschoolid", "school_schoolyear"),
                        function(df)
                          c(reading.score = mean(df$reading.score),
                            math.score    = mean(df$math.score)))
maryland <- merge(assessments.md, demographics.md,
                  by=c("school_ncesschoolid", "school_schoolyear"),
                  all=TRUE)

# MONTANA
assessments.mt <- ddply(assessments.mt,
                        c("school_ncesschoolid", "school_schoolyear"),
                        function(df)
                          c(reading.score = mean(df$reading.score),
                            math.score    = mean(df$math.score)))
montana <- merge(assessments.mt, demographics.mt,
                 by=c("school_ncesschoolid", "school_schoolyear"),
                 all=TRUE)


# OREGON
oregon <- merge(oregon, inst.crosswalk, by="InstID")

#clean oregon



# ADDITIONAL VARIABLE CREATION #################################################
# Create total instruction time
#diff in num of ODRs pre-post multiplied by time factor found in Muscott article

# Create leadership/admin time
#diff in num of ODRs pre-post multiplied by time factor found in Muscott article

# Create teacher time
#diff in num of ODRs pre-post multiplied by time factor found in Muscott article


# MERGE DATA FOR MULTIVARIABLE ANALYSIS ########################################
lm.data <- MergeBySchoolAndYear(a, ebs.scores)
lm.data <- MergeBySchoolAndYear(lm.data, set.scores)
lm.data <- MergeBySchoolAndYear(lm.data, odrs)
lm.data <- lm.data[-which(is.na(lm.data$school_stateid)), ]
#lm.data <- merge(lm.data, colorado,
#                 by=c("school_ncesschoolid", "school_schoolyear",
#                      "school.statelabel"),
#                 all.x=TRUE)
#lm.data <- merge(lm.data, illinois,
#                 by=c("school_ncesschoolid", "school_schoolyear",
#                      "school.statelabel"),
#                 all.x=TRUE)
#lm.data <- merge(lm.data, maryland,
#                 by=c("school_ncesschoolid", "school_schoolyear",
#                      "school.statelabel"),
#                 all.x=TRUE)
#lm.data <- merge(lm.data, montana,
#                 by=c("school_ncesschoolid", "school_schoolyear",
#                      "school.statelabel"),
#                 all.x=TRUE)
#lm.data <- merge(lm.data, oregon,
#                by=c("school_ncesschoolid", "school_schoolyear",
#                     "school.statelabel"),
#                all.x=TRUE)

# Merge PBIS data with states data
states$school_ncesschoolid <- as.numeric(format(states$school_ncesschoolid,
                                                sci=FALSE))

lm.data$school_ncesschoolid <- as.numeric(as.character(lm.data$school_ncesschoolid))
lm.data <- merge(lm.data, states,
           by=c("school_ncesschoolid", "school_schoolyear"),
           all.x=TRUE)

# Drop mixed schools
lm.data <- unique(lm.data[-which(lm.data$grade.type=="Mix"), ])


# Additional variable creation
impl.avg.names <- c("ebs.implementationaverage", "set_implementationaverage")
lm.data$implementationaverage <- rowMeans(lm.data[, impl.avg.names],
                                          na.rm=TRUE)


lm.data <- transform(lm.data,
                     medium.fidelity = ifelse(implementationaverage >= 0.5 &
                                              implementationaverage < 0.8,
                                              1,
                                              0),
                     high.fidelity = ifelse(implementationaverage >= 0.8,
                                            1,
                                            0))
lm.data <- transform(lm.data,
                     fidelity = ifelse(medium.fidelity == 1,
                                       "Medium",
                                       ifelse(high.fidelity == 1,
                                              "High",
                                              "Low")))
lm.data$fidelity <- factor(lm.data$fidelity, levels=c("Low", "Medium", "High"),
                           ordered=TRUE)
lm.data$ebs.cat <- ifelse(lm.data$ebs.implementationaverage < 0.7, 0, 1)
lm.data$high.school <- ifelse(lm.data$grade.type == "HS", 1, 0)

lm.data <- transform(lm.data,
                     iss.100          = iss          / nces_member * 100,
                     oss.100          = oss          / nces_member * 100,
                     expul.100        = expul        / nces_member * 100,
                     major.100        = major        / nces_member * 100,
                     odr.100          = odr          / nces_member * 100,
                     perc.stud.odr    = num.stud     / nces_member * 100,
                     stud.repeats.100 = stud.repeats / nces_member * 100)
lm.data$school.instruc.days <- as.numeric(as.character(lm.data$school.instruc.days))
#lm.data$school.instruc.days <- as.numeric(levels(lm.data$school.instruc.days))[lm.data$school.instruc.days]
lm.data <- transform(lm.data,
                     iss.100.day      = iss.100      / school.instruc.days,
                     oss.100.day      = oss.100      / school.instruc.days,
                     expul.100.day    = expul.100    / school.instruc.days,
                     major.100.day    = major.100    / school.instruc.days,
                     odr.100.day      = odr.100      / school.instruc.days)

lm.data <- transform(lm.data, exposure = 1/(odr/odr.100.day))


# Urbanicity Coding
urban.names <- c("school_educationalorgid", "nces_locale", "nces_ulocal")
lm.data.urban <- unique(lm.data[, urban.names])
lm.data.urban$nces_locale <- as.numeric(as.character(lm.data.urban$nces_locale))
#lm.data.urban$nces_locale <- as.numeric(levels(lm.data.urban$nces_locale))[lm.data.urban$nces_locale]
lm.data.urban$nces_ulocal <- as.numeric(as.character(lm.data.urban$nces_ulocal))
#lm.data.urban$nces_ulocal <- as.numeric(levels(lm.data.urban$nces_ulocal))[lm.data.urban$nces_ulocal]
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
                             labels=c("City", "Suburb/Fringe", "Town", "Rural"),
                             ordered=TRUE)

rm(urban.names, lm.data.urban.a, lm.data.urban.b, lm.data.urban.x)


# Center continuous covariates
lm.data <- transform(lm.data,
                     male.prop.cntr = CenterVar(male.prop),
                     afram.prop.cntr = CenterVar(afram.prop),
                     aian.prop.cntr = CenterVar(aian.prop),
                     asian.prop.cntr = CenterVar(asian.prop),
                     latino.prop.cntr = CenterVar(latino.prop),
                     white.prop.cntr = CenterVar(white.prop),
                     frpm.prop.cntr  = CenterVar(frpm.prop),
                 #    special.ed.enroll.perc.cntr = CenterVar(special.ed.enroll.perc),
                 #    lep.perc.cntr = CenterVar(lep.perc),
                     nces_member.cntr = CenterVar(nces_member),
                     enrollment.sq.cntr = CenterVar(enrollment.sq),
                     fte.100.student.cntr = CenterVar(fte.100.student))


# Missing data indicators for use in logistic regression missing data analysis
#lm.data <- transform(lm.data,
#                     iss.100.day.m = ifelse(is.na(iss.100.day), 1, 0),
#                     oss.100.day.m = ifelse(is.na(oss.100.day), 1, 0),
#                     expul.100.day.m = ifelse(is.na(expul.100.day), 1, 0),
#                     major.100.day.m = ifelse(is.na(major.100.day), 1, 0),
#                     total.100.day.m = ifelse(is.na(total.100.day), 1, 0),
#                     recid.m = ifelse(is.na(recid), 1, 0),
#                     grad.rate.m = ifelse(is.na(grad.rate),
#                     reading.score.m = ifelse(is.na(reading.score),
#                     math.score.m = ifelse(is.na(math.score))
                     




save.image(file="/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/data/workspaces/pbis_cleaned.RData")

# Save csv files for Stata
setwd("/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/data/pbis_data/final_csv")
write.csv(lm.data, "lm_data.csv")
# Drop outlier school that has > 8000 ODRs
lm.data.outlier.dropped <- lm.data[-which(lm.data$school_educationalorgid == 3006), ]
write.csv(lm.data.outlier.dropped, "lm_data_outlier_dropped.csv")
# Drop high schools
lm.data.outlier.hs.dropped <- lm.data.outlier.dropped[-which(lm.data$high.school == 1), ]
write.csv(lm.data.outlier.hs.dropped, "lm_data_outlier_hs_dropped.csv")


#sink()
