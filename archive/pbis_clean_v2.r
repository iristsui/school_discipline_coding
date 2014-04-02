#sink(file="/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/results/pbis_cleaning_output.txt")  # send output to file

# PBIS Cleaning File
# By Casey Tsui
# Format: R
# Last updated: 10/12/10


# Author Comment: This file cleans the variables of interest.
#   This is where all the ugly stuff lives.
#   Takes care of missing values, merging data frames, handling outliers. 


setwd("/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/coding")
source("pbis_load.r")
library(Hmisc) # Hmisc needed to change variable names to lowercase


# FUNCTION DEFINITIONS #########################################################
MakeA <- function(x) {
  # Args:
  #   x: Data frame
  x1 <- SubsetA(x)
  x2 <- DropFactors(x1)
  x3 <- ConvertA(x2)
  return(x3)
}
SubsetA <- function(x) {
  # Args:
  #   x: Data frame
  if (pbis.test.mode == "ebs") {
    x1 <- unique(x[, -c(2, 11:107)])
  } else {
    x1 <- unique(x[, -c(2, 11:55)])
  }
  x1[x1 == "NULL"] <- NA
  if (levels(x1$nces_gshi)[1] == "05") {
    x1 <- x1[-which(x1$nces_gshi == "05"), ]
  }
  return(x1)
}
DropFactors <- function(x) {
  # Args:
  #   x: Data frame
  x1 <- x
  # Drop unused factor levels
  for (i in 1:ncol(x1)) {
    if (is.factor(x1[, i]) == TRUE)
      x1[, i] <- factor(x1[, i])
  }
  return(x1)
}
ConvertA <- function(x) {
  x1 <- x
  # Convert variables to numeric from factor
  for (i in c(10:12, 14:17, 20:ncol(x1))) {
    x1[, i] <- as.numeric(as.character(x1[, i]))
  }
  return(x1)
}


MakeB <- function(x) {
  # Args:
  #   x: Data frame
  x1 <- SubsetB(x)
  x2 <- DropFactors(x1)
  if (pbis.test.mode == "ebs" {
    x2 <- ConvertB(x2)
  }
  return(x2)
}
SubsetB <- function(x) {
  x1 <- x
  if (pbis.test.mode == "ebs") {
    x1 <- x1[, c(1:3, 11:107)]
  } else {
    x1 <- x1[, c(1:3, 11:55)]
  }
  x1[x1 == "NULL"] <- NA  # Change NULLs to NAs
  return(x1)
}
ConvertB <- function(x) {
  x1 <- x
  # Convert variables to numeric from factor
  for (i in 9:ncol(x1)) {
    x1[, i] <- as.numeric(as.character(x1[, i]))
  }
  return(x1)
}


MakeC <- function(x) {
  # Args:
  #   x: Data frame
  x1 <- SubsetC(x)
  x2 <- DropFactors(x1)
  x3 <- RecodeC(x2)
  return(x3)
}
SubsetC <- function(x) {
  x1 <- x
  x1 <- x1[, c(1:3, 7:24)]
  x1[x1 == "NULL"] <- NA  # Change NULLs to NAs
  return(x1)
}
RecodeC <- function(x) {
  x1 <- x
  # Recode days suspended and expelled as numeric
  for (i in 20:21) {
    x1[, i] <- as.numeric(as.character(x1[, i]))
  }
  return(x1)
}


KeepConsistentGrades <- function(x, y) {
  # Args:
  #   x: Data frame
  #   y: Index data frame that includes only the row names that correspond to
  #   the school organizational IDs that maintain a consistent grade type ID
  x1 <- x
  y1 <- y
  x1 <- merge(x1, y1, by.x="school_educationalorgid", by.y="row.names")
  x1 <- x1[, -c((length(x1)-2):length(x1))]
  return(x1)
}


MakeStudentProportionA <- function(x) {
  # Args:
  #   x: Data frame variable
  x1 <- x / a$nces_member
  return(x1)
}

MakeStaffResponse <- function(x) {
  # Creates a variable for number of EBS surveys per FTE
  # Args:
    # x: Data frame
  x1 <- x

  MakeAggregate(x, 200405)
  MakeAggregate(x, 200506)
  MakeAggregate(x, 200607)
  #MakeAggregate(x, 200708)

  ebs.x <- rbind(ebs.x1, ebs.x2, ebs.x3)
  rm(ebs.y1, ebs.y2, ebs.y3, ebs.x1, ebs.x2, ebs.x3)
  #ebs.x <- susp.x[order(c(as.numeric(rownames(ebs.x)), ebs.y1), ]
  colnames(ebs.x) <- c("ebs.y1", "ebs.surveys")
  ebs.by.school <- merge(a, ebs.x,
                         by.x=c("school_educationalorgid", "school_schoolyear"),
                         by.y=c("row.names", "ebs.y1"), all=TRUE) 
  # ebs.by.school has an ebs survey count variable per school per year
  rm(ebs.x)
  # NOTE: Response rate can be greater than 100, as the nces_fte number can be
  # lower than the number of surveys completed...
}
MakeAggregate <- function(x, y) {
  # 
  # Arg:
  #   x: Data frame
  #   y: Year
  x1 <- x
  ebs.y1 <- rep(y, length(unique(x1$school_educationalorgid)))
  ebs.x1 <- cbind(ebs.y1,
                  table(b$school_educationalorgid[b$school_schoolyear == y] 
  return()
}


MakeYear <- function(x) {
  # Adds a year index variable to a data frame
  #
  # Args:
  #   x: Data frame
  #
  # Returns:
  #   The data frame with a new variable for year
  x1 <- x
  x1$year <- ifelse(x1$school_schoolyear == 200405,
                   1,
                   ifelse(x1$school_schoolyear == 200506,
                          2,
                          ifelse(x1$school_schoolyear == 200607,
                                 3,
                                 4)))
  return(x1)
}


# PRELIMINARY CLEANING AND RECODING ############################################
# Convert all variable names to lowercase
schools <- upData(schools, lowernames=TRUE)

# ebs schools
master.data <- lapply(master.data, upData, lowernames=TRUE)

# CHOOSE WHICH FIDELITY MEASURE AND YEAR SPAN
  # INDEX 1 = EBS 3-year
  # INDEX 2 = ODR Data 3-year
  # INDEX 3 = EBS 4-year
  # INDEX 4 = ODR Data 4-year
  # INDEX 5 = SET 3-year
  # INDEX 6 = ODR Data 3-year
  # INDEX 7 = SET 4-year
  # INDEX 8 = ODR Data 4-year
schools.and.surveys <- ebs.data[[1]]
odrs <- ebs.data[[2]]

# Remove other data sets to increase comp speed
#rm(ebs.data, set.data)


# SAVE ALL SCHOOL-LEVEL VARIABLES AS OBJECT "A"
a <- MakeA(schools.and.surveys)

# SAVE ALL FIDELITY MEASURE VARIABLES AS OBJECT "B"
b <- MakeB(schools.and.surveys)

# SAVE ALL ODR VARIABLES AS OBJECT "C"
c <- MakeC(odrs)


# Remove initial datasets to free up memory and increase computational speed
rm(schools.and.surveys, odrs)


# Create conditional vector of years based on data loaded
if (length(unique(a$school_schoolyear)) == 3) {
  years <- c("2004", "2005", "2006")
} else {
  years <- c("2004", "2005", "2006", "2007")
}


# SUBSETTING ###################################################################
# Create 3-category grade type ID
a$school.gradetypeid <- ifelse(a$nces_gslo == "09" & a$nces_gshi == "12",
                               2,          # 2=all high schools 9-12
                               ifelse((a$nces_gslo == "06" |
                                       a$nces_gslo == "07") &
                                       a$nces_gshi == "08",
                                       1,   # 1=all middle schools 6-8 or 7-8
                                       3))  # 3=mixed schools

# Keep only those schools that maintained grade type through all years
g <- table(a$school_educationalorgid, a$school.gradetypeid)
g <- g[(g[, 1] == 3 | g[, 2] == 3 | g[, 3] == 3), ]
a <- KeepConsistentGrades(a, g)
b <- KeepConsistentGrades(b, g)
c <- KeepConsistentGrades(c, g)
rm(g)


test.merge.data <- data.frame(unique(a$school_educationalorgid))
test.merge.data$indicator <- 1
names(test.merge.data) <- c("school_educationalorgid", names(ebs.data)[[1]])

schools <- merge(schools, test.merge.data,
                 by="school_educationalorgid",
                 all=TRUE)


# DESCRIPTIVE VARIABLE CREATION ################################################
# District
school_districtid <- sort(unique(a$school_districtid))
district.id <- seq(1:length(dist))
d <- cbind(school_districtid, district.id)
a1 <- merge(a, d,
           by="school_districtid",
           all=TRUE)
rm(d)


# Year
a <- MakeYear(a)
b <- MakeYear(b)
c <- MakeYear(c)


# Merge grade type id to dataset b and c
d <- unique(a[c("school_educationalorgid", "school.gradetypeid")])
b <- merge(b, d, by="school_educationalorgid", all=TRUE)
c <- merge(c, d, by="school_educationalorgid", all=TRUE)
rm(d)

# Total Enrollment
# Create categories for enrollment
a$enrollment.cat <- ifelse(a$nces_member < 200,
                           1,          # 1=small schools
                           ifelse(a$nces_member >= 200 & a$nces_member < 1000,
                                  2,   # 2=medium schools
                                  3))  # 3=large schools


# Create enrollment^2 for multivariable models
a$enrollment.sq <- a$nces_member ^ 2


# Create proportion of students for each gender out of total enrolled
a$female.prop <- MakeStudentProportionA(a$nces_femaleenrollment)
a$male.prop   <- MakeStudentProportionA(a$nces_maleenrollment)


# Create proportions of students for each grade out of total enrolled
a$grade.06.prop <- MakeStudentProportionA(a$nces_g06)
a$grade.07.prop <- MakeStudentProportionA(a$nces_g07)
a$grade.08.prop <- MakeStudentProportionA(a$nces_g08)
a$grade.09.prop <- MakeStudentProportionA(a$nces_g09)
a$grade.10.prop <- MakeStudentProportionA(a$nces_g10)
a$grade.11.prop <- MakeStudentProportionA(a$nces_g11)
a$grade.12.prop <- MakeStudentProportionA(a$nces_g12)


# Create proportions of students for each ethnicity out of total enrolled
a$afram.prop  <- MakeStudentProportionA(a$nces_black)
a$aian.prop   <- MakeStudentProportionA(a$nces_am)
a$asian.prop  <- MakeStudentProportionA(a$nces_asian)
a$latino.prop <- MakeStudentProportionA(a$nces_hisp)
a$white.prop  <- MakeStudentProportionA(a$nces_white)


# Create staff response rate for ebs
ebs.y1 <- rep(200405, length(unique(b$school_educationalorgid)))
ebs.y2 <- rep(200506, length(unique(b$school_educationalorgid)))
ebs.y3 <- rep(200607, length(unique(b$school_educationalorgid)))
#ebs.y4 <- rep(200708, length(unique(c$school_educationalorgid)))

ebs.x1 <- cbind(ebs.y1,
                table(b$school_educationalorgid[b$school_schoolyear == 200405]))
ebs.x2 <- cbind(ebs.y2,
                table(b$school_educationalorgid[b$school_schoolyear == 200506]))
ebs.x3 <- cbind(ebs.y3,
                table(b$school_educationalorgid[b$school_schoolyear == 200607]))
#ebs.x4 <- cbind(ebs.y4,
#                table(b$school_educationalorgid[b$school_schoolyear == 200708]))
ebs.x <- rbind(ebs.x1, ebs.x2, ebs.x3)
rm(ebs.y1, ebs.y2, ebs.y3, ebs.x1, ebs.x2, ebs.x3)
#ebs.x <- susp.x[order(c(as.numeric(rownames(ebs.x)), ebs.y1), ]
colnames(ebs.x) <- c("ebs.y1", "ebs.surveys")
ebs.by.school <- merge(a, ebs.x,
                       by.x=c("school_educationalorgid", "school_schoolyear"),
                       by.y=c("row.names", "ebs.y1"), all=TRUE) 
# ebs.by.school has an ebs survey count variable per school per year
rm(ebs.x)
# NOTE: Response rate can be greater than 100, as the nces_fte number can be
# lower than the number of surveys completed...

#mymat <- matrix(data=NA, nrow=172, ncol=1)
#k <- 1
#for (i in 1:66) {
#  for (j in 1:3) {
#    mymat[k, 1] <- ebs.surveys[j, i]
#    k <- k + 1
#  }
#}
#merge.ebs.tests <- cbind(unique(b$school_educationalorgid),
#                   rep(c(200405, 200506, 200607, 200708), 43), mymat)
#merge2 <- merge(a, merge.ebs.tests, by.x=c("school_educationalorgid",
#                "school_schoolyear"), by.y=c("a", "b"))


# Create staff response rate for set
#set.y1 <- rep(200405, length(unique(b$school_educationalorgid)))
#set.y2 <- rep(200506, length(unique(b$school_educationalorgid)))
#set.y3 <- rep(200607, length(unique(b$school_educationalorgid)))
#set.y4 <- rep(200708, length(unique(c$school_educationalorgid)))

#set.x1 <- cbind(set.y1,
#                table(b$school_educationalorgid[b$school_schoolyear == 200405]))
#set.x2 <- cbind(set.y2,
#                table(b$school_educationalorgid[b$school_schoolyear == 200506]))
#set.x3 <- cbind(set.y3,
#                table(b$school_educationalorgid[b$school_schoolyear == 200607]))
#set.x4 <- cbind(set.y4,
#                table(b$school_educationalorgid[b$school_schoolyear == 200708]))
#ebs.x <- rbind(ebs.x1, ebs.x2, ebs.x3)
#rm(set.y1, set.y2, set.y3, set.x1, set.x2, set.x3)
#set.x <- susp.x[order(c(as.numeric(rownames(set.x)), set.y1), ]
#colnames(set.x) <- c("set.y1", "set.surveys")
#set.by.school <- merge(a, set.x,
#                       by.x=c("school_educationalorgid", "school_schoolyear"),
#                       by.y=c("row.names", "set.y1"), all=TRUE) 
# set.by.school has an ebs survey count variable per school per year
#rm(set.x)
# NOTE: Response rate can be greater than 100, as the nces_fte number can be
# lower than the number of surveys completed...


# INDEPENDENT VARIABLE CREATION ################################################
if (pbis.test.mode == "ebs") {
# Create 4 ebs scale variables
b$schoolwide.disc.sys <- NA
b$nonclassroom.set    <- NA
b$classroom.set       <- NA
b$indiv.student.sys   <- NA

# Loop over number of observations in dataset b and add across specified vars
# excluding missing values
for (i in 1:nrow(b)) {
  b$schoolwide.disc.sys[i] <- sum(b$sasurvey_studentexpectationss[i] +
                                  b$sasurvey_behaviorstaughts[i] +
                                  b$sasurvey_behaviorsrewardeds[i] +
                                  b$sasurvey_problembehaviorsdefineds[i] +
                                  b$sasurvey_consequencesdefineds[i] +
                                  b$sasurvey_officeclassroomdistinctionss[i] +
                                  b$sasurvey_optionsexists[i] +
                                  b$sasurvey_emergencyproceduress[i] +
                                  b$sasurvey_behaviorsupportteams[i] +
                                  b$sasurvey_administratoronteams[i] +
                                  b$sasurvey_datasummarizeds[i] +
                                  b$sasurvey_patternsreporteds[i] +
                                  b$sasurvey_informingfamiliess[i] +
                                  b$sasurvey_boostertrainings[i] +
                                  b$sasurvey_supportteambudgets[i] +
                                  b$sasurvey_allstaffinvolveds[i] +
                                  b$sasurvey_trainingfromdistricts[i] +
                                  b$sasurvey_districtreports[i],
                                  na.rm=TRUE)

  b$nonclassroom.set[i] <- sum(b$sasurvey_ncrbehaviorss[i] +
                               b$sasurvey_ncrbehaviorstaughts[i] +
                               b$sasurvey_ncrsupervisions[i] +
                               b$sasurvey_ncrrewardss[i] +
                               b$sasurvey_ncrfeaturesmodifieds[i] +
                               b$sasurvey_ncrmovementschedulings[i] +
                               b$sasurvey_ncrstaffsupervisionskillss[i] +
                               b$sasurvey_ncrbehaviorevaluateds[i] +
                               b$sasurvey_ncrallstaffs[i],
                               na.rm=TRUE)

  b$classroom.set[i] <- sum(b$sasurvey_crbehaviorsdefineds[i] +
                            b$sasurvey_crproblembehaviorss[i] +
                            b$sasurvey_crbehaviorstaughts[i] +
                            b$sasurvey_crbehaviorsrewardeds[i] +
                            b$sasurvey_crconsequencess[i] +
                            b$sasurvey_crproceduress[i] +
                            b$sasurvey_croptionsexists[i] +
                            b$sasurvey_crmaterialsmatchabilitys[i] +
                            b$sasurvey_cracademicsuccesss[i] +
                            b$sasurvey_crteachersassisteds[i] +
                            b$sasurvey_crtransitionsefficients[i],
                            na.rm=TRUE)

  b$indiv.student.sys[i] <- sum(b$sasurvey_indregularassessmentss[i] +
                                b$sasurvey_indrequestassistanceprocesss[i] +
                                b$sasurvey_indbehaviorsupportteams[i] +
                                b$sasurvey_indbehavioralassessments[i] +
                                b$sasurvey_indlocalresourcess[i] +
                                b$sasurvey_indfamilymemberss[i] +
                                b$sasurvey_indfamilytrainings[i] +
                                b$sasurvey_indbehaviormonitoreds[i],
                                na.rm=TRUE)
}

# Validate additions by calculating the ranges of the aggregate variables
range(b$schoolwide.disc.sys, na.rm=TRUE)  # Should be 0-36
range(b$nonclassroom.set,    na.rm=TRUE)  # Should be 0-18
range(b$classroom.set,       na.rm=TRUE)  # Should be 0-22
range(b$indiv.student.sys,   na.rm=TRUE)  # Should be 0-16




ebs.1 <- summarize(b$schoolwide.disc.sys, llist(b$school_schoolyear,
                   b$school_educationalorgid), mean, na.rm=TRUE)
colnames(ebs.1) <- c("school_schoolyear", "school_educationalorgid",
                     "schoolwide.disc.sys")
ebs.2 <- summarize(b$nonclassroom.set, llist(b$school_schoolyear,
                   b$school_educationalorgid), mean, na.rm=TRUE)
colnames(ebs.2) <- c("school_schoolyear", "school_educationalorgid",
                     "nonclassroom.set")
ebs.3 <- summarize(b$classroom.set, llist(b$school_schoolyear,
                   b$school_educationalorgid), mean, na.rm=TRUE)
colnames(ebs.3) <- c("school_schoolyear", "school_educationalorgid",
                     "classroom.set")
ebs.4 <- summarize(b$indiv.student.sys, llist(b$school_schoolyear,
                   b$school_educationalorgid), mean, na.rm=TRUE)
colnames(ebs.4) <- c("school_schoolyear", "school_educationalorgid",
                     "indiv.student.sys")
ebs.merge <- merge(ebs.1, ebs.2,
                   by=c("school_schoolyear", "school_educationalorgid"),
                   all=TRUE)
ebs.merge <- merge(ebs.merge, ebs.3,
                   by=c("school_schoolyear", "school_educationalorgid"),
                   all=TRUE)
ebs.merge <- merge(ebs.merge, ebs.4,
                   by=c("school_schoolyear", "school_educationalorgid"),
                   all=TRUE)
}


# schoolwide discipline system - percent "in place"
b1 <- b[, c(1:3, seq(9, 43, 2))]

for (i in 1:nrow(b1)) {
  b1$newvar[i] <- sum(b1[i, 4:21] == 2, na.rm=TRUE) / 18 * 100 
}

summarize(b1$newvar, llist(school_educationalorgid, school_schoolyear), mean, na.rm=TRUE)


# nonclassroom setting - percent "in place"
b1 <- b[, c(1:3, seq(45, 61, 2))]

for (i in 1:nrow(b1)) {
  b1$newvar[i] <- sum(b1[i, 4:21] == 2, na.rm=TRUE) / 18 * 100 
}

summarize(b1$newvar, llist(school_educationalorgid, school_schoolyear), mean, na.rm=TRUE)


# classroom setting - percent "in place"
b1 <- b[, c(1:3, seq(63, 83, 2))]

for (i in 1:nrow(b1)) {
  b1$newvar[i] <- sum(b1[i, 4:21] == 2, na.rm=TRUE) / 18 * 100 
}

summarize(b1$newvar, llist(school_educationalorgid, school_schoolyear), mean, na.rm=TRUE)


# individual student system - percent "in place"
b1 <- b[, c(1:3, seq(85, 99, 2))]

for (i in 1:nrow(b1)) {
  b1$newvar[i] <- sum(b1[i, 4:21] == 2, na.rm=TRUE) / 18 * 100 
}

summarize(b1$newvar, llist(school_educationalorgid, school_schoolyear), mean, na.rm=TRUE)


# DEPENDENT VARIABLE CREATION ##################################################
# Suspension rates
#schoolyears <- list(200405, 200506, 200607, 200708)
#for (i in c(1:4)) {
#  schoolyears[i]
#  table(c$school_educationalorgid[c$school_schoolyear == schoolyears[i]],
#        c$odr_admindecisionid[c$school_schoolyear == schoolyears[i]])
#}


# Create ODR outcome variables
odr.y1 <- rep(200405, length(unique(c$school_educationalorgid)))
odr.y2 <- rep(200506, length(unique(c$school_educationalorgid)))
odr.y3 <- rep(200607, length(unique(c$school_educationalorgid)))
#odr.y4 <- rep(200708, 42)

odr.x1 <- cbind(odr.y1,
                 table(c$school_educationalorgid[c$school_schoolyear == 200405],
                       c$odr_admindecisionid[c$school_schoolyear == 200405]))
odr.x2 <- cbind(odr.y2,
                 table(c$school_educationalorgid[c$school_schoolyear == 200506],
                       c$odr_admindecisionid[c$school_schoolyear == 200506]))
odr.x3 <- cbind(odr.y3,
                 table(c$school_educationalorgid[c$school_schoolyear == 200607],
                       c$odr_admindecisionid[c$school_schoolyear == 200607]))
#odr.x4 <- cbind(odr.y4,
#                 table(c$school_educationalorgid[c$school_schoolyear == 200708],
#                       c$odr_admindecisionid[c$school_schoolyear == 200708]))
#odr.x <- rbind(odr.x1, odr.x2, odr.x3, odr.x4)
#rm(odr.y1, odr.y2, odr.y3, odr.y4, odr.x1, odr.x2, odr.x3, odr.x4)
odr.x <- rbind(odr.x1, odr.x2, odr.x3)
rm(odr.y1, odr.y2, odr.y3, odr.x1, odr.x2, odr.x3)
#susp.x <- odr.x[order(c(as.numeric(rownames(odr.x)), odr.y1), ]
colnames(odr.x) <- c("odr.y1", "time_in_office", "loss_of_priv",
                      "student_conf", "parent_contact", "detention",
                      "indiv_instruc", "iss", "oss", "sat_school", "expulsion",
                      "other", "unknown", "bus_susp", "restitution")
odr.by.school <- merge(a, odr.x,
                       by.x=c("school_educationalorgid", "school_schoolyear"),
                       by.y=c("row.names", "odr.y1"), all=TRUE) 
# odr.by.school has separate odr count variables per school per year
rm(odr.x)


#odr.by.school$ <- 
c$odr_problembehaviorismajor <- ifelse(c$odr_problembehaviorisminor == 0, 1, 0)


c1 <- c[c$odr_admindecisionid == 8, ]

susp.sums <- summarize(c1$odr_admindecisionid,
                       llist(c1$school_educationalorgid, c1$school_schoolyear),
                       sum, na.rm=TRUE)
names(susp.sums) <- c("school_educationalorgid", "school_schoolyear", "oss")
a <- merge(a, susp.sums, by="school_educationalorgid", all=TRUE)


c2 <- c[c$odr_admindecisionid == 10, ]

expulsion.sums <- summarize(c2$odr_admindecisionid,
                       llist(c2$school_educationalorgid, c2$school_schoolyear),
                       sum, na.rm=TRUE)
names(expulsion.sums) <- c("school_educationalorgid", "school_schoolyear",
                           "expulsions")
a <- merge(a, expulsion.sums, by="school_educationalorgid", all=TRUE)



c3 <- c[c$odr_admindecisionid == 10, ]

major.odr.sums <- summarize(c3$odr_admindecisionid,
                       llist(c3$school_educationalorgid, c3$school_schoolyear),
                       sum, na.rm=TRUE)
names(major.odr.sums) <- c("school_educationalorgid", "school_schoolyear",
                           "odr_problembehaviorismajor")
a <- merge(a, major.odr.sums, by="school_educationalorgid", all=TRUE)


### Calculate # of FTE per 100 students
a$fte.per.student <- a$nces_fte / a$nces_member * 100

### Calculate % Free and Reduced Price Meal students
a$percent.frpm <- a$nces_totfrl / a$nces_member * 100



### Number of major ODRs per 100 students per day
a$major.odr.100 <- (a$odr_problembehaviorismajor / a$nces_member) * 180



# ADDITIONAL VARIABLE CREATION #################################################
# Create total instruction time
#diff in num of ODRs pre-post multiplied by time factor found in Muscott article

# Create leadership/admin time
#diff in num of ODRs pre-post multiplied by time factor found in Muscott article

# Create teacher time
#diff in num of ODRs pre-post multiplied by time factor found in Muscott article


# MERGE DATA FOR MULTIVARIABLE ANALYSIS 
lm.data <- merge(a, ebs.merge,
                 by=c("school_educationalorgid", "school_schoolyear"),
                 all=TRUE)
#lm.data <- merge(lm.data, odr.merge,
#                 by=llist("b$school_educationalorgid", "b$school_schoolyear"),
#                 all=TRUE)



save.image(file="/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/data/workspaces/pbis_cleaned.RData")

#sink()
