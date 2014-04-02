#sink(file="/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/results/pbis_cleaning_output.txt")  # send output to file

# PBIS Cleaning File
# By Casey Tsui
# Format: R
# Last updated: 10/4/10


# Author Comment: This file cleans the variables of interest.
  # This is where all the ugly stuff lives.
  # Takes care of missing values, merging data frames, handling outliers. 


setwd("/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/coding")
source("pbis_load.r")
library(Hmisc) # Hmisc needed to change variable names to lowercase


# PRELIMINARY CLEANING AND RECODING ############################################
# Convert all variable names to lowercase
#schools <- upData(schools, lowernames=TRUE)

# ebs schools
ebs.data <- lapply(ebs.data, upData, lowernames=TRUE)

# set schools
set.data <- lapply(set.data, upData, lowernames=TRUE)


# CHOOSE WHICH FIDELITY MEASURE AND YEAR SPAN
  # INDEX 1 = Fidelity Measure Data 3-year
  # INDEX 2 = ODR Data 3-year
  # INDEX 3 = Fidelity Measure Data 4-year
  # INDEX 4 = ODR Data 4-year
schools.and.surveys <- ebs.data[[1]]
odrs <- ebs.data[[2]]

# Remove other data sets to increase comp speed
rm(ebs.data, set.data)

# SAVE ALL SCHOOL-LEVEL VARIABLES AS OBJECT "A"
a <- unique(schools.and.surveys[, -c(2, 11:107)])
a[a == "NULL"] <- NA  # Change NULLs to NAs

# DROP MIXED SCHOOLS THAT DON'T SERVE AT LEAST 6TH GRADE
a <- a[-which(a$nces_gshi == "05"), ]

# Drop unused factor levels
for (i in 1:ncol(a)) {
  if (is.factor(a[, i]) == TRUE)
    a[, i] <- factor(a[, i])
}

# Convert variables to numeric from factor
for (i in c(10:12, 14:17, 20:ncol(a))) {
  a[, i] <- as.numeric(as.character(a[, i]))
}


# SAVE ALL FIDELITY MEASURE VARIABLES AS OBJECT "B"
b <- schools.and.surveys[, c(1:3, 11:107)]
b[b == "NULL"] <- NA  # Change NULLs to NAs

# Drop unused factor levels
for (i in 1:ncol(b)) {
  if (is.factor(b[, i]) == TRUE)
    b[, i] <- factor(b[, i])
}

# Convert variables to numeric from factor
for (i in 9:ncol(b)) {
  b[, i] <- as.numeric(as.character(b[, i]))
}


# SAVE ALL ODR VARIABLES AS OBJECT "C"
c <- odrs[, c(1:3, 7:24)]
c[c == "NULL"] <- NA  # Change NULLs to NAs

# Drop unused factor levels
for (i in 1:ncol(c)) {
  if (is.factor(c[, i]) == TRUE)
    c[, i] <- factor(c[, i])
}

# Recode days suspended and expelled as numeric
for (i in 20:21) {
  c[, i] <- as.numeric(as.character(c[, i]))
}


# Remove initial datasets to free up memory and increase computational speed
rm(schools.and.surveys, odrs)


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
a <- merge(a, g, by.x="school_educationalorgid", by.y="row.names")
a <- a[, -c(62:64)]
b <- merge(b, g, by.x="school_educationalorgid", by.y="row.names")
b <- b[, -c(101:103)]
c <- merge(c, g, by.x="school_educationalorgid", by.y="row.names")
c <- c[, -c(22:24)]
rm(g)


# DESCRIPTIVE VARIABLE CREATION ################################################
# Year
a$year <- ifelse(a$school_schoolyear == 200405,
                 1,
                 ifelse(a$school_schoolyear == 200506,
                        2,
                        ifelse(a$school_schoolyear == 200607,
                               3,
                               4)))

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
a$female.prop <- a$nces_femaleenrollment / a$nces_member
a$male.prop   <- a$nces_maleenrollment   / a$nces_member


# Create proportions of students for each grade out of total enrolled
a$grade.06.prop <- a$nces_g06 / a$nces_member
a$grade.07.prop <- a$nces_g07 / a$nces_member
a$grade.08.prop <- a$nces_g08 / a$nces_member
a$grade.09.prop <- a$nces_g09 / a$nces_member
a$grade.10.prop <- a$nces_g10 / a$nces_member
a$grade.11.prop <- a$nces_g11 / a$nces_member
a$grade.12.prop <- a$nces_g12 / a$nces_member


# Create proportions of students for each ethnicity out of total enrolled
a$afram.prop  <- a$nces_black / a$nces_member
a$aian.prop   <- a$nces_am    / a$nces_member
a$asian.prop  <- a$nces_asian / a$nces_member
a$latino.prop <- a$nces_hisp  / a$nces_member
a$white.prop  <- a$nces_white / a$nces_member


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


# DEPENDENT VARIABLE CREATION ##################################################
# Suspension rates
#schoolyears <- list(200405, 200506, 200607, 200708)
#for (i in c(1:4)) {
#  schoolyears[i]
#  table(c$school_educationalorgid[c$school_schoolyear == schoolyears[i]],
#        c$odr_admindecisionid[c$school_schoolyear == schoolyears[i]])
#}


# Create ODR outcome variables
susp.y1 <- rep(200405, length(unique(c$school_educationalorgid)))
susp.y2 <- rep(200506, length(unique(c$school_educationalorgid)))
susp.y3 <- rep(200607, length(unique(c$school_educationalorgid)))
#susp.y4 <- rep(200708, 42)

susp.x1 <- cbind(susp.y1,
                 table(c$school_educationalorgid[c$school_schoolyear == 200405],
                       c$odr_admindecisionid[c$school_schoolyear == 200405]))
susp.x2 <- cbind(susp.y2,
                 table(c$school_educationalorgid[c$school_schoolyear == 200506],
                       c$odr_admindecisionid[c$school_schoolyear == 200506]))
susp.x3 <- cbind(susp.y3,
                 table(c$school_educationalorgid[c$school_schoolyear == 200607],
                       c$odr_admindecisionid[c$school_schoolyear == 200607]))
#susp.x4 <- cbind(susp.y4,
#                 table(c$school_educationalorgid[c$school_schoolyear == 200708],
#                       c$odr_admindecisionid[c$school_schoolyear == 200708]))
#susp.x <- rbind(susp.x1, susp.x2, susp.x3, susp.x4)
#rm(susp.y1, susp.y2, susp.y3, susp.y4, susp.x1, susp.x2, susp.x3, susp.x4)
susp.x <- rbind(susp.x1, susp.x2, susp.x3)
rm(susp.y1, susp.y2, susp.y3, susp.x1, susp.x2, susp.x3)
#susp.x <- susp.x[order(c(as.numeric(rownames(susp.x)), susp.y1), ]
colnames(susp.x) <- c("susp.y1", "time_in_office", "loss_of_priv",
                      "student_conf", "parent_contact", "detention",
                      "indiv_instruc", "iss", "oss", "sat_school", "expulsion",
                      "other", "unknown", "bus_susp", "restitution")
odr.by.school <- merge(a, susp.x,
                       by.x=c("school_educationalorgid", "school_schoolyear"),
                       by.y=c("row.names", "susp.y1"), all=TRUE) 
# odr.by.school has separate odr count variables per school per year
rm(susp.x)


#odr.by.school$ <- 
c$odr_problembehaviorismajor <- ifelse(c$odr_problembehaviorisminor == 0, 1, 0)



# ADDITIONAL VARIABLE CREATION #################################################
# Create total instruction time
#diff in num of ODRs pre-post multiplied by time factor found in Muscott article

# Create leadership/admin time
#diff in num of ODRs pre-post multiplied by time factor found in Muscott article

# Create teacher time
#diff in num of ODRs pre-post multiplied by time factor found in Muscott article


# MERGE DATA FOR MULTIVARIABLE ANALYSIS 
#lm.data <- 


#sink()
