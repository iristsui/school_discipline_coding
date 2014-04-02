# PBIS Cleaning File
# By Casey Tsui
# Format: R
# Last updated: 4/29/11


# Author Comment: This file cleans the variables of interest.
#   Takes care of missing values, merging data frames, handling outliers. 
#   Creates the "lm.data" object that will be used in pbis_do.r for all analyses


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
                                                     ordered=TRUE))))
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


# Create counts of total odrs and oss by race
CreateOdrPercsByRace <- function(x) {
  odrx <- x     # This should be your odr-by-student data

  race.list <- vector("list", 7)

  # Create counts of african american total odrs and oss
  afram.odrs <- subset(odrx, student_ethnicitylabel == "Black")
  race.list[[1]] <- ddply(afram.odrs,
                     c("school_educationalorgid", "school_schoolyear"),
                       function(df)
                         c(afram.odr = length(df$student_ethnicitylabel),
                           afram.major = length(df$student_ethnicitylabel[which(df$odr_problembehaviorismajor == 1)]),
                           afram.oss = length(df$student_ethnicitylabel[which(df$odr_admindecisionid == 8)])))

  # Create counts of asian total odrs and oss
  asian.odrs <- subset(odrx, student_ethnicitylabel == "Asian")
  race.list[[2]] <- ddply(asian.odrs, c("school_educationalorgid", "school_schoolyear"),
                     function(df)
                       c(asian.odr = length(df$student_ethnicitylabel),
                         asian.major = length(df$student_ethnicitylabel[which(df$odr_problembehaviorismajor == 1)]),
                         asian.oss = length(df$student_ethnicitylabel[which(df$odr_admindecisionid == 8)])))

  # Create counts of latino total odrs and oss
  latino.odrs <- subset(odrx, student_ethnicitylabel == "Hispanic/Latino")
  race.list[[3]] <- ddply(latino.odrs, c("school_educationalorgid", "school_schoolyear"),
                     function(df)
                       c(latino.odr = length(df$student_ethnicitylabel),
                         latino.major = length(df$student_ethnicitylabel[which(df$odr_problembehaviorismajor == 1)]),
                         latino.oss = length(df$student_ethnicitylabel[which(df$odr_admindecisionid == 8)])))


  # Create counts of white total odrs and oss
  white.odrs <- subset(odrx, student_ethnicitylabel == "White")
  race.list[[4]] <- ddply(white.odrs, c("school_educationalorgid", "school_schoolyear"),
                     function(df)
                       c(white.odr = length(df$student_ethnicitylabel),
                         white.major = length(df$student_ethnicitylabel[which(df$odr_problembehaviorismajor == 1)]),
                         white.oss = length(df$student_ethnicitylabel[which(df$odr_admindecisionid == 8)])))

  race.list[[5]] <- odr
  race.list[[6]] <- major
  race.list[[7]] <- oss

  # Merge african american and latino objects
  races.odr <- Reduce(function(x, y)
                      merge(x, y, all=TRUE,
                            by=c("school_educationalorgid", "school_schoolyear")),
                            race.list, accumulate=FALSE)

  races.odr <- transform(races.odr,
                         afram.odr.perc = afram.odr / odr * 100,
                         asian.odr.perc = asian.odr / odr * 100,
                         latino.odr.perc = latino.odr / odr * 100,
                         white.odr.perc = white.odr / odr * 100,
                         afram.major.perc = afram.major / major * 100,
                         asian.major.perc = asian.major / major * 100,
                         latino.major.perc = latino.major / major * 100,
                         white.major.perc = white.major / major * 100,
                         afram.oss.perc = afram.oss / oss * 100,
                         asian.oss.perc = asian.oss / oss * 100,
                         latino.oss.perc = latino.oss / oss * 100,
                         white.oss.perc = white.oss / oss * 100)
  races.odr <- transform(races.odr,
                         odr = NULL,
                         major = NULL,
                         oss = NULL)
  return(races.odr)
}

CreateRacialOdrRatios <- function(x) {
  df <- x
  a.names <- c("school_educationalorgid", "school_schoolyear", "nces_member",
               "afram.perc", "asian.perc", "latino.perc", "white.perc",
               "frpm.perc")
  l <- a[, a.names]
  df <- merge(df, l,
              by=c("school_educationalorgid", "school_schoolyear"),
              all.x=TRUE)

  df <- transform(df,
                  afram.odr.ratio = afram.odr.perc / afram.perc,
                  asian.odr.ratio = asian.odr.perc / asian.perc,
                  latino.odr.ratio = latino.odr.perc / latino.perc,
                  white.odr.ratio = white.odr.perc / white.perc,

                  afram.major.ratio = afram.major.perc / afram.perc,
                  asian.major.ratio = asian.major.perc / asian.perc,
                  latino.major.ratio = latino.major.perc / latino.perc,
                  white.major.ratio = white.major.perc / white.perc,

                  nonafram.major.ratio = (100 - afram.major.perc) / (100 - afram.perc),
                  nonasian.major.ratio = (100 - asian.major.perc) / (100 - asian.perc),
                  nonlatino.major.ratio = (100 - latino.major.perc) / (100 - latino.perc),
                  nonwhite.major.ratio = (100 - white.major.perc) / (100 - white.perc),

                  afram.oss.ratio = afram.oss.perc / afram.perc,
                  asian.oss.ratio = asian.oss.perc / asian.perc,
                  latino.oss.ratio = latino.oss.perc / latino.perc,
                  white.oss.ratio = white.oss.perc / white.perc,

                  nonafram.oss.ratio = (100 - afram.oss.perc) / (100 - afram.perc),
                  nonasian.oss.ratio = (100 - asian.oss.perc) / (100 - asian.perc),
                  nonlatino.oss.ratio = (100 - latino.oss.perc) / (100 - latino.perc),
                  nonwhite.oss.ratio = (100 - white.oss.perc) / (100 - white.perc))

  df$afram.odr.ratio[which(df$afram.odr.ratio == Inf)] <- NA
  df$afram.major.ratio[which(df$afram.major.ratio == Inf)] <- NA
  df$asian.odr.ratio[which(df$asian.odr.ratio == Inf)] <- NA

  df$asian.oss.ratio[which(df$asian.oss.ratio == Inf)] <- NA

  df <- transform(df,
              afram.major.over.white = afram.major.ratio / white.major.ratio,
              asian.major.over.white = asian.major.ratio / white.major.ratio,
              latino.major.over.white = latino.major.ratio / white.major.ratio,
              white.major.over.white = white.major.ratio / white.major.ratio,
              afram.major.disp.index = afram.major.ratio / nonafram.major.ratio,
              asian.major.disp.index = asian.major.ratio / nonasian.major.ratio,
              latino.major.disp.index = latino.major.ratio / nonlatino.major.ratio,
              white.major.disp.index = white.major.ratio / nonwhite.major.ratio,
              afram.oss.disp.index = afram.oss.ratio / nonafram.oss.ratio,
              asian.oss.disp.index = asian.oss.ratio / nonasian.oss.ratio,
              latino.oss.disp.index = latino.oss.ratio / nonlatino.oss.ratio,
              white.oss.disp.index = white.oss.ratio / nonwhite.oss.ratio)

  df$latino.oss.disp.index[which(df$latino.oss.disp.index == Inf)] <- NA

  df <- transform(df,
        nces_member = NULL,
        afram.perc = NULL,
        asian.perc = NULL,
        latino.perc = NULL,
        white.perc = NULL,
        frpm.perc = NULL)
  return(df)
}


CreateRacialDispDataWide <- function(x) {
  dat <- x
  race.wide <- ddply(dat, "school_schoolyear", function(df)
       c(afram.major.disp.index = mean(df$afram.major.disp.index, na.rm=TRUE),
         afram.oss.disp.index = mean(df$afram.oss.disp.index, na.rm=TRUE),
         afram.major.over.white = mean(df$afram.major.over.white, na.rm=TRUE),
         afram.major.ratio = mean(df$afram.major.ratio, na.rm=TRUE),
         afram.odr.ratio = mean(df$afram.odr.ratio, na.rm=TRUE),
         afram.oss.ratio = mean(df$afram.oss.ratio, na.rm=TRUE),
         asian.major.disp.index = mean(df$asian.major.disp.index, na.rm=TRUE),
         asian.oss.disp.index = mean(df$asian.oss.disp.index, na.rm=TRUE),
         asian.major.over.white = mean(df$asian.major.over.white, na.rm=TRUE),
         asian.major.ratio = mean(df$asian.major.ratio, na.rm=TRUE),
         asian.odr.ratio = mean(df$asian.odr.ratio, na.rm=TRUE),
         asian.oss.ratio = mean(df$asian.oss.ratio, na.rm=TRUE),
         latino.major.disp.index = mean(df$latino.major.disp.index, na.rm=TRUE),
         latino.oss.disp.index = mean(df$latino.oss.disp.index, na.rm=TRUE),
         latino.major.over.white = mean(df$latino.major.over.white, na.rm=TRUE),
         latino.major.ratio = mean(df$latino.major.ratio, na.rm=TRUE),
         latino.odr.ratio = mean(df$latino.odr.ratio, na.rm=TRUE),
         latino.oss.ratio = mean(df$latino.oss.ratio, na.rm=TRUE),
         white.major.disp.index = mean(df$white.major.disp.index, na.rm=TRUE),
         white.oss.disp.index = mean(df$white.oss.disp.index, na.rm=TRUE),
         white.major.over.white = mean(df$white.major.over.white, na.rm=TRUE),
         white.major.ratio = mean(df$white.major.ratio, na.rm=TRUE),
         white.odr.ratio = mean(df$white.odr.ratio, na.rm=TRUE),
         white.oss.ratio = mean(df$white.oss.ratio, na.rm=TRUE)))
  return(race.wide)
}


ReshapeRacialDispDataToLong <- function(x) {
  dat <- x
  race.long <- reshape(dat,
                       direction="long",
                       idvar="school_schoolyear",
                       varying=c(2:length(races.odr.y)),
                       v.names=c("major.disp.index", "oss.disp.index",
                                 "major.over.white", "major.ratio",
                                 "odr.ratio", "oss.ratio"),
                       times=c("African American", "Asian", "Latino", "White"),
                       timevar="race")
  return(race.long)
}


SyncDatasets <- function(x, y) {
  df <- x
  ref <- y
  return(df[which(df$school_educationalorgid %in% ref$school_educationalorgid), ])
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

# Sync datasets
b <- SyncDatasets(b, a)
c <- SyncDatasets(c, a)
d <- SyncDatasets(d, a)


# Create 3-category grade type ID
a$grade.type <- with(a, ifelse((nces_gslo == "06" | nces_gslo == "07") &
                                nces_gshi == "8",
                                "MS",            # 1=all middle 6-8 or 7-8
                                ifelse(nces_gslo == "09" & nces_gshi == "12",
                                       "HS",     # 2=all high schools 9-12
                                       "Mix")))  # 3=mixed schools

a$grade.type <- factor(a$grade.type, levels=c("MS", "HS", "Mix"), ordered=TRUE)
a$high.school <- ifelse(a$grade.type == "HS", 1, 0)


# Keep only those schools that maintained grade type through all years
a <- a[-which(a$school_ncesschoolid == "174169005293"), ]  # Gordon Gregory Middle School switched to grade 5-8 in 2005-06 only, so drop it
a.names.grade <- c("school_educationalorgid", "school_schoolyear", "grade.type")
a.unique.grade <- unique(a[a.names.grade])
g <- with(a.unique.grade, table(school_educationalorgid, grade.type))
rm(a.names.grade, a.unique.grade)
g <- g[(g[, 1] >= 3 | g[, 2] >= 3 | g[, 3] >= 3), ]
a <- a[a$school_educationalorgid %in% row.names(g), ]
b <- b[b$school_educationalorgid %in% row.names(g), ]
c <- c[c$school_educationalorgid %in% row.names(g), ]
d <- d[d$school_educationalorgid %in% row.names(g), ]



# Drop schools that don't have ODR data
a <- SyncDatasets(a, d)
b <- SyncDatasets(b, d)
c <- SyncDatasets(c, d)

# Drop schools that have only 1 year of ODR data
d.sub <- unique(d[, c("school_educationalorgid", "school_schoolyear")])
d.sub <- ddply(d.sub, c("school_educationalorgid"),
                 function(df)
                   c(odr.number.years = length(df$school_schoolyear)))
d.sub <- subset(d.sub, odr.number.years >= 2, select = school_educationalorgid)
d <- SyncDatasets(d, d.sub)
rm(d.sub)
a <- SyncDatasets(a, d)
b <- SyncDatasets(b, d)
c <- SyncDatasets(c, d)


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

# Drop mixed schools
a <- a[-which(a$grade.type=="Mix"), ]
b <- SyncDatasets(b, a)
c <- SyncDatasets(c, a)
d <- SyncDatasets(d, a)
 
a <- FactorVariablesAll(a)
b <- FactorVariablesAll(b)
c <- FactorVariablesAll(c)
d <- FactorVariablesAll(d)




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
               stud.per.fte    = nces_member / nces_fte,
               frpm.prop       = MakeStudentProportionA(nces_totfrl),
               year            = as.numeric(school_schoolyear))

a <- transform(a,
               female.perc     = female.prop * 100,
               male.perc       = male.prop * 100,
               afram.perc      = afram.prop * 100,
               aian.perc       = asian.prop * 100,
               asian.perc      = asian.prop * 100,
               latino.perc     = latino.prop * 100,
               white.perc      = white.prop * 100,
               frpm.perc       = frpm.prop * 100)

# INDEPENDENT VARIABLE CREATION ################################################
# EBS ##########################################################################
# Create sum of EBS scores
b <- b[, c(1:9, seq(10, 100, 2), 102)]

sw.sub  <- SubsetByNames(b, c("sasurvey_studentexpectationss",
                              "sasurvey_districtreports"))
ncr.sub <- SubsetByNames(b, c("sasurvey_ncrbehaviorss",
                              "sasurvey_ncrallstaffs"))
cr.sub  <- SubsetByNames(b, c("sasurvey_crbehaviorsdefineds",
                              "sasurvey_crtransitionsefficients"))
ind.sub <- SubsetByNames(b, c("sasurvey_indregularassessmentss",
                              "sasurvey_indbehaviormonitoreds"))

b <- transform(b,
               sw.disc.sys  = rowSums(sw.sub, na.rm=TRUE),
               ncr.set      = rowSums(ncr.sub, na.rm=TRUE),
               cr.set       = rowSums(cr.sub, na.rm=TRUE),
               ind.stud.sys = rowSums(ind.sub, na.rm=TRUE))


# Validate additions by calculating the ranges of the aggregate variables
r1 <- range(b$sw.disc.sys,  na.rm=TRUE)  # Should be 0-36
r2 <- range(b$ncr.set,      na.rm=TRUE)  # Should be 0-18
r3 <- range(b$cr.set,       na.rm=TRUE)  # Should be 0-22
r4 <- range(b$ind.stud.sys, na.rm=TRUE)  # Should be 0-16

# Calculate percentage of EBS scores that are marked "In Place"
#   Includes partial in place scores
b <- transform(b,
               perc.in.place.sw.disc.sys  = sw.disc.sys  / r1[2] * 100,
               perc.in.place.ncr.set      = ncr.set      / r2[2] * 100,
               perc.in.place.cr.set       = cr.set       / r3[2] * 100,
               perc.in.place.ind.stud.sys = ind.stud.sys / r4[2] * 100)


ebs.scores <- ddply(b, c("school_educationalorgid", "school_schoolyear"),
                    function(df)
                      c(perc.in.place.sw.disc.sys = mean(df$perc.in.place.sw.disc.sys, na.rm=TRUE),
                        perc.in.place.ncr.set = mean(df$perc.in.place.ncr.set, na.rm=TRUE),
                        perc.in.place.cr.set = mean(df$perc.in.place.cr.set, na.rm=TRUE),
                        perc.in.place.ind.stud.sys = mean(df$perc.in.place.ind.stud.sys, na.rm=TRUE)))
ebs.scores <- transform(ebs.scores,
                ebs.implementationaverage = rowMeans(ebs.scores[, c(3:6)],
                                                     na.rm=TRUE),
                ebs.criterion8080 = ifelse(perc.in.place.sw.disc.sys >= 80 &
                                           perc.in.place.ncr.set >= 80,
                                           1,
                                           0))




# SET ##########################################################################
c$set_implementationaverage <- rowMeans(c[, 9:15], na.rm=TRUE)

set.scores <- unique(c[, c(1, 3, 9:15, 48:50)])
set.scores <- ddply(set.scores,
   c("school_educationalorgid", "school_schoolyear"),
   function(df)
     c(set.expectationsdefined = mean(df$set_expectationsdefined, na.rm=TRUE) * 100,
       set.expectationstaught = mean(df$set_expectationstaught, na.rm=TRUE) * 100,
       set.rewardsystem = mean(df$set_rewardsystem, na.rm=TRUE) * 100,
       set.violationssystem = mean(df$set_violationssystem, na.rm=TRUE) * 100,
       set.monitoringevaluation = mean(df$set_monitoringevaluation, na.rm=TRUE) * 100,
       set.leadership = mean(df$set_leadership, na.rm=TRUE) * 100,
       set.districtsupport = mean(df$set_districtsupport, na.rm=TRUE) * 100,
       set.implementationaverage = mean(df$set_implementationaverage, na.rm=TRUE) * 100))

set.scores <- transform(set.scores,
       set.criterion8080 = ifelse(set.implementationaverage >= 80 &
                                  set.expectationstaught >= 80,
                                           1,
                                           0))



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

oss.odr.afram <- subset(oss.odr, student_ethnicitylabel == "Black")
oss.afram <- ddply(oss.odr.afram,
                     c("school_educationalorgid", "school_schoolyear"),
                       function(df) 
                         c(oss.afram = length(df$odr_admindecisionid)))
oss.odr.asian <- subset(oss.odr, student_ethnicitylabel == "Asian")
oss.asian <- ddply(oss.odr.asian,
                     c("school_educationalorgid", "school_schoolyear"),
                       function(df) 
                         c(oss.asian = length(df$odr_admindecisionid)))
oss.odr.latino <- subset(oss.odr, student_ethnicitylabel == "Hispanic/Latino")
oss.latino <- ddply(oss.odr.latino,
                     c("school_educationalorgid", "school_schoolyear"),
                       function(df) 
                         c(oss.latino = length(df$odr_admindecisionid)))
oss.odr.white <- subset(oss.odr, student_ethnicitylabel == "White")
oss.white <- ddply(oss.odr.white,
                     c("school_educationalorgid", "school_schoolyear"),
                       function(df) 
                         c(oss.white = length(df$odr_admindecisionid)))
rm(list=ls()[which(substr(ls(), 1, 7) == "oss.odr")])

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

major.odr.afram <- subset(major.odr, student_ethnicitylabel == "Black")
major.afram <- ddply(major.odr.afram,
                     c("school_educationalorgid", "school_schoolyear"),
                       function(df) 
                         c(major.afram = length(df$odr_problembehaviorismajor)))
major.odr.asian <- subset(major.odr, student_ethnicitylabel == "Asian")
major.asian <- ddply(major.odr.asian,
                     c("school_educationalorgid", "school_schoolyear"),
                       function(df) 
                         c(major.asian = length(df$odr_problembehaviorismajor)))
major.odr.latino <- subset(major.odr, student_ethnicitylabel == "Hispanic/Latino")
major.latino <- ddply(major.odr.latino,
                     c("school_educationalorgid", "school_schoolyear"),
                       function(df) 
                         c(major.latino = length(df$odr_problembehaviorismajor)))
major.odr.white <- subset(major.odr, student_ethnicitylabel == "White")
major.white <- ddply(major.odr.white,
                     c("school_educationalorgid", "school_schoolyear"),
                       function(df) 
                         c(major.white = length(df$odr_problembehaviorismajor)))
rm(list=ls()[which(substr(ls(), 1, 9) == "major.odr")])


# Calculate number of total (major+minor) ODRs per school per year
odr <- ddply(d, c("school_educationalorgid", "school_schoolyear"),
             function(df)
               c(odr = length(df$odr_problembehaviorismajor)))


# Real recidivism rate = percent of students who have more than 1 ODR, among
# students that have at least 1 ODR
recid.odr <- ddply(d[which(d$odr_problembehaviorismajor == 1), ],
                   c("school_educationalorgid", "school_schoolyear",
                     "student_studentid"),
                   function(df) 
                     c(recid.odr = length(df$student_studentid)))
recid.odr$recid.tag <- ifelse(recid.odr$recid.odr > 1, 1, 0)
recid.count <- ddply(recid.odr,
                    c("school_educationalorgid", "school_schoolyear"),
                    function(df)
                      c(recid.count = sum(df$recid.tag)))
# Counts number of students with at least one ODR
num.stud <- ddply(recid.odr, c("school_educationalorgid", "school_schoolyear"),
                  function(df) 
                    c(num.stud = length(df$student_studentid)))
recid <- merge(recid.count, num.stud, all=TRUE)
recid$recid <- with(recid, recid.count / num.stud)
recid$recid.count <- NULL
recid$num.stud <- NULL


# African American Recidivism
recid.odr.afram <- ddply(d[which(d$odr_problembehaviorismajor == 1 & 
                                 d$student_ethnicitylabel=="Black"), ],
                         c("school_educationalorgid", "school_schoolyear",
                           "student_studentid"),
                    function(df) 
                      c(recid.odr.afram = length(df$student_studentid)))
recid.odr.afram$recid.tag.afram <- ifelse(recid.odr.afram$recid.odr.afram > 1, 1, 0)
recid.count.afram <- ddply(recid.odr.afram,
                    c("school_educationalorgid", "school_schoolyear"),
                    function(df)
                      c(recid.count.afram = sum(df$recid.tag.afram)))
# Counts number of students with at least one ODR
num.stud.afram <- ddply(recid.odr.afram,
                        c("school_educationalorgid", "school_schoolyear"),
                        function(df) 
                        c(num.stud.afram = length(df$student_studentid)))
recid.afram <- merge(recid.count.afram, num.stud.afram, all=TRUE)
recid.afram$recid.afram <- with(recid.afram, recid.count.afram / num.stud.afram)
recid.afram$recid.count.afram <- NULL
recid.afram$num.stud.afram <- NULL


# Asian Recidivism
recid.odr.asian <- ddply(d[which(d$odr_problembehaviorismajor == 1 &
                                 d$student_ethnicitylabel=="Asian"), ],
                         c("school_educationalorgid", "school_schoolyear",
                           "student_studentid"),
                    function(df) 
                      c(recid.odr.asian = length(df$student_studentid)))
recid.odr.asian$recid.tag.asian <- ifelse(recid.odr.asian$recid.odr.asian > 1, 1, 0)
recid.count.asian <- ddply(recid.odr.asian,
                    c("school_educationalorgid", "school_schoolyear"),
                    function(df)
                      c(recid.count.asian = sum(df$recid.tag.asian)))
# Counts number of students with at least one ODR
num.stud.asian <- ddply(recid.odr.asian,
                        c("school_educationalorgid", "school_schoolyear"),
                        function(df) 
                        c(num.stud.asian = length(df$student_studentid)))
recid.asian <- merge(recid.count.asian, num.stud.asian, all=TRUE)
recid.asian$recid.asian <- with(recid.asian, recid.count.asian / num.stud.asian)
recid.asian$recid.count.asian <- NULL
recid.asian$num.stud.asian <- NULL


# Latino Recidivism
recid.odr.latino <- ddply(d[which(d$odr_problembehaviorismajor == 1 &
                                  d$student_ethnicitylabel=="Hispanic/Latino"), ],
                         c("school_educationalorgid", "school_schoolyear",
                           "student_studentid"),
                    function(df) 
                      c(recid.odr.latino = length(df$student_studentid)))
recid.odr.latino$recid.tag.latino <- ifelse(recid.odr.latino$recid.odr.latino > 1, 1, 0)
recid.count.latino <- ddply(recid.odr.latino,
                    c("school_educationalorgid", "school_schoolyear"),
                    function(df)
                      c(recid.count.latino = sum(df$recid.tag.latino)))
# Counts number of students with at least one ODR
num.stud.latino <- ddply(recid.odr.latino,
                        c("school_educationalorgid", "school_schoolyear"),
                        function(df) 
                        c(num.stud.latino = length(df$student_studentid)))
recid.latino <- merge(recid.count.latino, num.stud.latino, all=TRUE)
recid.latino$recid.latino <- with(recid.latino, recid.count.latino / num.stud.latino)
recid.latino$recid.count.latino <- NULL
recid.latino$num.stud.latino <- NULL


# White Recidivism
recid.odr.white <- ddply(d[which(d$odr_problembehaviorismajor == 1 &
                                 d$student_ethnicitylabel=="White"), ],
                         c("school_educationalorgid", "school_schoolyear",
                           "student_studentid"),
                    function(df) 
                      c(recid.odr.white = length(df$student_studentid)))
recid.odr.white$recid.tag.white <- ifelse(recid.odr.white$recid.odr.white > 1, 1, 0)
recid.count.white <- ddply(recid.odr.white,
                    c("school_educationalorgid", "school_schoolyear"),
                    function(df)
                      c(recid.count.white = sum(df$recid.tag.white)))
# Counts number of students with at least one ODR
num.stud.white <- ddply(recid.odr.white,
                        c("school_educationalorgid", "school_schoolyear"),
                        function(df) 
                        c(num.stud.white = length(df$student_studentid)))
recid.white <- merge(recid.count.white, num.stud.white, all=TRUE)
recid.white$recid.white <- with(recid.white, recid.count.white / num.stud.white)
recid.white$recid.count.white <- NULL
recid.white$num.stud.white <- NULL


# Calculate number of repeats* per school per year
#   *Students with ODRs in a given year that have ODRs in the previous year
recid.odr.1 <- subset(recid.odr, school_schoolyear == 200405)
recid.odr.2 <- subset(recid.odr, school_schoolyear == 200506)
recid.odr.3 <- subset(recid.odr, school_schoolyear == 200607)
recid.odr.4 <- subset(recid.odr, school_schoolyear == 200708)
repeats.0506 <- recid.odr.2[recid.odr.2$student_studentid %in%
                            recid.odr.1$student_studentid, ]
repeats.0607 <- recid.odr.3[recid.odr.3$student_studentid %in%
                            recid.odr.2$student_studentid, ]
repeats.0708 <- recid.odr.4[recid.odr.4$student_studentid %in%
                            recid.odr.3$student_studentid, ]
rm(recid.odr.1, recid.odr.2, recid.odr.3, recid.odr.4)
num.repeats.0506 <- ddply(repeats.0506,
                          c("school_educationalorgid", "school_schoolyear"),
                          function(df) 
                            c(stud.repeats = length(df$student_studentid)))
num.repeats.0607 <- ddply(repeats.0607,
                          c("school_educationalorgid", "school_schoolyear"),
                          function(df) 
                            c(stud.repeats = length(df$student_studentid)))
num.repeats.0708 <- ddply(repeats.0708,
                          c("school_educationalorgid", "school_schoolyear"),
                          function(df) 
                            c(stud.repeats = length(df$student_studentid)))
num.repeats <- rbind(num.repeats.0506, num.repeats.0607, num.repeats.0708)
rm(num.repeats.0506, num.repeats.0607, num.repeats.0708)


# Days suspended and expelled
days_odr <- ddply(d, c("school_educationalorgid", "school_schoolyear"),
                  function(df)
                    c(days_susp  = mean(df$odr_dayssuspended, na.rm=TRUE),
                      days_expul = mean(df$odr_daysexpelled, na.rm=TRUE)))



# Calculate attendance through ODR skip class/truancy code
skips.odr <- subset(d, odr_problembehaviorid == 9)
skips <- ddply(skips.odr,
               c("school_educationalorgid", "school_schoolyear",
                 "student_studentid", "odr_date"),
               function(df)
                 c(skips  = length(df$odr_problembehaviorid)))
days.unexcused.per.student <- ddply(skips,
                      c("school_educationalorgid", "school_schoolyear",
                        "student_studentid"),
                      function(df)
                      	c(days.unexcused.per.student = length(df$student_studentid)))
days.unexcused <-  ddply(days.unexcused.per.student,
                      c("school_educationalorgid", "school_schoolyear"),
                      function(df)
                        c(days.unexcused = sum(df$days.unexcused.per.student)))

num.stud.unexcused <- ddply(days.unexcused.per.student,
                            c("school_educationalorgid", "school_schoolyear"),
                              function(df)
                                c(num.stud.unexcused = length(df$student_studentid)))

state.instruc <- unique(a[, c("school_educationalorgid",
                              "school_schoolyear", "school.statelabel",
                              "nces_member", "school.instruc.days")])
days.unexcused <- merge(days.unexcused, state.instruc,
                        by=c("school_educationalorgid", "school_schoolyear"))
num.stud.unexcused <- merge(num.stud.unexcused, state.instruc,
                        by=c("school_educationalorgid", "school_schoolyear"))

days.unexcused$school.instruc.days <- as.numeric(as.character(days.unexcused$school.instruc.days))
days.unexcused <- transform(days.unexcused,
                            mean.unexcused.days = days.unexcused / nces_member,
                            prop.unexcused.days = days.unexcused / school.instruc.days)
days.unexcused <- days.unexcused[, c(1:3, 7:8)]

num.stud.unexcused <- transform(num.stud.unexcused,
                       perc.unexcused = num.stud.unexcused / nces_member * 100)
num.stud.unexcused <- num.stud.unexcused[, c(1:3, 7)]


# Afram
# Calculate attendance through ODR skip class/truancy code
skips.odr.afram <- subset(d, odr_problembehaviorid == 9 & student_ethnicitylabel=="Black")
skips.afram <- ddply(skips.odr.afram,
               c("school_educationalorgid", "school_schoolyear",
                 "student_studentid", "odr_date"),
               function(df)
                 c(skips.afram = length(df$odr_problembehaviorid)))
days.unexcused.per.student.afram <- ddply(skips.afram,
                      c("school_educationalorgid", "school_schoolyear",
                        "student_studentid"),
                      function(df)
                      	c(days.unexcused.per.student.afram = length(df$student_studentid)))
days.unexcused.afram <-  ddply(days.unexcused.per.student.afram,
                      c("school_educationalorgid", "school_schoolyear"),
                      function(df)
                        c(days.unexcused.afram = sum(df$days.unexcused.per.student.afram)))

num.stud.unexcused.afram <- ddply(days.unexcused.per.student.afram,
                            c("school_educationalorgid", "school_schoolyear"),
                              function(df)
                                c(num.stud.unexcused.afram = length(df$student_studentid)))

state.instruc.afram <- unique(a[, c("school_educationalorgid",
                              "school_schoolyear", "school.statelabel",
                              "nces_black", "school.instruc.days")])
num.stud.unexcused.afram <- merge(num.stud.unexcused.afram, state.instruc.afram,
                        by=c("school_educationalorgid", "school_schoolyear"))

num.stud.unexcused.afram <- transform(num.stud.unexcused.afram,
                       perc.unexcused.afram = num.stud.unexcused.afram / nces_black * 100)
num.stud.unexcused.afram <- num.stud.unexcused.afram[, c(1:3, 7)]


# Asian
# Calculate attendance through ODR skip class/truancy code
skips.odr.asian <- subset(d, odr_problembehaviorid == 9 &
                             student_ethnicitylabel=="Asian")
skips.asian <- ddply(skips.odr.asian,
               c("school_educationalorgid", "school_schoolyear",
                 "student_studentid", "odr_date"),
               function(df)
                 c(skips.asian = length(df$odr_problembehaviorid)))
days.unexcused.per.student.asian <- ddply(skips.asian,
                      c("school_educationalorgid", "school_schoolyear",
                        "student_studentid"),
                      function(df)
                      	c(days.unexcused.per.student.asian = length(df$student_studentid)))
days.unexcused.asian <-  ddply(days.unexcused.per.student.asian,
                      c("school_educationalorgid", "school_schoolyear"),
                      function(df)
                        c(days.unexcused.asian = sum(df$days.unexcused.per.student.asian)))

num.stud.unexcused.asian <- ddply(days.unexcused.per.student.asian,
                            c("school_educationalorgid", "school_schoolyear"),
                              function(df)
                                c(num.stud.unexcused.asian = length(df$student_studentid)))

state.instruc.asian <- unique(a[, c("school_educationalorgid",
                              "school_schoolyear", "school.statelabel",
                              "nces_asian", "school.instruc.days")])
num.stud.unexcused.asian <- merge(num.stud.unexcused.asian, state.instruc.asian,
                        by=c("school_educationalorgid", "school_schoolyear"))

num.stud.unexcused.asian <- transform(num.stud.unexcused.asian,
                       perc.unexcused.asian = num.stud.unexcused.asian / nces_asian * 100)
num.stud.unexcused.asian <- num.stud.unexcused.asian[, c(1:3, 7)]


# Latino
# Calculate attendance through ODR skip class/truancy code
skips.odr.latino <- subset(d, odr_problembehaviorid == 9 &
                              student_ethnicitylabel=="Hispanic/Latino")
skips.latino <- ddply(skips.odr.latino,
               c("school_educationalorgid", "school_schoolyear",
                 "student_studentid", "odr_date"),
               function(df)
                 c(skips.latino = length(df$odr_problembehaviorid)))
days.unexcused.per.student.latino <- ddply(skips.latino,
                      c("school_educationalorgid", "school_schoolyear",
                        "student_studentid"),
                      function(df)
                      	c(days.unexcused.per.student.latino = length(df$student_studentid)))
days.unexcused.latino <-  ddply(days.unexcused.per.student.latino,
                      c("school_educationalorgid", "school_schoolyear"),
                      function(df)
                        c(days.unexcused.latino = sum(df$days.unexcused.per.student.latino)))

num.stud.unexcused.latino <- ddply(days.unexcused.per.student.latino,
                            c("school_educationalorgid", "school_schoolyear"),
                              function(df)
                                c(num.stud.unexcused.latino = length(df$student_studentid)))

state.instruc.latino <- unique(a[, c("school_educationalorgid",
                              "school_schoolyear", "school.statelabel",
                              "nces_hisp", "school.instruc.days")])
num.stud.unexcused.latino <- merge(num.stud.unexcused.latino, state.instruc.latino,
                        by=c("school_educationalorgid", "school_schoolyear"))

num.stud.unexcused.latino <- transform(num.stud.unexcused.latino,
                       perc.unexcused.latino = num.stud.unexcused.latino / nces_hisp * 100)
num.stud.unexcused.latino <- num.stud.unexcused.latino[, c(1:3, 7)]


# White
# Calculate attendance through ODR skip class/truancy code
skips.odr.white <- subset(d, odr_problembehaviorid == 9 &
                              student_ethnicitylabel=="White")
skips.white <- ddply(skips.odr.white,
               c("school_educationalorgid", "school_schoolyear",
                 "student_studentid", "odr_date"),
               function(df)
                 c(skips.white = length(df$odr_problembehaviorid)))
days.unexcused.per.student.white <- ddply(skips.white,
                      c("school_educationalorgid", "school_schoolyear",
                        "student_studentid"),
                      function(df)
                      	c(days.unexcused.per.student.white = length(df$student_studentid)))
days.unexcused.white <-  ddply(days.unexcused.per.student.white,
                      c("school_educationalorgid", "school_schoolyear"),
                      function(df)
                        c(days.unexcused.white = sum(df$days.unexcused.per.student.white)))

num.stud.unexcused.white <- ddply(days.unexcused.per.student.white,
                            c("school_educationalorgid", "school_schoolyear"),
                              function(df)
                                c(num.stud.unexcused.white = length(df$student_studentid)))

state.instruc.white <- unique(a[, c("school_educationalorgid",
                              "school_schoolyear", "school.statelabel",
                              "nces_white", "school.instruc.days")])
num.stud.unexcused.white <- merge(num.stud.unexcused.white, state.instruc.white,
                        by=c("school_educationalorgid", "school_schoolyear"))

num.stud.unexcused.white <- transform(num.stud.unexcused.white,
                       perc.unexcused.white = num.stud.unexcused.white / nces_white * 100)
num.stud.unexcused.white <- num.stud.unexcused.white[, c(1:3, 7)]


rm(list=ls()[which(substr(ls(), 1, 9) == "recid.odr")])
rm(list=ls()[which(substr(ls(), 1, 9) == "skips.odr")])
rm(list=ls()[which(substr(ls(), 1, 11) == "recid.count")])

# Merge ODR statistics
#odrs <- MergeBySchoolAndYear(iss, oss)
#odrs <- MergeBySchoolAndYear(odrs, expul)
#odrs <- MergeBySchoolAndYear(odrs, major)
#odrs <- MergeBySchoolAndYear(odrs, odr)
#odrs <- MergeBySchoolAndYear(odrs, recid)
#odrs <- MergeBySchoolAndYear(odrs, num.stud)
#odrs <- MergeBySchoolAndYear(odrs, num.repeats)
#odrs <- MergeBySchoolAndYear(odrs, days_odr)


odr.list <- list(iss, oss, oss.afram, oss.asian, oss.latino, oss.white, expul,
                 major, major.afram, major.asian, major.latino, major.white,
                 odr, recid, recid.afram, recid.asian, recid.latino, recid.white,
                 num.stud, num.stud.afram, num.stud.asian, num.stud.latino, num.stud.white,
                 num.repeats, days_odr, days.unexcused, num.stud.unexcused,
                 num.stud.unexcused.afram, num.stud.unexcused.asian,
                 num.stud.unexcused.latino, num.stud.unexcused.white)
odrs <- Reduce(MergeBySchoolAndYear, odr.list, accumulate=FALSE)


# STATE-SPECIFIC ASSESSMENT AND DEMOGRAPHIC DATA ###############################
# COLORADO
#assessments.co <- ddply(assessments.co,
#                        c("school_ncesschoolid", "school_schoolyear"),
#                        function(df)
#                          c(reading.score = mean(df$reading.score),
#                            math.score    = mean(df$math.score)))
#colorado <- merge(assessments.co, demographics.co,
#                  by=c("school_ncesschoolid", "school_schoolyear"),
#                  all=TRUE)
#
#
## ILLINOIS
#assessments.il <- ddply(assessments.il,
#                        c("school_ncesschoolid", "school_schoolyear"),
#                        function(df)
#                          c(reading.score = mean(df$reading.score),
#                            math.score    = mean(df$math.score)))
#illinois <- merge(assessments.il, demographics.il,
#                  by=c("school_ncesschoolid", "school_schoolyear"),
#                  all=TRUE)
#
#
#
## MARYLAND
#assessments.md <- ddply(assessments.md,
#                        c("school_ncesschoolid", "school_schoolyear"),
#                        function(df)
#                          c(reading.score = mean(df$reading.score),
#                            math.score    = mean(df$math.score)))
#maryland <- merge(assessments.md, demographics.md,
#                  by=c("school_ncesschoolid", "school_schoolyear"),
#                  all=TRUE)
#
## MONTANA
#assessments.mt <- ddply(assessments.mt,
#                        c("school_ncesschoolid", "school_schoolyear"),
#                        function(df)
#                          c(reading.score = mean(df$reading.score),
#                            math.score    = mean(df$math.score)))
#montana <- merge(assessments.mt, demographics.mt,
#                 by=c("school_ncesschoolid", "school_schoolyear"),
#                 all=TRUE)
#
#
## OREGON
#oregon <- merge(oregon, inst.crosswalk, by="InstID")
#
##clean oregon
#
#
#
# ADDITIONAL VARIABLE CREATION #################################################
# Create total instruction time
#diff in num of ODRs pre-post multiplied by time factor found in Muscott article

# Create leadership/admin time
#diff in num of ODRs pre-post multiplied by time factor found in Muscott article

# Create teacher time
#diff in num of ODRs pre-post multiplied by time factor found in Muscott article


# RACIAL DISPROPORTIONALITY ####################################################
races.odr <- CreateOdrPercsByRace(d)

races.odr.x <- CreateRacialOdrRatios(races.odr)

races.odr.y <- CreateRacialDispDataWide(races.odr.x)

races.odr.z <- ReshapeRacialDispDataToLong(races.odr.y)


# Look at schools by FRPM
#frpm.lower.third <- quantile(lm.data$frpm.prop, probs=(1/3), na.rm=TRUE)
#frpm.upper.third <- quantile(lm.data$frpm.prop, probs=(2/3), na.rm=TRUE)
#lm.data$frpm.cat <- ifelse(lm.data$frpm.prop < frpm.lower.third,
#                           1,
#                           ifelse(lm.data$frpm.prop >= frpm.upper.third,
#                                  3,
#                                  2))
#frpms <- subset(lm.data, select = c("school_educationalorgid",
#                                    "school_schoolyear", "frpm.cat"))
#races.odr.x.frpm <- merge(races.odr.x, frpms)
#
#races.odr.y.frpm <- ddply(races.odr.x.frpm,
#           c("school_schoolyear", "frpm.cat"), function(df)
#           c(afram.major.over.white = mean(df$afram.major.over.white, na.rm=TRUE),
#             afram.major.ratio = mean(df$afram.major.ratio, na.rm=TRUE),
#             afram.odr.ratio = mean(df$afram.odr.ratio, na.rm=TRUE),
#             afram.oss.ratio = mean(df$afram.oss.ratio, na.rm=TRUE),
#             asian.major.over.white = mean(df$asian.major.over.white, na.rm=TRUE),
#             asian.major.ratio = mean(df$asian.major.ratio, na.rm=TRUE),
#             asian.odr.ratio = mean(df$asian.odr.ratio, na.rm=TRUE),
#             asian.oss.ratio = mean(df$asian.oss.ratio, na.rm=TRUE),
#             latino.major.over.white = mean(df$latino.major.over.white, na.rm=TRUE),
#             latino.major.ratio = mean(df$latino.major.ratio, na.rm=TRUE),
#             latino.odr.ratio = mean(df$latino.odr.ratio, na.rm=TRUE),
#             latino.oss.ratio = mean(df$latino.oss.ratio, na.rm=TRUE),
#             white.major.over.white = mean(df$white.major.over.white, na.rm=TRUE),
#             white.major.ratio = mean(df$white.major.ratio, na.rm=TRUE),
#             white.odr.ratio = mean(df$white.odr.ratio, na.rm=TRUE),
#             white.oss.ratio = mean(df$white.oss.ratio, na.rm=TRUE)))
#
#races.odr.z.frpm <- reshape(races.odr.y,
#                       direction="long",
#                       idvar=c("school_schoolyear", "frpm.cat"),
#                       varying=c(2:length(races.odr.y)),
#                       v.names=c("major.over.white", "major.ratio", "odr.ratio", "oss.ratio"),
#                       times=c("African American", "Asian", "Latino", "White"),
#                       timevar="race")
#
#races.odr.z.frpm$frpm.cat <- factor(races.odr.z.frpm$frpm.cat,
#                                    labels=c("Lower FRPM", "Middle FRPM",
#                                             "High FRPM"))
#


# MERGE DATA FOR MULTIVARIABLE ANALYSIS ########################################
lm.data.list <- list(a, ebs.scores, set.scores, odrs, races.odr.x)
lm.data <- Reduce(MergeBySchoolAndYear, lm.data.list, accumulate=FALSE)
#lm.data <- lm.data[-which(is.na(lm.data$school_stateid)), ]

# states.list <- list(colorado, illinois, maryland, montana, oregon)
# lm.data <- Reduce(function(x, y)
#                     merge(x, y, all.x=TRUE,
#                           by=c("school_ncesschoolid", "school_schoolyear",
#                                "school.statelabel"),
#                   states.list, accumulate=FALSE)


# Merge PBIS data with states data
states$school_ncesschoolid <- as.numeric(format(states$school_ncesschoolid,
                                                sci=FALSE))

lm.data$school_ncesschoolid <- as.numeric(as.character(lm.data$school_ncesschoolid))
lm.data <- merge(lm.data, states,
           by=c("school_ncesschoolid", "school_schoolyear"),
           all.x=TRUE)


# Additional variable creation
impl.avg.names <- c("ebs.implementationaverage", "set.implementationaverage")
lm.data$implementationaverage <- rowMeans(lm.data[, impl.avg.names],
                                          na.rm=TRUE)


lm.data <- transform(lm.data,
                     medium.fidelity = ifelse(implementationaverage >= 50 &
                                              implementationaverage < 80,
                                              1,
                                              0),
                     high.fidelity = ifelse(implementationaverage >= 80,
                                            1,
                                            0))

lm.data$pbis.quintile <- with(lm.data, ifelse(implementationaverage < 20,
                        1,
                        ifelse(implementationaverage >= 20 & implementationaverage < 40,
                               2,
                               ifelse(implementationaverage >= 40 & implementationaverage < 60,
                                      3,
                                      ifelse(implementationaverage >= 60 & implementationaverage < 80,
                                             4,
                                             5)))))

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

rm(urban.names, lm.data.urban, lm.data.urban.a, lm.data.urban.b,
   lm.data.urban.x)


# Center continuous covariates
lm.data <- transform(lm.data,
                     male.perc.cntr = CenterVar(male.perc),
                     afram.perc.cntr = CenterVar(afram.perc),
                     aian.perc.cntr = CenterVar(aian.perc),
                     asian.perc.cntr = CenterVar(asian.perc),
                     latino.perc.cntr = CenterVar(latino.perc),
                     white.perc.cntr = CenterVar(white.perc),
                     frpm.perc.cntr  = CenterVar(frpm.perc),
                 #    special.ed.enroll.perc.cntr = CenterVar(special.ed.enroll.perc),
                 #    lep.perc.cntr = CenterVar(lep.perc),
                     nces_member.cntr = CenterVar(nces_member),
                     enrollment.sq.cntr = CenterVar(enrollment.sq),
                     stud.per.fte.cntr = CenterVar(stud.per.fte))


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
                     

lm.data$i.ebs <- ifelse(!is.na(lm.data$ebs.implementationaverage), 1, 0)
lm.data$i.set <- ifelse(!is.na(lm.data$set.implementationaverage), 1, 0)
lm.data$i.both <- with(lm.data, ifelse(i.ebs == 1 & i.set == 1, 1, 0)) 
lm.data$pbis <- with(lm.data,
                     ifelse(i.ebs == 1 & i.set == 0,
                            1,
                            ifelse(i.set == 1 & i.ebs == 0,
                                   2,
                                   3)))

lm.data <- transform(lm.data,
                     grad.rate = grad.rate / 100,
                     perc.unexcused = perc.unexcused / 100,
                     perc.unexcused.afram = perc.unexcused.afram / 100,
                     perc.unexcused.asian = perc.unexcused.asian / 100,
                     perc.unexcused.latino = perc.unexcused.latino / 100,
                     perc.unexcused.white = perc.unexcused.white / 100,
                     attend.rate = attend.rate / 100,
                     reading.score = reading.score / 100,
                     math.score = math.score / 100)



# PBIS Start Year
names(start.years)[4:11] <- paste("y", as.character(2000:2007), sep=".")
start.years <- upData(start.years, lowernames=TRUE)
start.years <- reshape(start.years,
                       direction="long",
                       varying=c(4:length(start.years)),
                       v.names="x",
                       idvar="school_educationalorgid",
                       drop=c("nces_level", "years"),
                       times=c(as.character(2000:2007)),
                       timevar="y",
                       sep=".",
                       new.row.names=1:512)

start.years <- start.years[which(!is.na(start.years$x)), ]
start.years <- start.years[order(start.years$school_educationalorgid), ]

start.years <- start.years[!duplicated(start.years$school_educationalorgid), ]
start.years$x <- NULL
names(start.years)[2] <- "pbis.start.year"
start.years$pbis.start.year <- as.numeric(start.years$pbis.start.year)
start.years$pbis.start.year[start.years$school_educationalorgid == 217] <- 2004
start.years$pbis.start.year[start.years$school_educationalorgid == 220] <- 2004

lm.data <- merge(lm.data, start.years, all.x=TRUE)

lm.data <- transform(lm.data,
                     current.year = as.numeric(substr(school_schoolyear, 1, 4)))

lm.data <- transform(lm.data,
                     year.of.pbis = current.year - pbis.start.year + 1)



# Make lm.data.original to store original file including high-odr outlier school
lm.data.original <- lm.data

# Drop outlier school that has > 8000 ODRs and save as lm.data
lm.data <- lm.data.original[-which(lm.data$school_educationalorgid == 3006), ]

# Drop outlier school and high schools
lm.data.hs.dropped <- lm.data[-which(lm.data$high.school == 1), ]










save.image(file="/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/data/workspaces/pbis_cleaned.RData")

# Save csv files for Stata
setwd("/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/data/pbis_data/final_csv")
write.csv(lm.data.original, "lm_data_original.csv")
write.csv(lm.data, "lm_data.csv")
write.csv(lm.data.hs.dropped, "lm_data_hs_dropped.csv")


