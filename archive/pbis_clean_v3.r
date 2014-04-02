#sink(file="/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/results/pbis_cleaning_output.txt")  # send output to file

# PBIS Cleaning File
# By Casey Tsui
# Format: R
# Last updated: 10/28/10


# Author Comment: This file cleans the variables of interest.
#   This is where all the ugly stuff lives.
#   Takes care of missing values, merging data frames, handling outliers. 


setwd("/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/coding")
source("pbis_load.r")
library(Hmisc) # Hmisc needed to change variable names to lowercase


# FUNCTION DEFINITIONS #########################################################
MakeA <- function(x) {
  # Creates object "a" that contains all school-level variables
  #
  # Args:
  #   x: Data frame
  x1 <- SubsetA(x)
  x2 <- DropFactors(x1)
  x3 <- ConvertA(x2)
  return(x3)
}
SubsetA <- function(x) {
  # Subsets, factors and converts all school-level variables.
  #
  # Args:
  #   x: Data frame
  x1 <- unique(x[, 1:48])
  x1[x1 == "NULL"] <- NA
  if (levels(x1$nces_gshi)[1] == "05") {
    x1 <- x1[-which(x1$nces_gshi == "05"), ]
  }
  return(x1)
}
DropFactors <- function(x) {
  # Drops unused factor levels
  #
  # Args:
  #   x: Data frame
  #
  # Returns:
  #   Data frame with all unused factor levels removed
  x1 <- x
  for (i in 1:ncol(x1)) {
    if (is.factor(x1[, i]) == TRUE)
      x1[, i] <- factor(x1[, i])
  }
  return(x1)
}
ConvertA <- function(x) {
  # Convert variables to numeric from factor
  #
  # Args:
  #   x: Data frame
  x1 <- x
  for (i in c(10:12, 14:17, 20:ncol(x1))) {
    x1[, i] <- as.numeric(as.character(x1[, i]))
  }
  return(x1)
}


MakeB <- function(x) {
  # Subsets, factors and converts all EBS and SET variables.
  #
  # Args:
  #   x: Data frame
  x1 <- SubsetB(x)
  x2 <- DropFactors(x1)
  x3 <- ConvertB(x2)
  return(x3)
}
SubsetB <- function(x) {
  # Args:
  #   x: Data frame
  x1 <- x
  x1 <- x1[, c(1:3, 45:ncol(x1))]
  x1[x1 == "NULL"] <- NA  # Change NULLs to NAs
  return(x1)
}
ConvertB <- function(x) {
  # Convert variables to numeric from factor
  #
  # Args:
  #   x: Data frame
  x1 <- x
  for (i in c(14:105)) {  # Convert only EBS variables, as SET is fine
    x1[, i] <- as.numeric(as.character(x1[, i]))
  }
  return(x1)
}


MakeC <- function(x) {
  # Subsets, formats and recodes all ODR variables.
  #
  # Args:
  #   x: Data frame
  x1 <- SubsetC(x)
  x2 <- DropFactors(x1)
  x3 <- RecodeC(x2)
  return(x3)
}
SubsetC <- function(x) {
  # Args:
  #   x: Data frame
  x1 <- x
  x1 <- x1[, c(1:3, 7:24)]
  x1[x1 == "NULL"] <- NA  # Change NULLs to NAs
  return(x1)
}
RecodeC <- function(x) {
  # Recode days suspended and expelled as numeric.
  #
  # Args:
  #   x: Data frame
  x1 <- x
  for (i in 20:21) {
    x1[, i] <- as.numeric(as.character(x1[, i]))
  }
  return(x1)
}


KeepConsistentGrades <- function(x, y) {
  # Keeps only the observations that maintain consistent grade type ID through
  # all 3 or 4 years
  #
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
  # Divides the given data frame variable by the total enrollment at each school
  #   and returns teh proportion.
  #
  # Args:
  #   x: Data frame variable
  x1 <- x / a$nces_member
  return(x1)
}

MakeStaffResponse <- function(x) {
  # Creates a variable for number of EBS surveys per FTE
  # Args:
  #   x: Data frame
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


MakeSum <- function(x) {
  # Summarizes the variable and returns the results as a data frame
  #
  # Args:
  #   x: Data frame
  #
  # Returns:
  #   The results as a data frame.
  x1 <- x
  x2 <- summarize(x1$odr_admindecisionid,
                  llist(x1$school_educationalorgid, x1$school_schoolyear),
                  sum, na.rm=TRUE)
  return(x2)
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
master  <- upData(master,  lowernames=TRUE)
odr     <- upData(odr,     lowernames=TRUE)
s.ids   <- upData(s.ids,   lowernames=TRUE)

schools$school_schoolyear <- factor(master$school_schoolyear,
                                    levels=c("200405", "200506", "200607",
                                             "200708"))
master$school_schoolyear <- factor(master$school_schoolyear,
                                   levels=c("200405", "200506", "200607",
                                            "200708"))
odr$school_schoolyear <- factor(master$school_schoolyear,
                                levels=c("200405", "200506", "200607",
                                         "200708"))
s.ids$school_schoolyear <- factor(master$school_schoolyear,
                                  levels=c("200405", "200506", "200607",
                                           "200708"))



# SAVE ALL SCHOOL-LEVEL VARIABLES AS OBJECT "A"
a <- MakeA(master)

# SAVE ALL FIDELITY MEASURE VARIABLES AS OBJECT "B"
b <- MakeB(master)

# SAVE ALL ODR VARIABLES AS OBJECT "C"
c <- MakeC(odr)

# Merge student IDs to ODRs
s.ids <- s.ids[, c("odr_referralid", "student_studentid")]
c <- unique(merge(c, s.ids))

# Remove initial datasets to free up memory and increase computational speed
rm(master, odr)




# SUBSETTING ###################################################################
# Create 3-category grade type ID
a$school.gradetypeid <- ifelse(a$nces_gslo == "09" & a$nces_gshi == "12",
                               "HS",          # 2=all high schools 9-12
                               ifelse((a$nces_gslo == "06" |
                                       a$nces_gslo == "07") &
                                       a$nces_gshi == "08",
                                       "MS",   # 1=all middle schools 6-8 or 7-8
                                       "Mix"))  # 3=mixed schools

a$school.gradetypeid <- factor(a$school.gradetypeid,
                               levels=c("MS", "HS", "Mix"))

# Keep only those schools that maintained grade type through all years
g <- table(a$school_educationalorgid, a$school.gradetypeid)
g <- g[(g[, 1] >= 3 | g[, 2] >= 3 | g[, 3] >= 3), ]
a <- KeepConsistentGrades(a, g)
b <- KeepConsistentGrades(b, g)
c <- KeepConsistentGrades(c, g)



# Look at the schools without ODR data and see if you want to exclude them
schools.c <- data.frame(cbind(unique(c$school_educationalorgid), 1))
a <- merge(a, schools.c, by.x="school_educationalorgid", by.y="X1")
a <- a[, -length(a)]  # Remove X2
b <- merge(b, schools.c, by.x="school_educationalorgid", by.y="X1")
b <- b[, -length(b)]  # Remove X2
g <- table(a$school_educationalorgid, a$school.gradetypeid)

# Add a variable to "a" that specifies how many years of data are available for
#   each school
years.of.data <- ifelse((g[, 1] == 3 | g[, 2] == 3 | g[, 3] == 3), 3, 4)
rm(g)
a <- cbind(a, years.of.data)

# Add State labels to dataset "a"
school_stateid <- c("8", "17", "24", "26", "30", "33", "38", "41")
school.statelabel <- c("CO", "IL", "MD", "MI", "MT", "NH", "ND", "OR")
state.labels <- data.frame(cbind(school_stateid, school.statelabel))
a <- merge(a, state.labels, by="school_stateid")

# Merge state labels to "a" to tabulate NCES IDs by state to give to Laura
test.merge.data <- data.frame(unique(a.new[, c("school_educationalorgid",
                                               "school_ncesschoolid",
                                               "school_statelabel")]))
schools1 <- schools
schools1 <- merge(schools1, test.merge.data,
                  by="school_educationalorgid",
                  all.y=TRUE)
write.csv(schools1, "nces_id_by_state.csv")


# DESCRIPTIVE VARIABLE CREATION ################################################
# Create an index variable for each school to use in regression
school_educationalorgid <- sort(unique(a$school_educationalorgid))
school.id <- seq(1:length(school_educationalorgid))
d <- cbind(school_educationalorgid, school.id)
a <- merge(a, d, by="school_educationalorgid", all=TRUE)
rm(d)

# Create an index variable for each district to use in regression
school_districtid <- sort(unique(a$school_districtid))
district.id <- seq(1:length(school_districtid))
d <- cbind(school_districtid, district.id)
a <- merge(a, d, by="school_districtid", all=TRUE)
rm(d)


# Create an index variable for each year to use in regression
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
                           "S",          # 1=small schools
                           ifelse(a$nces_member >= 200 & a$nces_member < 1000,
                                  "M",   # 2=medium schools
                                  "L"))  # 3=large schools
a$enrollment.cat <- factor(a$enrollment.cat, levels=c("S", "M", "L"))
######################DOESN'T WORK YET#
a.unique <- within(a.unique,
                   enrollment.cat <- ifelse(nces_member < 200,
                                            "S",          # 1=small schools
                                            ifelse(nces_member >= 200 & nces_member < 1000,
                                            "M",   # 2=medium schools
                                            "L"))  # 3=large schools
a.unique$enrollment.cat <- factor(a.unique$enrollment.cat,
                                  levels=c("S", "M", "L"))




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


### Calculate # of FTE per 100 students
a$fte.per.student <- a$nces_fte / a$nces_member * 100


### Calculate % Free and Reduced Price Meal students
a$percent.frpm <- a$nces_totfrl / a$nces_member * 100


############################################################################
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
b.ebs <- b[(b$i.ebs.3yr == 1 | b$i.ebs.4yr == 1), c(1:105, 152:153)]
b.set <- b[(b$i.set.4yr == 1 | b$i.set.4yr == 1), c(1:7,   106:153)]


# Create sum of EBS scores
b.ebs$sw.disc.sys  <- rowSums(b.ebs[, seq(14,  48, 2)], na.rm=TRUE)
b.ebs$ncr.set      <- rowSums(b.ebs[, seq(50,  66, 2)], na.rm=TRUE)
b.ebs$cr.set       <- rowSums(b.ebs[, seq(68,  88, 2)], na.rm=TRUE)
b.ebs$ind.stud.sys <- rowSums(b.ebs[, seq(90, 104, 2)], na.rm=TRUE)


# Validate additions by calculating the ranges of the aggregate variables
range(b.ebs$sw.disc.sys,  na.rm=TRUE)  # Should be 0-36
range(b.ebs$ncr.set,      na.rm=TRUE)  # Should be 0-18
range(b.ebs$cr.set,       na.rm=TRUE)  # Should be 0-22
range(b.ebs$ind.stud.sys, na.rm=TRUE)  # Should be 0-16


#b.ebs <- b.ebs[, c(1:5, 106:length(b.ebs))]
b.ebs <- unique(b.ebs[, -c(4:7)])


################################### THROW AWAY? ################################
# Merge aggregated EBS scores into one object
ebs.1 <- summarize(b$sw.disc.sys, llist(b$school_schoolyear,
                   b$school_educationalorgid), mean, na.rm=TRUE)
colnames(ebs.1) <- c("school_schoolyear", "school_educationalorgid",
                     "sw.disc.sys")
ebs.2 <- summarize(b$ncr.set, llist(b$school_schoolyear,
                   b$school_educationalorgid), mean, na.rm=TRUE)
colnames(ebs.2) <- c("school_schoolyear", "school_educationalorgid",
                     "ncr.set")
ebs.3 <- summarize(b$cr.set, llist(b$school_schoolyear,
                   b$school_educationalorgid), mean, na.rm=TRUE)
colnames(ebs.3) <- c("school_schoolyear", "school_educationalorgid",
                     "cr.set")
ebs.4 <- summarize(b$ind.stud.sys, llist(b$school_schoolyear,
                   b$school_educationalorgid), mean, na.rm=TRUE)
colnames(ebs.4) <- c("school_schoolyear", "school_educationalorgid",
                     "ind.stud.sys")
ebs.merge <- merge(ebs.1, ebs.2,
                   by=c("school_schoolyear", "school_educationalorgid"),
                   all=TRUE)
ebs.merge <- merge(ebs.merge, ebs.3,
                   by=c("school_schoolyear", "school_educationalorgid"),
                   all=TRUE)
ebs.merge <- merge(ebs.merge, ebs.4,
                   by=c("school_schoolyear", "school_educationalorgid"),
                   all=TRUE)
################################################################################


# Calculate percentage of EBS scores that are marked "In Place"
# Schoolwide discipline system - percent "in place"
b1 <- b.ebs[, c(1:3, seq(10, 44, 2))]

# New, more efficient method to calculate percent in place

b1$perc.in.place.sw.disc.sys <- rowSums(b1[, 4:length(b1)] == 2, na.rm=TRUE) / 18 * 100 

x1 <- with(b1,
           summarize(perc.in.place.sw.disc.sys,
                     llist(school_educationalorgid, school_schoolyear),
                     mean, na.rm=TRUE))
#names(x1) <- c("school_educationalorgid", "school_schoolyear", "perc.in.place.sw.disc.sys")


# Noncr.setting - percent "in place"
b2 <- b.ebs[, c(1:3, seq(46, 62, 2))]

b2$perc.in.place.ncr.set <- rowSums(b2[, 4:length(b2)] == 2, na.rm=TRUE) / 9 * 100

x2 <- with(b2, 
           summarize(perc.in.place.ncr.set,
                     llist(school_educationalorgid, school_schoolyear),
                     mean, na.rm=TRUE))
#names(x2) <- c("school_educationalorgid", "school_schoolyear", "perc.in.place.ncr.set")


# Classroom setting - percent "in place"
b3 <- b.ebs[, c(1:3, seq(64, 84, 2))]

b3$perc.in.place.cr.set <- rowSums(b3[, 4:length(b3)] == 2, na.rm=TRUE) / 11 * 100 

x3 <- with(b3,
           summarize(perc.in.place.cr.set,
                     llist(school_educationalorgid, school_schoolyear),
                     mean, na.rm=TRUE))
#names(x3) <- c("school_educationalorgid", "school_schoolyear", "perc.in.place.cr.set")


# Individual student system - percent "in place"
b4 <- b.ebs[, c(1:3, seq(86, 100, 2))]

b4$perc.in.place.ind.stud.sys <- rowSums(b4[, 4:length(b4)] == 2, na.rm=TRUE) / 8 * 100 

x4 <- with(b4,
           summarize(perc.in.place.ind.stud.sys,
                     llist(school_educationalorgid, school_schoolyear),
                     mean, na.rm=TRUE))
#names(x4) <- c("school_educationalorgid", "school_schoolyear", "perc.in.place.indiv.stud.sys")



b.total <- cbind(x1, x2, x3, x4)
b.total <- b.total[-nrow(b.total), -c(4:5, 7:8, 10:11)]


# SET ##########################################################################
b.set <- b.set[, c(1:3, 6:7, 11:18, 53:length(b.set))]


# DEPENDENT VARIABLE CREATION ##################################################
# Suspension rates
#schoolyears <- list(200405, 200506, 200607, 200708)
#for (i in c(1:4)) {
#  schoolyears[i]
#  table(c$school_educationalorgid[c$school_schoolyear == schoolyears[i]],
#        c$odr_admindecisionid[c$school_schoolyear == schoolyears[i]])
#}


# Create ODR outcome variables
odr.x1 <- as.matrix(table(c$school_educationalorgid[c$school_schoolyear == 200405],
                          c$odr_admindecisionid[c$school_schoolyear == 200405]))
odr.x1 <- cbind(rep(200405, nrow(odr.x1)), odr.x1)

odr.x2 <- as.matrix(table(c$school_educationalorgid[c$school_schoolyear == 200506],
                          c$odr_admindecisionid[c$school_schoolyear == 200506]))
odr.x2 <- cbind(rep(200506, nrow(odr.x2)), odr.x2)
odr.x3 <- as.matrix(table(c$school_educationalorgid[c$school_schoolyear == 200607],
                          c$odr_admindecisionid[c$school_schoolyear == 200607]))
odr.x3 <- cbind(rep(200607, nrow(odr.x3)), odr.x3)
odr.x4 <- as.matrix(table(c$school_educationalorgid[c$school_schoolyear == 200708],
                          c$odr_admindecisionid[c$school_schoolyear == 200708]))
odr.x4 <- cbind(rep(200708, nrow(odr.x4)), odr.x4)
#odr.x <- rbind(odr.x1, odr.x2, odr.x3, odr.x4)
#rm(odr.y1, odr.y2, odr.y3, odr.y4, odr.x1, odr.x2, odr.x3, odr.x4)
odr.x <- rbind(odr.x1, odr.x2, odr.x3)
rm(odr.y1, odr.y2, odr.y3, odr.x1, odr.x2, odr.x3)
#susp.x <- odr.x[order(c(as.numeric(rownames(odr.x)), odr.y1), ]
colnames(odr.x) <- c("school_schoolyear", "time_in_office", "loss_of_priv",
                      "student_conf", "parent_contact", "detention",
                      "indiv_instruc", "iss", "oss", "sat_school", "expulsion",
                      "other", "unknown", "bus_susp", "restitution")
odr.by.school <- merge(a, odr.x,
                       by.x=c("school_educationalorgid", "school_schoolyear"),
                       by.y=c("row.names", "school_schoolyear"), all=TRUE) 
# odr.by.school has separate odr count variables per school per year
rm(odr.x)


c$odr_problembehaviorismajor <- ifelse(c$odr_problembehaviorisminor == 0, 1, 0)

c1 <- c[c$odr_admindecisionid == 7, ]
iss.sums <- MakeSum(c1)
names(iss.sums) <- c("school_educationalorgid", "school_schoolyear", "iss")
a <- merge(a, iss.sums, by="school_educationalorgid", all=TRUE)

c2 <- c[c$odr_admindecisionid == 8, ]
oss.sums <- MakeSum(c2)
names(oss.sums) <- c("school_educationalorgid", "school_schoolyear", "oss")
a <- merge(a, oss.sums, by="school_educationalorgid", all=TRUE)

c3 <- c[c$odr_admindecisionid == 10, ]
expulsion.sums <- MakeSum(c3)
names(expulsion.sums) <- c("school_educationalorgid", "school_schoolyear",
                           "expulsions")
a <- merge(a, expulsion.sums, by="school_educationalorgid", all=TRUE)

c4 <- c[c$odr_problembehaviorismajor == 1, ]
major.odr.sums <- MakeSum(c4)
names(major.odr.sums) <- c("school_educationalorgid", "school_schoolyear",
                           "major.odrs")
a <- merge(a, major.odr.sums,
           by=c("school_educationalorgid", "school_schoolyear"),
           all=TRUE)


### Number of major ODRs per 100 students per day
a$major.odr.100 <- ifelse(a$school_stateid == "IL",
                          (a$odr_problembehaviorismajor / a$nces_member) * 176,
                          ifelse(a$school_statelabel == "MI",
                                 (a$odr_problembehaviorismajor / a$nces_member) * 1098 hours,
                                 ifelse(a$school_statelabel == "ND",
                                        (a$odr_problembehaviorismajor / a$nces_member) * 173,
                                        ifelse(a$school_statelabel == "OR",
                                               (a$odr_problembehaviorismajor / a$nces_member) * 900 for (6-8) and 990 hours for (7-12),
                                               (a$odr_problembehaviorismajor / a$nces_member) * 180))))

# Add in-school and out-of-school suspensions to create new dependent variable
a$suspension <- sum(a$iss + a$oss, na.rm=TRUE)



# ADDITIONAL VARIABLE CREATION #################################################
# Create total instruction time
#diff in num of ODRs pre-post multiplied by time factor found in Muscott article

# Create leadership/admin time
#diff in num of ODRs pre-post multiplied by time factor found in Muscott article

# Create teacher time
#diff in num of ODRs pre-post multiplied by time factor found in Muscott article


# MERGE DATA FOR MULTIVARIABLE ANALYSIS 
lm.data <- merge(a, b.total,
                 by=c("school_educationalorgid", "school_schoolyear"),
                 all=TRUE)
#lm.data <- merge(lm.data, odr.merge,
#                 by=llist("b$school_educationalorgid", "b$school_schoolyear"),
#                 all=TRUE)



save.image(file="/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/data/workspaces/pbis_cleaned.RData")

#sink()
