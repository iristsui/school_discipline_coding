sink(file="/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/results/pbis_analysis_output.txt") #send output to file

# PBIS Do File
# By Casey Tsui
# Format: R
# Last updated: 11/08/10


# Author Comment: This file contains the coding for analysis and graphing for 
# the PBIS data set.
#   Calls the functions defined in pbis_clean.r to clean the data.
#   Calls the functions defined in pbis_func.r to perform the analysis and
#   produces charts and tables.


setwd("/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/coding")
source("pbis_clean.r")
setwd("/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/coding")
source("pbis_func.r")
#library(ggplot2)
library(gee)
library(geepack)
library(lme4)


years <- c("2004", "2005", "2006", "2007")


# UNIVARIATE ANALYSIS ##########################################################
# Table 1
for (i in 1:length(years)) {
  # Loops through 2004 to the last year and makes table.1."year" object
  #   for each year
  new.var <- paste("table.1", as.character(years[i]), sep='.')
  table.1 <- list(length(unique(a$school_educationalorgid[a$year == i])),
                  length(unique(a$school_educationalorgid[a$year == i & a$grade.type == 1])),
                  length(unique(a$school_educationalorgid[a$year == i & a$grade.type == 2])),
                  length(unique(a$school_educationalorgid[a$year == i & a$grade.type == 3])),
                  mean(a$nces_member[a$year == i], na.rm=TRUE),
                  mean(a$male.prop[a$year == i], na.rm=TRUE) * 100,
                  mean(a$afram.prop[a$year == i], na.rm=TRUE) * 100,
                  mean(a$aian.prop[a$year == i], na.rm=TRUE) * 100,
                  mean(a$asian.prop[a$year == i], na.rm=TRUE) * 100,
                  mean(a$latino.prop[a$year == i], na.rm=TRUE) * 100,
                  mean(a$white.prop[a$year == i], na.rm=TRUE) * 100,
                  mean(a$grade.06.prop[a$year == i], na.rm=TRUE) * 100,
                  mean(a$grade.07.prop[a$year == i], na.rm=TRUE) * 100,
                  mean(a$grade.08.prop[a$year == i], na.rm=TRUE) * 100,
                  mean(a$grade.09.prop[a$year == i], na.rm=TRUE) * 100,
                  mean(a$grade.10.prop[a$year == i], na.rm=TRUE) * 100,
                  mean(a$grade.11.prop[a$year == i], na.rm=TRUE) * 100,
                  mean(a$grade.12.prop[a$year == i], na.rm=TRUE) * 100)
  assign(new.var, table.1, envir=.GlobalEnv)  # Makes new table.1."year" object 
}

names(table.1.2004) <- c("Number of Schools",  # Names table rows
                         "Number of Middle Schools",
                         "Number of High Schools",
                         "Number of Mixed Grade Schools",
                         "Average Enrollment",
                         "Percent Male Enrollment",
                         "Percent African American Enrollment",
                         "Percent American Indian/Alaska Native Enrollment",
                         "Percent Asian American Enrollment",
                         "Percent Latino Enrollment",
                         "Percent Caucasian Enrollment",
                         "Percent 6th Grade Enrollment",
                         "Percent 7th Grade Enrollment",
                         "Percent 8th Grade Enrollment",
                         "Percent 9th Grade Enrollment",
                         "Percent 10th Grade Enrollment",
                         "Percent 11th Grade Enrollment",
                         "Percent 12th Grade Enrollment")

cbind(table.1.2004, table.1.2005, table.1.2006, table.1.2007)  # Output Table 1


# Table 2
for (i in 1:length(years)) {
  # Loops through 2004 to the last year and makes table.2."year" object
  #   for each year
  new.var <- paste("table.2", as.character(years[i]), sep='.')
  table.2 <- list(nrow(c[c$year == i, ]),
                  nrow(c[c$year == i & c$odr_admindecisionid == 8, ]),
                  nrow(c[c$year == i & c$odr_admindecisionid == 10, ]),
                  mean(c$major.odr.100[c$year == i], na.rm=TRUE),
                  mean(c$oss.100[c$year == i], na.rm=TRUE),
                  mean(c$expulsions.100[c$year == i], na.rm=TRUE),
                  mean(c$oss.100.day[c$year == i], na.rm=TRUE),
                  mean(c$expulsions.100.day[c$year == i], na.rm=TRUE),
                  mean(c$odr.per.student[c$year == i], na.rm=TRUE),
                  mean(c$perc.students.with.odr[c$year == i], na.rm=TRUE))
  assign(new.var, table.2, envir=.GlobalEnv) 
}

names(table.2.2004) <- c("Number of ODRs",  # Names table rows
                         "Number of Suspensions",
                         "Number of Expulsions",
                         "Major ODRs per 100 Students",
                         "Suspensions per 100 Students",
                         "Expulsions per 100 Students",
                         "Suspensions per 100 Students per Day",
                         "Expulsions per 100 Students per Day",
                         "Average Number of ODR per Student",
                         "Average Percent of Students with ODR")

cbind(table.2.2004, table.2.2005, table.2.2006, table.2.2007)  # Output Table 2


# Table 3
for (i in 1:length(years)) {
  # Loops through 2004 to the last year and makes table.3."year" object
  #   for each year
  new.var <- paste("table.3", as.character(years[i]), sep='.')
  table.3 <- list(mean(b$schoolwide.disc.sys[b$year == i], na.rm=TRUE),
                  mean(b$nonclassroom.set[b$year == i], na.rm=TRUE),
                  mean(b$classroom.set[b$year == i], na.rm=TRUE),
                  mean(b$indiv.student.sys[b$year == i], na.rm=TRUE))
  assign(new.var, table.3, envir=.GlobalEnv) 
}

names(table.3.2004) <- c("Schoolwide Discipline System",  # Names table rows
                         "Nonclassroom Setting",
                         "Classroom Setting",
                         "Individual Student System")

cbind(table.3.2004, table.3.2005, table.3.2006, table.3.2007)  # Output Table 3



# CORRELATION ANALYSIS
cor(x, y, use="complete.obs")


# CROSS-TABULATIONS ############################################################
### Demographic means over 4 years - Table 1

var1 <- SchoolMeans(a$nces_member)  # summarize(a$nces_member, a$school_educationalorgid, mean, na.rm=TRUE)
var2 <- SchoolMeans(a$nces_totfrl)
var3 <- SchoolMeans(a$nces_fte)

school.means.table <- merge(var1, var2, by="a$school_educationalorgid",
                            all=TRUE)
school.means.table <- merge(school.means.table, var3,
                            by="a$school_educationalorgid", all=TRUE)
colnames(school.means.table) <- c("School ID", "Total Enrollment",
                                  "Total Students Eligible for FRPM",
                                  "Number of FTE")
school.means.table

ethnicities <- list(a$nces_am, a$nces_asian, a$nces_black, a$nces_hisp,
                    a$nces_white)
eths.means.table <- cbind(sapply(ethnicities, SchoolMeans))
colnames(eth.means.table) <- c("American Indian", "Asian", "Black", "Latino",
                               "White")
rownames(eths.means.table) <- c(unique(a$school_educationalorgid))

### Gender by grade
PropTable(c$student_gradeid, c$student_genderid)


### Mean number of students by ethnicity in each school per year
by(odr.by.school[, 14:18], list(schoolyear = odr.by.school$school_schoolyear,
                    orgid = odr.by.school$school_educationalorgid), mean)

### Mean number of odr outcomes per year
mean.odr <- by(odr.by.school[, 19:32], odr.by.school$school_schoolyear, mean)




### Mean free and reduced price meals, by school from 04-05 to 07-08
mean.frp <- cbind(tapply(a$nces_totfrl, a$school_educationalorgid, mean))


#test- list of matrices
mymats <- list()
for (i in 1:5) {
   myname <- paste('mymatrix', i, sep='')
   mymats[[myname]] <- matrix
}


####Adding a colsum to table
d1 <- table(b$school_educationalorgid, b$school_schoolyear)
total.col <- colSums(d1)
d2 <- rbind(d1, total.col)
total.row <- rowSums(d2)
cbind(d2, total.row)


### EBS surveys per school per year
#table(b$sasurvey_id, b$school_educationalorgid)




### ODR by school year
num.odr <- table(c$school_schoolyear, c$odr_admindecisionid)

# days suspended and expelled
#cbind(tapply(c$odr_dayssuspended, c$school_schoolyear, mean, na.rm=TRUE))
YearMeans(c$odr_dayssuspended)

#cbind(tapply(c$odr_daysexpelled, c$school_schoolyear, mean, na.rm=TRUE))
YearMeans(c$odr_daysexpelled)

PropTable(c$odr_dayssuspended, c$school_schoolyear)

PropTable(c$odr_daysexpelled, c$school_schoolyear)


### odr by grade
odr.by.grade <- table(c$student_gradeid, c$odr_admindecisionid)
##
GradeMeans(c$odr_dayssuspended)

GradeMeans(c$odr_daysexpelled)



###
cbind(tapply(c$odr_dayssuspended[c$school_schoolyear == 200405],
             c$student_gradeid[c$school_schoolyear == 200405],
             mean, na.rm=TRUE))
cbind(tapply(c$odr_dayssuspended[c$school_schoolyear == 200506],
             c$student_gradeid[c$school_schoolyear == 200506],
             mean, na.rm=TRUE))
cbind(tapply(c$odr_dayssuspended[c$school_schoolyear == 200607],
             c$student_gradeid[c$school_schoolyear == 200607],
             mean, na.rm=TRUE))
cbind(tapply(c$odr_dayssuspended[c$school_schoolyear == 200708],
             c$student_gradeid[c$school_schoolyear == 200708],
             mean, na.rm=TRUE))

cbind(tapply(c$odr_daysexpelled[c$school_schoolyear == 200405],
             c$student_gradeid[c$school_schoolyear == 200405],
             mean, na.rm=TRUE))
cbind(tapply(c$odr_daysexpelled[c$school_schoolyear == 200506],
             c$student_gradeid[c$school_schoolyear == 200506],
             mean, na.rm=TRUE))
cbind(tapply(c$odr_daysexpelled[c$school_schoolyear == 200607],
             c$student_gradeid[c$school_schoolyear == 200607],
             mean, na.rm=TRUE))
cbind(tapply(c$odr_daysexpelled[c$school_schoolyear == 200708],
             c$student_gradeid[c$school_schoolyear == 200708],
             mean, na.rm=TRUE))

###
PropTable(c$odr_dayssuspended, c$student_gradeid)

PropTable(c$odr_daysexpelled, c$student_gradeid)
##


### odr by gender
table(c$student_genderid, c$odr_admindecisionid)

GenderMeans(c$odr_dayssuspended)

GenderMeans(c$odr_daysexpelled)

PropTable(c$odr_dayssuspended, c$student_genderid)

PropTable(c$odr_daysexpelled, c$student_genderid)


### odr by ethnicity
table(c$student_ethnicityid, c$odr_admindecisionid)

EthnicityMeans(c$odr_dayssuspended)

EthnicityMeans(c$odr_daysexpelled)

PropTable(c$odr_dayssuspended, c$student_ethnicityid)

PropTable(c$odr_daysexpelled, c$student_ethnicityid)


# Combine ebs data and calculate mean scores over each school year, and t-tests
ebs.group1.by.year <- summarize(b$schoolwide.disc.sys, b$school_schoolyear,
                                mean, na.rm=TRUE)
#chisq.test(ebs.group1.by.year)
#t.test(b$schoolwide.disc.sys, b$school_schoolyear, na.rm=TRUE)

ebs.group2.by.year <- summarize(b$nonclassroom.set, b$school_schoolyear,
                                mean, na.rm=TRUE)
#chisq.test(ebs.group2.by.year)
#t.test(b$nonclassroom.set, b$school_schoolyear, na.rm=TRUE)

ebs.group3.by.year <- summarize(b$classroom.set, b$school_schoolyear, mean,
                                na.rm=TRUE)
#chisq.test(ebs.group3.by.year)
#t.test(b$classroom.set, b$school_schoolyear, na.rm=TRUE)

ebs.group4.by.year <- summarize(b$indiv.student.sys, b$school_schoolyear,
                                mean, na.rm=TRUE)
#chisq.test(ebs.group4.by.year)
#t.test(b$indiv.student.sys, b$school_schoolyear, na.rm=TRUE)


# GRAPHS #######################################################################
# Average number of suspensions by school year
plot(summarize(odr.by.school$oss, by=odr.by.school$school_schoolyear, mean,
               na.rm=TRUE),
     main="Number of Suspensions Per School Year",
     ylab="Average Number of Suspensions",
     xlab="School Year")
#myline.fit <- lm(summarize(odr.by.school$oss, by=odr.by.school$school_schoolyear, mean,
#               na.rm=TRUE))
#abline(myline.fit)
#
#
# Boxplot of OSS by school year
boxplot(oss ~ school_schoolyear, data=odr.by.school, main="Suspensions",
        ylab="Number of OSS",
        xlab="School Year")


# Average number of suspensions by locale
plot(summarize(odr.by.school$oss, by=odr.by.school$nces_locale, mean,
               na.rm=TRUE),
     main="Number of Suspensions Per Locale ID",
     ylab="Average Number of Suspensions",
     xlab="Locale ID")
# Boxplot of OSS by locale
boxplot(oss ~ nces_locale, data=odr.by.school, 
        main="Out of School Suspensions",
        ylab="Number of OSS",
        xlab="Locale ID")

# Average number of suspensions by grade type ID
plot(summarize(odr.by.school$oss, by=odr.by.school$nces_locale, mean,
               na.rm=TRUE),
     main="Number of Suspensions Per Grade Type ID",
     ylab="Average Number of Suspensions",
     xlab="Grade Type ID")
# Boxplot of OSS by grade type ID
boxplot(oss ~ grade.type, data=odr.by.school,
        main="Suspensions",
        ylab="Number of OSS",
        xlab="Grade ID")

# Average number of expulsions by school year
plot(summarize(odr.by.school$expulsion, by=odr.by.school$school_schoolyear,
               mean, na.rm=TRUE),
     main="Number of Expulsions Per School Year",
     ylab="Average Number of Expulsions",
     xlab="School Year")
# Boxplot of expulsions by school year
boxplot(expulsion ~ school_schoolyear, data=odr.by.school, 
        main="Expulsions",
        ylab="Number of Expulsions",
        xlab="School Year")


# Number of Major ODRs by 100 Students by year
plot(summarize(a$, by=odr.by.school$school_schoolyear, mean, na.rm=TRUE),
     main="Number of ODRs per 100 Students Per School Year",
     ylab="Average Number of ODRs per 100 Students",
     xlab="School Year")


# Recividism per student
summarize(c$student_studentid, c$school_schoolyear)


# Average EBS scores by school year
plot(ebs.group1.by.year,
     main="Average Score of Schoolwide Discipline System Per School Year",
     ylab="Average Score",
     xlab="School Year")
plot(ebs.group2.by.year,
     main="Average Score of Non-classroom Set Per School Year",
     ylab="Average Score",
     xlab="School Year")
plot(ebs.group3.by.year,
     main="Average Score of Classroom Set Per School Year",
     ylab="Average Score",
     xlab="School Year")
plot(ebs.group4.by.year,
     main="Average Score of Individual Student System Per School Year",
     ylab="Average Score",
     xlab="School Year")

ebs.groups.by.year <- merge(ebs.group1.by.year, ebs.group2.by.year,
                            by="b$school_schoolyear")
ebs.groups.by.year <- merge(ebs.groups.by.year, ebs.group3.by.year,
                            by="b$school_schoolyear")
ebs.groups.by.year <- merge(ebs.groups.by.year, ebs.group4.by.year,
                            by="b$school_schoolyear")

# All 4 EBS groups on one plot
plot(rep(ebs.groups.by.year[, 1], 4),
     c(ebs.groups.by.year[, 2],
       ebs.groups.by.year[, 3],
       ebs.groups.by.year[, 4],
       ebs.groups.by.year[, 5]),
     col=rep(c("red", "green", "blue", "purple"),
             e=length(ebs.groups.by.year[, 1])),
     pch=rep(c(15:18), e=length(ebs.groups.by.year[, 1])),
     main="Average Scores of All 4 EBS Measures Per School Year",
     ylab="Average Score",
     xlab="School Year")
     axis(1, at=1:4, labels=c("2004-05", "2005-06", "2006-07", "2007-08"))
     
     





# LINEAR REGRESSION MODELS #####################################################
lm(perc.in.place.sw.disc.sys    ~ year, data=lm.data)
lm(perc.in.place.ncr.set        ~ year, data=lm.data)
lm(perc.in.place.cr.set         ~ year, data=lm.data)
lm(perc.in.place.indiv.stud.sys ~ year, data=lm.data)


lm(major.odrs ~ perc.in.place.sw.disc.sys +
                perc.in.place.ncr.set +
                perc.in.place.cr.set +
                perc.in.place.indiv.stud.sys, data=lm.data)



# LONGITUDINAL MIXED EFFECTS REGRESSION MODELS #################################
# ODR
# logistic regression model 1 - demographics only
lmer(major.odrs ~ year +
                     (perc.in.place.sw.disc.sys    | school.id) +
                     (perc.in.place.ncr.set        | school.id) +
                     (perc.in.place.cr.set         | school.id) +
                     (perc.in.place.indiv.stud.sys | school.id) +
                      grade.type +  # fixed
                     (grade.06.prop                | school.id) +
                     (grade.07.prop                | school.id) +
                     (grade.08.prop | school.id) +
                     (grade.09.prop | school.id) +
                     (grade.10.prop | school.id) +
                     (grade.11.prop | school.id) +
                     (grade.12.prop | school.id) +
                     (male.prop | school.id) +
                     (afram.prop | school.id) +
                     (asian.prop | school.id) +
                     (latino.prop | school.id) +
#                     (white.prop | school.id) +
                     (percent.frpm | school.id) +
                     (nces_member | school.id) +
                     (enrollment.sq | school.id) +
                     (school.id ) +
                     (district.id ) +
                     (fte.per.student | school.id),
                     data=lm.data, family=gaussian(link = "identity"),
                     subset=grade.type == 1)
#logistic regression model 2 - 
glm(odr ~ grade + gender + ethnicity + frpm +
          enroll + school.fte.student,
          family=binomial(link="logit"), data=lm.data, ) 
#logistic regression model 3 - 
glm(odr ~    school.grade + school.gender + school.ethnicity + school.frpm +
          school.total.enroll + school.fte.student,

          family=binomial(link="logit"), data=lm.data, ) 
#logistic regression model 4 - 
glm( ~    school.grade + school.gender + school.ethnicity + school.frpm +
          school.total.enroll + school.fte.student,

          family=binomial(link="logit"), data=lm.data, ) 
#logistic regression model 5 - 
glm( ~    school.grade + school.gender + school.ethnicity + school.frpm +
          school.total.enroll + school.fte.student,

          family=binomial(link="logit"), data=lm.data, ) 

# SUSPENSIONS
# logistic regression model 1 - demographics only
glm(susp ~ school.grade + school.gender + school.ethnicity + school.frpm +
           school.total.enroll + school.fte.student,
           family=binomial(link="logit"), data=, ) 
#logistic regression model 2 - 
glm(susp ~  
          family=binomial(link="logit"), data=, ) 
#logistic regression model 3 - 
glm( ~ 
          family=binomial(link="logit"), data=, ) 
#logistic regression model 4 - 
glm( ~ 
          family=binomial(link="logit"), data=, ) 
#logistic regression model 5 - 
glm( ~ 
          family=binomial(link="logit"), data=, ) 

# EXPULSIONS
# logistic regression model 1 - demographics only
glm(expulsions ~ school.grade + school.gender + school.ethnicity + school.frpm +
          school.total.enroll + school.fte.student,
          family=binomial(link="logit"), data=, ) 
#logistic regression model 2 - 
glm(expulsions ~  
          family=binomial(link="logit"), data=, ) 
#logistic regression model 3 - 
glm( ~ 
          family=binomial(link="logit"), data=, ) 
#logistic regression model 4 - 
glm( ~ 
          family=binomial(link="logit"), data=, ) 
#logistic regression model 5 - 
glm( ~ 
          family=binomial(link="logit"), data=, ) 


# NON-LINEAR REGRESSION MODELS #################################################
#non-linear mixed effects model
nlme(odr_dayssuspended ~ 
#non-linear mixed effects model
nlme(odr_daysexpelled ~  

sink() #close link to output file
