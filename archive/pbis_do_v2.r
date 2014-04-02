sink(file="/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/results/pbis_analysis_output.txt") #send output to file

# PBIS Do File
# By Casey Tsui
# Format: R
# Last updated: 2/7/11


# Author Comment: This file contains the coding for analysis and graphing for 
# the PBIS data set.
#   Calls the functions defined in pbis_clean.r to clean the data.
#   Calls the functions defined in pbis_func.r to perform the analysis and
#   produces charts and tables.


setwd("/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/coding")
source("pbis_clean.r")
setwd("/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/coding")
source("pbis_func.r")
library(ggplot2)
library(gee)
#library(geepack)
#library(lme4)
library(pwr)
library(multilevel)



# EVALUATING INTERNAL CONSISTENCY OF THE TWO PBIS FIDELITY MEASURES ############
# Cronbach's Alpha on SET scores to measure internal consistency
#   Closer to 1, the better
# Overall
cronbach(a[, c(155:166, 168:176, 178:184)])

# Cronbach's Alpha on EBS scores to measure internal consistency
#   Closer to 1, the better
# Overall
cronbach(a[, c(seq(51, 141, 2))])

# Schoolwide Discipline System
cronbach(a[, c(seq(51,  85,  2))])
# Nonclassroom Setting
cronbach(a[, c(seq(87,  103, 2))])
# Classroom Setting
cronbach(a[, c(seq(105, 125, 2))])
# Individual Student System
cronbach(a[, c(seq(127, 141, 2))])


# CORRELATION ANALYSIS OF OUTCOME VARIABLES#####################################
disc.var.names <- c("iss.100.day", "oss.100.day", "expul.100.day",
                    "major.100.day", "odr.100.day", "recid", "num.stud",
                    "stud.repeats", "grad.rate", "attend.rate")

lm.data.wide <- reshape(lm.data, v.names=disc.var.names,
                        timevar="school_schoolyear",
                        idvar="school_educationalorgid",
                        direction="wide", sep=".")
lm.data.wide <- lm.data.wide[, c(135:162)]


iss.cors <- lm.data.wide[, which(substr(names(lm.data.wide), 1, 11) == "iss.100.day")]
cor(iss.cors, use="complete.obs")

oss.cors <- lm.data.wide[, which(substr(names(lm.data.wide), 1, 11) == "oss.100.day")]
cor(oss.cors, use="complete.obs")

expul.cors <- lm.data.wide[, which(substr(names(lm.data.wide), 1, 13) == "expul.100.day")]
cor(expul.cors, use="complete.obs")

major.cors <- lm.data.wide[, which(substr(names(lm.data.wide), 1, 13) == "major.100.day")]
cor(major.cors, use="complete.obs")

odr.cors <- lm.data.wide[, which(substr(names(lm.data.wide), 1, 11) == "odr.100.day")]
cor(odr.cors, use="complete.obs")

recid.cors <- lm.data.wide[, which(substr(names(lm.data.wide), 1, 5) == "recid")]
cor(recid.cors, use="complete.obs")


grad.cors <- lm.data.wide[, which(substr(names(lm.data.wide), 1, 9) == "grad.rate")]
cor(grad.cors, use="complete.obs")

attend.cors <- lm.data.wide[, which(substr(names(lm.data.wide), 1, 11) == "attend.rate")]
cor(attend.cors, use="complete.obs")


# UNIVARIATE ANALYSIS ##########################################################
years <- c("2004", "2005", "2006", "2007")


# Table 1
for (i in 1:length(years)) {
  # Loops through 2004 to the last year and makes table.1."year" object
  #   for each year
  x <- subset(lm.data, year == i)
  new.var <- paste("table.1", as.character(years[i]), sep='.')
  table.1 <- list(with(x, length(unique(school_educationalorgid))),
                  with(x, length(unique(school_educationalorgid[grade.type == "MS"]))),
                  with(x, length(unique(school_educationalorgid[grade.type == "HS"]))),
                  with(x, length(unique(school_educationalorgid[grade.type == "Mix"]))),
                  with(x, length(unique(school_educationalorgid[school.statelabel == "CO"]))),
                  with(x, length(unique(school_educationalorgid[school.statelabel == "IL"]))),
                  with(x, length(unique(school_educationalorgid[school.statelabel == "MD"]))),
                  with(x, length(unique(school_educationalorgid[school.statelabel == "MT"]))),
                  with(x, length(unique(school_educationalorgid[school.statelabel == "OR"]))),
                  with(x, length(unique(school_educationalorgid[urbanicity == "City"]))),
                  with(x, length(unique(school_educationalorgid[urbanicity == "Suburb/Fringe"]))),
                  with(x, length(unique(school_educationalorgid[urbanicity == "Town"]))),
                  with(x, length(unique(school_educationalorgid[urbanicity == "Rural"]))),
                  with(x, length(unique(school_districtid))),
                  with(x, mean(nces_member,     na.rm=TRUE)),
                  with(x, mean(male.prop,       na.rm=TRUE) * 100),
                  with(x, mean(afram.prop,      na.rm=TRUE) * 100),
                  with(x, mean(aian.prop,       na.rm=TRUE) * 100),
                  with(x, mean(asian.prop,      na.rm=TRUE) * 100),
                  with(x, mean(latino.prop,     na.rm=TRUE) * 100),
                  with(x, mean(white.prop,      na.rm=TRUE) * 100),
                  with(x, mean(frpm.prop,       na.rm=TRUE)),
                  with(x, mean(fte.100.student, na.rm=TRUE)))
  assign(new.var, table.1, envir=.GlobalEnv)  # Makes new table.1."year" object 
}

names(table.1.2004) <- c("Number of Total Schools",  # Names table rows
                         "Number of Middle Schools",
                         "Number of High Schools",
                         "Number of Mixed Grade Schools",
                         "Number of CO schools",
                         "Number of IL schools",
                         "Number of MD schools",
                         "Number of MT schools",
                         "Number of OR schools",
                         "Number of Schools in City Setting",
                         "Number of Schools in Suburban/Fringe Setting",
                         "Number of Schools in Town Setting",
                         "Number of Schools in Rural Setting",
                         "Number of School Districts",
                         "Average Enrollment",
                         "Percent Male Enrollment",
                         "Percent African American Enrollment",
                         "Percent American Indian/Alaska Native Enrollment",
                         "Percent Asian American Enrollment",
                         "Percent Latino Enrollment",
                         "Percent Caucasian Enrollment",
                         "Percent Free and Reduced Priced Meals",
                         "Average Number of FTE per 100 Students")

# Output Table 1
table.1 <- cbind(table.1.2004, table.1.2005, table.1.2006, table.1.2007)


# Table 2
for (i in 1:length(years)) {
  # Loops through 2004 to the last year and makes table.2."year" object
  #   for each year
  x <- subset(lm.data, year == i)
  new.var <- paste("table.2", as.character(years[i]), sep='.')
  table.2 <- list(sum(x$odr,            na.rm=TRUE),
                  sum(x$iss,            na.rm=TRUE),
                  sum(x$oss,            na.rm=TRUE),
                  sum(x$expul,          na.rm=TRUE),
                  sum(x$major,          na.rm=TRUE),
                  sum(x$odr,            na.rm=TRUE),
                  mean(x$iss   / x$odr, na.rm=TRUE),
                  mean(x$oss   / x$odr, na.rm=TRUE),
                  mean(x$expul / x$odr, na.rm=TRUE),
                  mean(x$recid,         na.rm=TRUE),
                  mean(x$iss.100,       na.rm=TRUE),
                  mean(x$oss.100,       na.rm=TRUE),
                  mean(x$expul.100,     na.rm=TRUE),
                  mean(x$major.100,     na.rm=TRUE),
                  mean(x$odr.100,       na.rm=TRUE),
                  mean(x$iss.100.day,   na.rm=TRUE),
                  mean(x$oss.100.day,   na.rm=TRUE),
                  mean(x$expul.100.day, na.rm=TRUE),
                  mean(x$major.100.day, na.rm=TRUE),
                  mean(x$odr.100.day,   na.rm=TRUE),
                  mean(x$perc.stud.odr, na.rm=TRUE))
               #   mean(x$odr_dayssuspended[b$odr_admindecisionid==7], na.rm=TRUE),
               #   mean(x$odr_dayssuspended[b$odr_admindecisionid==8], na.rm=TRUE),
               #   mean(x$odr_dayssuspended, na.rm=TRUE),
               #   mean(x$odr_daysexpelled, na.rm=TRUE))
               #   mean(x$grad.rate,     na.rm=TRUE),
               #   mean(x$reading.score, na.rm=TRUE),
               #   mean(x$math.score,    na.rm=TRUE))
  assign(new.var, table.2, envir=.GlobalEnv) 
}

names(table.2.2004) <- c("Number of Total ODRs",  # Names table rows
                         "Number of In-school Suspensions",
                         "Number of Out-of-school Suspensions",
                         "Number of Expulsions",
                         "Number of Major ODRs",
                         "Number of Total ODRs",
                         "Proportion of Total ODRs that are ISS",
                         "Proportion of Total ODRs that are OSS",
                         "Proportion of Total ODRs that are Expulsions",
                         "Average Number of ODRs Among Students With At Least 1 Incident",
                         "In-school Suspensions per 100 Students",
                         "Out-of-school Suspensions per 100 Students",
                         "Expulsions per 100 Students",
                         "Major ODRs per 100 Students",
                         "Total ODRs per 100 Students",
                         "In-school Suspensions per 100 Students per Day",
                         "Out-of-school Suspensions per 100 Students per Day",
                         "Expulsions per 100 Students per Day",
                         "Major ODRs per 100 Students per Day",
                         "Total ODRs per 100 Students per Day",
                         "Average Percent of Students with ODR")
                   #      "Average Number of Days Suspended (ISS)",
                   #      "Average Number of Days Suspended (OSS)",
                   #      "Average Number of Days Suspended (ISS & OSS)",
                   #      "Average Number of Days Expelled",
                   #      "Average Graduation Rate",
                   #      "Average Reading Score",
                   #      "Average Math Score")

# Output Table 2
table.2 <- cbind(table.2.2004, table.2.2005, table.2.2006, table.2.2007)


# Table 3
for (i in 1:length(years)) {
  # Loops through 2004 to the last year and makes table.3."year" object
  #   for each year
  x <- subset(lm.data, year == i)
  school.denom <- with(x, length(unique(school_educationalorgid[year==i])))
  new.var <- paste("table.3", as.character(years[i]), sep='.')
  table.3 <- list(mean(x$perc.in.place.sw.disc.sys, na.rm=TRUE),
                  mean(x$perc.in.place.ncr.set, na.rm=TRUE),
                  mean(x$perc.in.place.cr.set, na.rm=TRUE),
                  mean(x$perc.in.place.ind.stud.sys, na.rm=TRUE),
                  mean(x$set_expectationsdefined, na.rm=TRUE),
                  mean(x$set_expectationstaught, na.rm=TRUE),
                  mean(x$set_rewardsystem, na.rm=TRUE),
                  mean(x$set_violationssystem, na.rm=TRUE),
                  mean(x$set_monitoringevaluation, na.rm=TRUE),
                  mean(x$set_leadership, na.rm=TRUE),
                  mean(x$set_districtsupport, na.rm=TRUE),
                  mean(x$set_implementationaverage, na.rm=TRUE),
                  sum(x$set_criterion8080, na.rm=TRUE) / school.denom)
  assign(new.var, table.3, envir=.GlobalEnv) 
}

names(table.3.2004) <- c("EBS Schoolwide Discipline System",  # Names table rows
                         "EBS Nonclassroom Setting",
                         "EBS Classroom Setting",
                         "EBS Individual Student System",
                         "SET Expectations Defined",
                         "SET Expectations Taught",
                         "SET Reward System",
                         "SET Violation System",
                         "SET Monitoring and Evaluation",
                         "SET Leadership",
                         "SET District Support",
                         "SET Implementation Average",
                         "Percent of Schools Meeting SET Criterion 8080")

# Output Table 3
table.3 <- cbind(table.3.2004, table.3.2005, table.3.2006, table.3.2007)  


# Table of fidelity levels by year - New table 3?
fidelity.table <- with(lm.data, table(fidelity, school_schoolyear))
fidelity.prop.table <- prop.table(fidelity.table, margin=2)





# CORRELATION ANALYSIS
cor(x, y, use="complete.obs")


# BIVARIATE CROSS-TABULATIONS ##################################################
BivarCrossTabs <- function(x) {
  for (i in 1:length(x)) {
    index <- which(names(lm.data) == x[i])
    model <- lm(lm.data[, index] ~ lm.data$fidelity)
    print(x[i])
    print(summary(model))
    print(names(model))
    print(model$coef)
  }
}





# Covariates vs fidelity category
cov.var.names <- list("male.prop", "afram.prop", "aian.prop",
                      "asian.prop", "latino.prop", "white.prop", "frpm.prop",
                     # "special.ed.enroll.prop", "lep.perc",
                      "nces_member",
                      "enrollment.sq.cntr", "fte.100.student",
                      "school.statelabel", "grade.type")
BivarCrossTabs(cov.var.names)


# Outcome variables vs. fidelity category
dep.var.names <- list("iss.100.day", "oss.100.day", "expul.100.day",
                      "major.100.day", "odr.100.day", "recid", "perc.stud.odr")
                     # "grad.rate", "reading.score", "math.score")
BivarCrossTabs(dep.var.names)



# ANOVA
with(lm.data, summary(aov(odr.100.day ~ fidelity)))


# GRAPHS #######################################################################
# Naive Plot of Average EBS scores
plot(lm.data$perc.in.place.sw.disc.sys,
     main="Average Score of Schoolwide Discipline System",
     ylab="Average Score")
plot(lm.data$perc.in.place.ncr.set,
     main="Average Score of Non-classroom Set",
     ylab="Average Score")
plot(lm.data$perc.in.place.cr.set,
     main="Average Score of Classroom Set",
     ylab="Average Score")
plot(lm.data$perc.in.place.ind.stud.sys,
     main="Average Score of Individual Student System",
     ylab="Average Score")



## WORKS #####################################################################
ggplot(lm.data, aes(factor(grade.type), iss.100.day)) +
       geom_boxplot(aes(fill = factor(grade.type))) +
       facet_grid(fidelity ~ year)
ggplot(lm.data[which(lm.data$oss.100.day < 1), ], aes(factor(grade.type), oss.100.day)) +
       geom_boxplot(aes(fill = factor(grade.type))) +
       facet_grid(fidelity ~ year)
ggplot(lm.data[which(lm.data$expul.100.day < 0.025), ], aes(factor(grade.type), expul.100.day)) +
       geom_boxplot(aes(fill = factor(grade.type))) +
       facet_grid(fidelity ~ year)
ggplot(lm.data, aes(factor(grade.type), odr.100.day)) +
       geom_boxplot(aes(fill = factor(grade.type))) +
       facet_grid(fidelity ~ year)
ggplot(lm.data[which(lm.data$recid < 35), ], aes(factor(grade.type), recid)) +
       geom_boxplot(aes(fill = factor(grade.type))) +
       facet_grid(fidelity ~ year)


ggplot(lm.data[which(lm.data$odr<2000), ], aes(factor(grade.type), odr)) +
       geom_boxplot(aes(fill=factor(grade.type))) +
       facet_grid(fidelity~year)
################################################################################

# Boxplots of outcome variables by year
boxplot(lm.data.2004$iss, lm.data.2005$iss, lm.data.2006$iss, lm.data.2007$iss,
        main="Mean Number of In-school Suspensions Over Time",
        ylab="Mean Number of In-school Suspensions",
        xlab="Year")

p <- ggplot(lm.data, aes(factor(year), iss))
p + geom_boxplot()
p + geom_boxplot(aes(fill = factor(grade.type)))


boxplot(lm.data.2004$oss, lm.data.2005$oss, lm.data.2006$oss, lm.data.2007$oss)

p <- ggplot(lm.data, aes(factor(year), oss))
p + geom_boxplot()
p + geom_boxplot(aes(fill = factor(grade.type)))


boxplot(lm.data.2004$expul, lm.data.2005$expul, lm.data.2006$expul,
        lm.data.2007$expul)

p <- ggplot(lm.data, aes(factor(year), expul))
p + geom_boxplot()
p + geom_boxplot(aes(fill = factor(grade.type)))

boxplot(lm.data.2004$major, lm.data.2005$major, lm.data.2006$major,
        lm.data.2007$major)
boxplot(lm.data.2004$odr, lm.data.2005$odr, lm.data.2006$odr,
        lm.data.2007$odr)


# Boxplots of outcome variables per 100 Students by year
boxplot(lm.data.2004$susp.100, lm.data.2005$susp.100, lm.data.2006$susp.100,
        lm.data.2007$susp.100,
        main="Mean Number of Suspensions Per 100 Students Over Time",
        ylab="Mean Number of Suspensions Per 100 Students",
        xlab="Year")
boxplot(lm.data.2004$expul.100, lm.data.2005$expul.100, lm.data.2006$expul.100,
        lm.data.2007$expul.100)
boxplot(lm.data.2004$major.100, lm.data.2005$major.100, lm.data.2006$major.100,
        lm.data.2007$major.100)
boxplot(lm.data.2004$odr.100, lm.data.2005$odr.100, lm.data.2006$odr.100,
        lm.data.2007$odr.100)


# Boxplots of outcome variables per 100 Students by year
p <- ggplot(lm.data, aes(factor(year), oss.100.day))

boxplot(lm.data.2004$susp.100.day, lm.data.2005$susp.100.day,
        lm.data.2006$susp.100.day, lm.data.2007$susp.100.day,
        main="Mean Number of Suspensions Per 100 Students Per Day Over Time",
        ylab="Mean Number of Suspensions Per 100 Students Per Day",
        xlab="Year")
boxplot(lm.data.2004$expul.100.day, lm.data.2005$expul.100.day,
        lm.data.2006$expul.100.day, lm.data.2007$expul.100.day)
boxplot(lm.data.2004$major.100.day, lm.data.2005$major.100.day,
        lm.data.2006$major.100.day, lm.data.2007$major.100.day)
boxplot(lm.data.2004$odr.100.day,   lm.data.2005$odr.100.day,
        lm.data.2006$odr.100.day,   lm.data.2007$odr.100.day)


# Boxplots of independent variables by year
boxplot(lm.data.2004$perc.in.place.sw.disc.sys,
        lm.data.2005$perc.in.place.sw.disc.sys,
        lm.data.2006$perc.in.place.sw.disc.sys,
        lm.data.2007$perc.in.place.sw.disc.sys,
        main="Changes in EBS Measure 1 over time",
        xlab="Year")
boxplot(lm.data.2004$perc.in.place.ncr.set,
        lm.data.2005$perc.in.place.ncr.set,
        lm.data.2006$perc.in.place.ncr.set,
        lm.data.2007$perc.in.place.ncr.set,
        main="Changes in EBS Measure 2 over time",
        xlab="Year")
boxplot(lm.data.2004$perc.in.place.cr.set,
        lm.data.2005$perc.in.place.cr.set,
        lm.data.2006$perc.in.place.cr.set,
        lm.data.2007$perc.in.place.cr.set,
        main="Changes in EBS Measure 3 over time",
        xlab="Year")
boxplot(lm.data.2004$perc.in.place.ind.stud.sys,
        lm.data.2005$perc.in.place.ind.stud.sys,
        lm.data.2006$perc.in.place.ind.stud.sys,
        lm.data.2007$perc.in.place.ind.stud.sys,
        main="Changes in EBS Measure 4 over time",
        xlab="Year")


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
     



x1 <- summarize(lm.data$perc.in.place.sw.disc.sys, lm.data$year, mean, na.rm=TRUE)
x2 <- summarize(lm.data$perc.in.place.ncr.set, lm.data$year, mean, na.rm=TRUE)
x3 <- summarize(lm.data$perc.in.place.cr.set, lm.data$year, mean, na.rm=TRUE)
x4 <- summarize(lm.data$perc.in.place.ind.stud.sys, lm.data$year, mean, na.rm=TRUE)

x <- cbind(x1, x2[, 2], x3[, 2], x4[, 2])

plot(rep(x[, 1], 4),
     c(x[, 2], x[, 3], x[, 4], x[, 5]),
     col=rep(c("red", "green", "blue", "purple"),
             e=length(x[, 1])),
     pch=rep(c(15:18), e=length(x[, 1])),
     main="Average Scores of All 4 EBS Measures Per School Year",
     ylab="Average Score",
     xlab="School Year")
axis(1, at=1:4, labels=c("2004-05", "2005-06", "2006-07", "2007-08"))
 


# NAIVE LINEAR REGRESSION MODELS ###############################################
# Independent variables over year
summary(lm(perc.in.place.sw.disc.sys  ~ year, data=lm.data))
summary(lm(perc.in.place.ncr.set      ~ year, data=lm.data))
summary(lm(perc.in.place.cr.set       ~ year, data=lm.data))
summary(lm(perc.in.place.ind.stud.sys ~ year, data=lm.data))


# Suspensions with EBS independent variables - Poisson Regression
summary(glm(susp ~ perc.in.place.sw.disc.sys + perc.in.place.ncr.set +
            perc.in.place.cr.set + perc.in.place.ind.stud.sys, 
            family=poisson, data=lm.data))

# Expulsions with EBS independent variables - Poisson Regression
summary(glm(expul ~ perc.in.place.sw.disc.sys + perc.in.place.ncr.set +
            perc.in.place.cr.set + perc.in.place.ind.stud.sys, 
            family=poisson, data=lm.data))

# Major ODRs with EBS independent variables - Poisson Regression
summary(glm(major ~ perc.in.place.sw.disc.sys + perc.in.place.ncr.set +
           perc.in.place.cr.set + perc.in.place.ind.stud.sys, 
           family=poisson, data=lm.data))


# Suspensions per 100 students with EBS independent variables - OLS Regression
summary(lm(susp.100 ~ perc.in.place.sw.disc.sys + perc.in.place.ncr.set +
            perc.in.place.cr.set + perc.in.place.ind.stud.sys, data=lm.data))

# Expulsions per 100 students with EBS independent variables - OLS Regression
summary(lm(expul..100 ~ perc.in.place.sw.disc.sys + perc.in.place.ncr.set +
            perc.in.place.cr.set + perc.in.place.ind.stud.sys, data=lm.data))

# Major ODRs per 100 students with EBS independent variables - OLS Regression
summary(lm(major.100 ~ perc.in.place.sw.disc.sys + perc.in.place.ncr.set +
            perc.in.place.cr.set + perc.in.place.ind.stud.sys, data=lm.data))

# LONGITUDINAL MIXED EFFECTS REGRESSION MODELS #################################
# GEE Major ODRs per 100 - model 1 - BASE mode, demographics only
model1 <- gee(major.100 ~ school_schoolyear +
                          perc.in.place.sw.disc.sys +
                          perc.in.place.ncr.set +
                          perc.in.place.cr.set +
                          perc.in.place.ind.stud.sys +
                          male.prop +
                          grade.type,
                          id=school_educationalorgid,
                          data=lm.data, family=gaussian, corstr="AR-M", Mv=1)


######CALCULATE PVALUE USING A NORMAL APPROXIMATION FOR THE DIST OF Z
p.model1 <- cbind(2 * pnorm(abs(coef(summary(model1))[,5]), lower.tail = FALSE))




model1 <- gee(major.100 ~ school_schoolyear +
                          medium.fidelity +
                          high.fidelity +
                          high.school,
                          id=school_educationalorgid,
                          data=lm.data, family=gaussian, corstr="AR-M", Mv=1)















# GEE Major ODRs per 100 students - model 2 - demographics + socio-economic
model2 <- gee(major.100 ~ school_schoolyear +
                          perc.in.place.sw.disc.sys +
                          perc.in.place.ncr.set +
                          perc.in.place.cr.set +
                          perc.in.place.ind.stud.sys +
                          male.prop +
                          grade.type +
                          nonwhite.prop +
                          frpm.prop,
                          id=school_educationalorgid,
                          data=lm.data, family=gaussian, corstr="AR-M", Mv=1)

######CALCULATE PVALUE USING A NORMAL APPROXIMATION FOR THE DIST OF Z
p.model2 <- cbind(2 * pnorm(abs(coef(summary(model2))[,5]), lower.tail = FALSE))

# GEE Major ODRs per 100 students - model 3 - demographics + school environment
model3 <- gee(major.100 ~ school_schoolyear +
                     perc.in.place.sw.disc.sys +
                     perc.in.place.ncr.set +
                     perc.in.place.cr.set +
                     perc.in.place.ind.stud.sys +
                     male.prop +
                     grade.type +
                     nces_member +
                     enrollment.sq +
                     fte.100.student +
                     urbanicity,
                     id=school_educationalorgid,
                     data=lm.data, family=gaussian, corstr="AR-M", Mv=1)
 
######CALCULATE PVALUE USING A NORMAL APPROXIMATION FOR THE DIST OF Z
p.model3 <- cbind(2 * pnorm(abs(coef(summary(model2))[,5]), lower.tail = FALSE))

# GEE Major ODRs per 100 students - model 4 - FULL model
model4 <- gee(major.100 ~ school_schoolyear +
                          perc.in.place.sw.disc.sys +
                          perc.in.place.ncr.set +
                          perc.in.place.cr.set +
                          perc.in.place.ind.stud.sys +
                          male.prop +
                          grade.type +
                          nonwhite.prop +
                          frpm.prop +
                          nces_member +
                          enrollment.sq +
                          fte.100.student +
                          urbanicity,
                          id=school_educationalorgid,
                          data=lm.data, family=gaussian, corstr="AR-M", Mv=1)
 
######CALCULATE PVALUE USING A NORMAL APPROXIMATION FOR THE DIST OF Z
p.model4 <- cbind(2 * pnorm(abs(coef(summary(model4))[,5]), lower.tail = FALSE))
p.model4 <- data.frame(p.model4)
m <- ifelse(p.model4 <= 0.001,
            "***",
            ifelse(p.model4 > 0.001 & p.model4 <= 0.01,
                   "**",
                   ifelse(p.model4 > 0.01 & p.model4 <= 0.05,
                          "*",
                          "")))


# SUSPENSIONS ##################################################################
model1 <- gee(susp.100 ~ school_schoolyear +
                          perc.in.place.sw.disc.sys +
                          perc.in.place.ncr.set +
                          perc.in.place.cr.set +
                          perc.in.place.ind.stud.sys +
                          male.prop +
                          grade.type,
                          id=school_educationalorgid,
                          data=lm.data, family=gaussian, corstr="AR-M", Mv=1)


######CALCULATE PVALUE USING A NORMAL APPROXIMATION FOR THE DIST OF Z
p.model1 <- cbind(2 * pnorm(abs(coef(summary(model1))[,5]), lower.tail = FALSE))










sink()  # Close link to output file
