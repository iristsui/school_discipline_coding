
# PBIS Do File
# By Casey Tsui
# Format: R
# Last updated: 7/19/11


# Author Comment: This file contains the coding for analysis and graphing for 
# the PBIS data set.



library(ggplot2)
library(gee)
#library(geepack)
#library(lme4)
library(pwr)
library(multilevel)


# FUNCTION DEFINITIONS #########################################################
ReshapeWide <- function(x) {
  # Args:
  #   x: Data frame
  reshape(x, v.names=disc.var.names,
             timevar="school_schoolyear",
             idvar="school_educationalorgid",
             direction="wide", sep=".")
}


PrintCors <- function(x) {
  iss.cors <- x[, which(substr(names(x), 1, 3) == "iss")]
  print(cor(iss.cors, use="complete.obs"))

  oss.cors <- x[, which(substr(names(x), 1, 3) == "oss")]
  print(cor(oss.cors, use="complete.obs"))

#  expul.cors <- x[, which(substr(names(x), 1, 5) == "expul")]
#  cor(expul.cors, use="complete.obs")

  major.cors <- x[, which(substr(names(x), 1, 5) == "major")]
  print(cor(major.cors, use="complete.obs"))

  odr.cors <- x[, which(substr(names(x), 1, 3) == "odr")]
  print(cor(odr.cors, use="complete.obs"))

  recid.cors <- x[, which(substr(names(x), 1, 5) == "recid")]
  print(cor(recid.cors, use="complete.obs"))

  num.stud.cors <- x[, which(substr(names(x), 1, 8) == "num.stud")]
  print(cor(num.stud.cors, use="complete.obs"))

#  stud.repeats.cors <- x[, which(substr(names(x), 1, 12) == "stud.repeats")]
#  print(cor(stud.repeats.cors, use="complete.obs"))

  grad.cors <- x[, which(substr(names(x), 1, 9) == "grad.rate")]
  print(cor(grad.cors, use="complete.obs"))

  attend.cors <- x[, which(substr(names(x), 1, 11) == "attend.rate")]
  print(cor(attend.cors, use="complete.obs"))
}


# Fit distributions using log likelihood
FitDistr <- function(x) {
  # Args:
  #   x: String of response variable to fit
  dists <- c("normal", "logistic", "lognormal", "Poisson", "negative binomial")
  dep.var <- lm.data[, x]
  for (i in seq(along=dists)) {
    print(paste(dists[i], ": ", fitdistr(na.exclude(dep.var), dists[i])$loglik))
  }
}


DepVarCorrs <- function(x) {
  # Args:
  #   x: data frame variable - pbis measure
  lm.data.wide <- lm.data[which(!is.na(x)), c(id.names, disc.var.names)]
  lm.data.wide <- ReshapeWide(lm.data.wide)
  PrintCors(lm.data.wide)
}


CreateTable1 <- function(x1) {
  # Args:
  #   x: Data frame
  years <- c("2004", "2005", "2006", "2007", "Total")
  table.1 <- vector("list", 8)
  table.1[[1]] <- c("Number of Total Schools",  # Names table rows
                    "Number of Middle Schools",
                    "Number of High Schools",
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
                    "Percent American Indian/Alaska Native Enrollment",
                    "Percent Asian American Enrollment",
                    "Percent Latino Enrollment",
                    "Percent Caucasian Enrollment",
                    "Percent Free and Reduced Priced Meals",
                    "Average Number of Students per FTE")

  for (i in 1:length(years)) {
    # Loops through 2004 to the last year and makes table.1."year" object
    #   for each year
    if (i <= 4) x <- subset(x1, year == i)
    else x <- x1
    table.1[[i+1]] <- list(with(x, length(unique(school_educationalorgid))),
                           with(x, length(unique(school_educationalorgid[grade.type == "MS"]))),
                           with(x, length(unique(school_educationalorgid[grade.type == "HS"]))),
                           with(x, length(unique(school_educationalorgid[school.statelabel.x == "CO"]))),
                           with(x, length(unique(school_educationalorgid[school.statelabel.x == "IL"]))),
                           with(x, length(unique(school_educationalorgid[school.statelabel.x == "MD"]))),
                           with(x, length(unique(school_educationalorgid[school.statelabel.x == "MT"]))),
                           with(x, length(unique(school_educationalorgid[school.statelabel.x == "OR"]))),
                           with(x, length(unique(school_educationalorgid[urbanicity == "City"]))),
                           with(x, length(unique(school_educationalorgid[urbanicity == "Suburb/Fringe"]))),
                           with(x, length(unique(school_educationalorgid[urbanicity == "Town"]))),
                           with(x, length(unique(school_educationalorgid[urbanicity == "Rural"]))),
                           with(x, length(unique(school_districtid))),
                           with(x, mean(nces_member,     na.rm=TRUE)),
                           with(x, mean(male.perc,       na.rm=TRUE)),
                           with(x, mean(afram.perc,      na.rm=TRUE)),
                           with(x, mean(asian.perc,      na.rm=TRUE)),
                           with(x, mean(latino.perc,     na.rm=TRUE)),
                           with(x, mean(white.perc,      na.rm=TRUE)),
                           with(x, mean(frpm.perc,       na.rm=TRUE)),
                           with(x, mean(stud.per.fte, na.rm=TRUE)))
  }
  empties <- rep(NA, 13)
  all.covars <- c("nces_member", "male.perc", "afram.perc", 
                  "asian.perc", "latino.perc", "white.perc", "frpm.perc",
                  "stud.per.fte")
 
  p <- vector("numeric", 8)
  f.stat <- vector("numeric", 8)
  for (j in seq(along=all.covars)) {
    dep.var <- x1[, all.covars[j]]
    model <- lm(dep.var ~ year, data=x1)
    summary.model <- summary(model)
    p[j] <- coef(summary.model)[2, 4]
    f.stat[j] <- as.numeric(summary.model$fstatistic[1])
  }
  table.1[[7]] <- c(empties, f.stat)
  table.1[[8]] <- c(empties, p)
  table <- do.call("cbind", table.1)
  colnames(table) <- c("Characteristic", "2004", "2005", "2006", "2007", "Total", "F-statistic", "p-value")
  return(table)
}




# FOR STUDENT LEVEL DATA - HERE FOR REFERENCE ONLY
#enrolled <- sum(lm.data$nces_member, na.rm=TRUE)
#lm.data <- with(lm.data, transform(lm.data,
#                          stud_ms = sum(nces_member[grade.type == "MS"]) / enrolled * 100,
#                          stud_hs = sum(nces_member[grade.type == "HS"]) / enrolled * 100,
#                          stud_co = sum(nces_member[school.statelabel.x == "CO"]) / enrolled * 100,
#                          stud_il = sum(nces_member[school.statelabel.x == "IL"]) / enrolled * 100,
#                          stud_md = sum(nces_member[school.statelabel.x == "MD"]) / enrolled * 100,
##                         stud_mt = sum(nces_member[school.statelabel.x == "MT"]) / enrolled * 100,
#                          stud_or = sum(nces_member[school.statelabel.x == "OR"]) / enrolled * 100,
#                          stud_city = sum(nces_member[urbanicity == "City"]) / enrolled * 100,
#                          stud_sub = sum(nces_member[urbanicity == "Suburb/Fringe"]) / enrolled * 100,
#                          stud_town = sum(nces_member[urbanicity == "Town"]) / enrolled * 100,
#                          stud_rural = sum(nces_member[urbanicity == "Rural"]) / enrolled * 100))



CreateTableStudents <- function(x1) {
  # Args:
  #   x: Data frame
  years <- c("2004", "2005", "2006", "2007", "Total")
  table.students <- vector("list", 6)
  table.students[[1]] <- c("Number of Total Students",  # Names table rows
                           "Percent of Students in Middle School",
                           "Percent of Students in High School",
                           "Percent of Students in CO Students",
                           "Percent of Students in IL Students",
                           "Percent of Students in MD Students",
#                          "Percent of Students in MT Students",
                           "Percent of Students in OR Students",
                           "Percent of Students in City Setting",
                           "Percent of Students in Suburban/Fringe Setting",
                           "Percent of Students in Town Setting",
                           "Percent of Students in Rural Setting")

  for (i in 1:length(years)) {
    # Loops through 2004 to the last year and makes table.students."year" object
    #   for each year
    if (i <= 4) {
      x <- subset(x1, year == i)
      enrolled <- sum(x$nces_member[which(x$year == i)], na.rm=TRUE)
    } else {
      x <- x1
      enrolled <- sum(x$nces_member, na.rm=TRUE)
    }
    table.students[[i+1]] <- list(with(x, sum(nces_member, na.rm=TRUE)),
                                  with(x, sum(nces_member[grade.type == "MS"]) / enrolled * 100),
                                  with(x, sum(nces_member[grade.type == "HS"]) / enrolled * 100),
                                  with(x, sum(nces_member[school.statelabel.x == "CO"]) / enrolled * 100),
                                  with(x, sum(nces_member[school.statelabel.x == "IL"]) / enrolled * 100),
                                  with(x, sum(nces_member[school.statelabel.x == "MD"]) / enrolled * 100),
#                                 with(x, sum(nces_member[school.statelabel.x == "MT"]) / enrolled * 100),
                                  with(x, sum(nces_member[school.statelabel.x == "OR"]) / enrolled * 100),
                                  with(x, sum(nces_member[urbanicity == "City"]) / enrolled * 100),
                                  with(x, sum(nces_member[urbanicity == "Suburb/Fringe"]) / enrolled * 100),
                                  with(x, sum(nces_member[urbanicity == "Town"]) / enrolled * 100),
                                  with(x, sum(nces_member[urbanicity == "Rural"]) / enrolled * 100))
  }
  table <- do.call("cbind", table.students)
  colnames(table) <- c("Characteristic", "2004", "2005", "2006", "2007", "Total")
  return(table)
}




CreateTable2 <- function(x1) {
  years <- c("2004", "2005", "2006", "2007", "Total")
  table.2 <- vector("list", 8)
  table.2[[1]] <- c("Number of Total ODRs",  # Names table rows
                    "Number of In-school Suspensions",
                    "Number of Out-of-school Suspensions",
                    "Number of Expulsions",
                    "Number of Major ODRs",
                    "Average Major ODRs per 100 students per day",
                    "Average OSS per 100 students per day",
                    "Percentage of Major ODRs that are ISS",
                    "Percentage of Major ODRs that are OSS",
                    "Percentage of Major ODRs that are Expulsions",
                    "Percentage of Students with at least 2 Disc. Incidents in the Same Year",
                    "Percentage of Students with at least 1 Disc. Incident",
                    "Percentage of Students with ODRs who had an ODR in the Previous Year",
                    "Percentage of Students with at least 1 Truancy",
                    "Percent of Students Proficient or Advanced in Reading",
                    "Percent of Students Proficient or Advanced in Math",
                    "Ratio of African American OSSs over non-African American OSSs",
                    "Ratio of Asian OSSs over non-Asian OSSs",
                    "Ratio of Latino OSSs over non-Latino OSSs",
                    "Ratio of African American ODRs over non-African American ODRs",
                    "Ratio of Asian ODRs over non-Asian ODRs",
                    "Ratio of Latino ODRs over non-Latino ODRs",
                    "Ratio of African American ODRs over White ODRs",
                    "Ratio of Asian ODRs over White ODRs",
                    "Ratio of Latino ODRs over White ODRs")

   x1 <- transform(x1,
                   iss.perc = iss / major * 100,
                   oss.perc = oss / major * 100,
                   expul.perc = expul / major * 100,
                   num.stud.perc = num.stud / nces_member * 100,
                   stud.repeats.perc = stud.repeats / nces_member * 100)

  for (i in 1:length(years)) {
    # Loops through 2004 to the last year and makes table.2."year" object
    #   for each year
    if (i <= 4) x <- subset(x1, year == i)
    else x <- x1
    table.2[[i+1]] <- list(sum(x$odr,            na.rm=TRUE),
                           sum(x$iss,            na.rm=TRUE),
                           sum(x$oss,            na.rm=TRUE),
                           sum(x$expul,          na.rm=TRUE),
                           sum(x$major,          na.rm=TRUE),
                           mean(x$major.100.day, na.rm=TRUE),
                           mean(x$oss.100.day, na.rm=TRUE),
                           mean(x$iss.perc, na.rm=TRUE),
                           mean(x$oss.perc, na.rm=TRUE),
                           mean(x$expul.perc, na.rm=TRUE),
                           mean(x$recid,         na.rm=TRUE) * 100,
                           mean(x$num.stud.perc, na.rm=TRUE),
                           mean(x$stud.repeats.perc, na.rm=TRUE),
                           mean(x$perc.unexcused, na.rm=TRUE) * 100,
                           mean(x$reading.score, na.rm=TRUE) * 100,
                           mean(x$math.score,    na.rm=TRUE) * 100,
                           mean(x$afram.oss.disp.index, na.rm=TRUE),
                           mean(x$asian.oss.disp.index, na.rm=TRUE),
                           mean(x$latino.oss.disp.index, na.rm=TRUE),
                           mean(x$afram.major.disp.index, na.rm=TRUE),
                           mean(x$asian.major.disp.index, na.rm=TRUE),
                           mean(x$latino.major.disp.index, na.rm=TRUE),
                           mean(x$afram.major.over.white, na.rm=TRUE),
                           mean(x$asian.major.over.white, na.rm=TRUE),
                           mean(x$latino.major.over.white, na.rm=TRUE))
  }
  empties <- rep(NA, 5)
  all.covars <- c("iss.perc", "oss.perc", "expul.perc", "recid",
                  "num.stud.perc", "stud.repeats.perc", "perc.unexcused",
                  "reading.score", "math.score", "afram.oss.disp.index",
                  "asian.oss.disp.index", "latino.oss.disp.index",
                  "afram.major.disp.index", "asian.major.disp.index",
                  "latino.major.disp.index", "afram.major.over.white",
                  "asian.major.over.white", "latino.major.over.white")

  p <- vector("numeric", 18)
  f.stat <- vector("numeric", 18)
  for (j in seq(along=all.covars)) {
    dep.var <- x1[, all.covars[j]]
    model <- lm(dep.var ~ year, data=x1)
    summary.model <- summary(model)
    p[j] <- coef(summary.model)[2, 4]
    f.stat[j] <- as.numeric(summary.model$fstatistic[1])
  }
  table.2[[7]] <- c(empties, f.stat)
  table.2[[8]] <- c(empties, p)

  table <- do.call("cbind", table.2)
  colnames(table) <- c("Characteristic", "2004", "2005", "2006", "2007", "Total", "F-statistic", "p-value")
  return(table)
}




CreateTable3 <- function(x1) {
  # Args:
  #   x: Data frame
  years <- c("2004", "2005", "2006", "2007", "Total")
  table.3 <- vector("list", 8)
  table.3[[1]] <- c("EBS Schoolwide Discipline System",  # Names table rows
                    "EBS Nonclassroom Setting",
                    "EBS Classroom Setting",
                    "EBS Individual Student System",
                    "EBS Implementation Average",
                    "Percent of EBS Schools Meeting EBS Criterion 8080",
                    "SET Expectations Defined",
                    "SET Expectations Taught",
                    "SET Reward System",
                    "SET Violation System",
                    "SET Monitoring and Evaluation",
                    "SET Leadership",
                    "SET District Support",
                    "SET Implementation Average",
                    "Percent of SET Schools Meeting SET Criterion 8080")

  for (i in seq(along=years)) {
    # Loops through 2004 to the last year and makes table.3."year" object
    #   for each year
    if (i <= 4) x <- subset(x1, year == i)
    else x <- x1
    table.3[[i+1]] <- list(mean(x$perc.in.place.sw.disc.sys, na.rm=TRUE),
                           mean(x$perc.in.place.ncr.set, na.rm=TRUE),
                           mean(x$perc.in.place.cr.set, na.rm=TRUE),
                           mean(x$perc.in.place.ind.stud.sys, na.rm=TRUE),
                           mean(x$ebs.implementationaverage, na.rm=TRUE),
                           mean(x$ebs.criterion8080, na.rm=TRUE) * 100,
                           mean(x$set.expectationsdefined, na.rm=TRUE),
                           mean(x$set.expectationstaught, na.rm=TRUE),
                           mean(x$set.rewardsystem, na.rm=TRUE),
                           mean(x$set.violationssystem, na.rm=TRUE),
                           mean(x$set.monitoringevaluation, na.rm=TRUE),
                           mean(x$set.leadership, na.rm=TRUE),
                           mean(x$set.districtsupport, na.rm=TRUE),
                           mean(x$set.implementationaverage, na.rm=TRUE),
                           mean(x$set.criterion8080, na.rm=TRUE) * 100)
  }
  all.covars <- c("perc.in.place.sw.disc.sys", "perc.in.place.ncr.set",
                  "perc.in.place.cr.set", "perc.in.place.ind.stud.sys",
                  "ebs.implementationaverage", "ebs.criterion8080",
                  "set.expectationsdefined", "set.expectationstaught",
                  "set.rewardsystem", "set.violationssystem", "set.monitoringevaluation",
                  "set.leadership", "set.districtsupport", "set.implementationaverage",
                  "set.criterion8080")
 
  p <- vector("numeric", 15)
  f.stat <- vector("numeric", 15)
  for (j in seq(along=all.covars)) {
    dep.var <- x1[, all.covars[j]]
    model <- lm(dep.var ~ year, data=x1)
    summary.model <- summary(model)
    p[j] <- coef(summary.model)[2, 4]
    f.stat[j] <- as.numeric(summary.model$fstatistic[1])
  }
  table.3[[7]] <- f.stat
  table.3[[8]] <- p
  table <- do.call("cbind", table.3)
  colnames(table) <- c("Characteristic", "2004", "2005", "2006", "2007", "Total", "F-statistic", "p-value")
  return(table)
}

Table1Series <- function(x) {
  print("##TABLE 1 ALL SCHOOLS##")
  print(CreateTable1(x))
  print("##TABLE 1 EBS SCHOOLS##")
  print(CreateTable1(x[which(x$i.ebs==1), ]))
  print("##TABLE 1 SET SCHOOLS##")
  print(CreateTable1(x[which(x$i.set==1), ]))
  print("##TABLE 1 EBS+SET SCHOOLS##")
  print(CreateTable1(x[which(x$i.both==1), ]))
}

TableStudentsSeries <- function(x) {
  print("##TABLE Students ALL SCHOOLS##")
  print(CreateTableStudents(x))
  print("##TABLE Students EBS SCHOOLS##")
  print(CreateTableStudents(x[which(x$i.ebs==1), ]))
  print("##TABLE Students SET SCHOOLS##")
  print(CreateTableStudents(x[which(x$i.set==1), ]))
  print("##TABLE Students EBS+SET SCHOOLS##")
  print(CreateTableStudents(x[which(x$i.both==1), ]))
}


Table2Series <- function(x) {
  print("##TABLE 2 ALL SCHOOLS##")
  print(CreateTable2(x))
  print("##TABLE 2 EBS SCHOOLS##")
  print(CreateTable2(x[which(x$i.ebs==1), ]))
  print("##TABLE 2 SET SCHOOLS##")
  print(CreateTable2(x[which(x$i.set==1), ]))
  print("##TABLE 2 EBS+SET SCHOOLS##")
  print(CreateTable2(x[which(x$i.both==1), ]))
}

Table3Series <- function(x) {
  print("##TABLE 3 ALL SCHOOLS##")
  print(CreateTable3(x))
  print("##TABLE 3 EBS SCHOOLS##")
  print(CreateTable3(x[which(x$i.ebs==1), ]))
  print("##TABLE 3 SET SCHOOLS##")
  print(CreateTable3(x[which(x$i.set==1), ]))
  print("##TABLE 3 EBS+SET SCHOOLS##")
  print(CreateTable3(x[which(x$i.both==1), ]))
}



# EVALUATING INTERNAL CONSISTENCY OF THE TWO PBIS FIDELITY MEASURES ############
# Cronbach's Alpha on SET scores to measure internal consistency
#   Closer to 1, the better
# Overall
c <- subset(c, school_educationalorgid != "3006")
cronbach(c[, c(16:27, 29:37, 39:45)])

# Cronbach's Alpha on EBS scores to measure internal consistency
#   Closer to 1, the better
# Overall
cronbach(b[, c(10:51)])

# Schoolwide Discipline System
cronbach(b[, c(10:27)])
# Nonclassroom Setting
cronbach(b[, c(28:36)])
# Classroom Setting
cronbach(b[, c(37:47)])
# Individual Student System
cronbach(b[, c(48:55)])


# CORRELATION ANALYSIS OF OUTCOME VARIABLES#####################################
id.names <- c("school_schoolyear", "school_educationalorgid")
disc.var.names <- c("iss", "oss", "expul", "major", "odr", "recid", "num.stud",
                    "stud.repeats", "grad.rate", "attend.rate")

# Look at correlations for all schools
lm.data.wide <- lm.data[, c(id.names, disc.var.names)]
lm.data.wide <- ReshapeWide(lm.data.wide)
PrintCors(lm.data.wide)


# Look at correlations for SET schools only
DepVarCorrs(lm.data$set_implementationaverage)

# Look at correlations for EBS schools only
DepVarCorrs(lm.data$ebs.implementationaverage)

# Now look at data set without outlier
#lm.data.wide.x <- lm.data.wide[which(lm.data.wide$school_educationalorgid != 3120), ]
#PrintCors(lm.data.wide.x)



# FIT DISTRIBUTIONS OF RESPONSE VARIABLES USING MLE ############################
response.names <- c("iss", "oss", "major", "odr", "recid", "num.stud",
                    "grad.rate", "attend.rate")
for (i in seq(along=response.names)) {
  print(response.names[i])
  print(FitDistr(response.names[i]))
}



# NAIVE PARAMETRIC ANOVA BETWEEN PBIS AND STATES ###############################
with(lm.data, summary(aov(ebs.implementationaverage ~ school.statelabel.y)))
with(lm.data, summary(aov(set_implementationaverage ~ school.statelabel.y)))


# NAIVE PARAMETRIC ANOVA BETWEEN PBIS AND FRPM #################################
with(lm.data, summary(aov(ebs.implementationaverage ~ frpm.prop)))
with(lm.data, summary(aov(set_implementationaverage ~ frpm.prop)))


# UNIVARIATE ANALYSIS ##########################################################
Table1Series(lm.data)
Table2Series(lm.data)
Table3Series(lm.data)


# BIVARIATE CROSS-TABULATIONS ##################################################
BivarCrossTabs <- function(x) {
  for (i in seq(along=x)) {
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



lm.data$pbis <- with(lm.data,
                     ifelse(i.ebs == 1 & i.set == 0,
                            1,
                            ifelse(i.set == 1 & i.ebs == 0,
                                   2,
                                   3)))
lm.data$i.both <- ifelse(lm.data$pbis == 3, 1, 0)


# GRAPHS #######################################################################
# Naive Plot of Average EBS subscores
ggplot(lm.data, aes(factor(year), perc.in.place.sw.disc.sys)) + geom_boxplot()
ggplot(lm.data, aes(factor(year), perc.in.place.ncr.set)) + geom_boxplot()
ggplot(lm.data, aes(factor(year), perc.in.place.cr.set)) + geom_boxplot()
ggplot(lm.data, aes(factor(year), perc.in.place.ind.stud.sys)) + geom_boxplot()

# Naive Plot of Average EBS scores
ggplot(lm.data, aes(factor(year), ebs.implementationaverage)) + geom_boxplot()

# Naive Plot of Average SET scores
ggplot(lm.data, aes(factor(year), set_implementationaverage)) + geom_boxplot()



# LINE GRAPH OF OSS FOR EACH SCHOOL ############################################
ggplot(lm.data, aes(x=year, y=oss, group=school_educationalorgid)) + geom_line()


## RESPONSE VARIABLES BY GRADE TYPE AND YEAR ###################################
ggplot(lm.data, aes(factor(high.school), iss)) +
       geom_boxplot(aes(fill = factor(high.school))) +
       facet_grid(~ year)
ggplot(lm.data, aes(factor(high.school), oss)) +
       geom_boxplot(aes(fill = factor(high.school))) +
       facet_grid(~ year)
ggplot(lm.data, aes(factor(high.school), expul)) +
       geom_boxplot(aes(fill = factor(high.school))) +
       facet_grid(~ year)
ggplot(lm.data, aes(factor(high.school), odr)) +
       geom_boxplot(aes(fill = factor(high.school))) +
       facet_grid(~ year)
ggplot(lm.data, aes(factor(high.school), recid)) +
       geom_boxplot(aes(fill = factor(high.school))) +
       facet_grid(~ year)
ggplot(lm.data, aes(factor(high.school), num.stud)) +
       geom_boxplot(aes(fill = factor(high.school))) +
       facet_grid(~ year)
ggplot(lm.data, aes(factor(high.school), grad.rate)) +
       geom_boxplot(aes(fill = factor(high.school))) +
       facet_grid(~ year)
ggplot(lm.data, aes(factor(high.school), attend.rate)) +
       geom_boxplot(aes(fill = factor(high.school))) +
       facet_grid(~ year)



################################################################################

# Average number of suspensions by locale
ggplot(lm.data, aes(factor(urbanicity), oss)) +
       geom_boxplot(aes(fill = factor(urbanicity))) +
       facet_grid(~ year)



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
NaiveGLM <- function(x) {
  summary(glm(dep.var ~ x + 

# Suspensions with EBS independent variables - Poisson Regression
summary(glm(oss ~ perc.in.place.sw.disc.sys + perc.in.place.ncr.set +
            perc.in.place.cr.set + perc.in.place.ind.stud.sys, 
            family=negative.binomial(theta=1), data=lm.data))


# Major ODRs with EBS independent variables - Poisson Regression
summary(glm(major ~ perc.in.place.sw.disc.sys + perc.in.place.ncr.set +
           perc.in.place.cr.set + perc.in.place.ind.stud.sys, 
           family=negative.binomial(theta=1), data=lm.data))



# LONGITUDINAL MIXED EFFECTS REGRESSION MODELS #################################
# GEE Major ODRs per 100 - model 1 - BASE mode, demographics only
model1 <- gee(major ~ year +
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
