# PBIS Loading File
# By Casey Tsui
# Format: R
# Last updated: 4/26/11


# Author Comment: This file loads in PBIS school discipline data from .csv files
#   End up with the following:
#   1) master = school demographic data
#   2) ebs = ebs fidelity measure survey scores
#   3) set = set fidelity measure scores
#   4) odr = all odr data
#   5) one object per state = state assessment scores


# FUNCTION DEFINITIONS #########################################################
ReadCsvData <- function(x) {
  return(read.csv(x, header=TRUE, sep=",", na.strings="NULL"))
}

PasteFileName <- function(x) {
  root1 <- "/Users/HumanImpactPartners/Documents/hias/school_discipline_hia"
  root2 <- "/data/pbis_data/"
  return(paste(root1, root2, x, sep=""))
}

ReadInPbisData <- function(x) {
  pbis <- vector("list", 2)
  file1 <- paste(x, "-SurveyData_200405to200607.csv", sep="")
  file2 <- paste(x, "-SurveyData_200405to200708.csv", sep="")
  pbis[[1]] <- ReadCsvData(file1)
  pbis[[2]] <- ReadCsvData(file2)
  merged <- unique(do.call("rbind", pbis))
  merged$School_NCESSchoolId <- sprintf("%012s", merged$School_NCESSchoolId)
  return(merged)
}

ReadInStateData <- function(state) {
  setwd(PasteFileName(state))
  demo.file <- paste(state, "demographics.csv", sep="_")
  demographics <- ReadCsvData(demo.file)
  assess.file <- paste(state, "assessments.csv", sep="_")
  assessments.by.grade <- ReadCsvData(assess.file)
  assessments <- ddply(assessments.by.grade,
                       c("school_ncesschoolid", "school_schoolyear"),
                       function(df)
                         c(reading.score = mean(df$reading.score, na.rm=TRUE),
                           math.score = mean(df$math.score, na.rm=TRUE)))
  state.data <- merge(demographics, assessments,
                      by=c("school_ncesschoolid", "school_schoolyear"),
                      all=TRUE)  # merge with assessments later
  state.data$school_ncesschoolid <- sprintf("%012s", state.data$school_ncesschoolid)
  return(state.data)
}


ReadInSwisStartYearData <- function(x) {
  setwd(PasteFileName(x))
  start.years.file <- "Gottlieb_crosstabs.csv"
  start.years <- ReadCsvData(start.years.file)
  return(start.years)
}


ReadInOdrData <- function() {
  odr <- vector("list", 4)
  odr[[1]] <- ReadCsvData("SAS-ODRData_200405to200607.csv") 
  odr[[2]] <- ReadCsvData("SAS-ODRData_200405to200708.csv")
  odr[[3]] <- ReadCsvData("SET-ODRData_200405to200607.csv")
  odr[[4]] <- ReadCsvData("SET-ODRData_200405to200708.csv")
  odr <- lapply(odr, function(x) return(x[, 1:24]))
  return(unique(do.call("rbind", odr)))
}


ReadInStudentIdData <- function() {
  s.id <- vector("list", 4)
  s.id[[1]] <- ReadCsvData("SAS-StudentId_200405to200607.csv")
  s.id[[2]] <- ReadCsvData("SAS-StudentId_200405to200708.csv")
  s.id[[3]] <- ReadCsvData("SET-StudentId_200405to200607.csv")
  s.id[[4]] <- ReadCsvData("SET-StudentId_200405to200708.csv")
  s.id <- unique(do.call("rbind", s.id))
  return(s.id[, c("ODR_ReferralId", "Student_StudentId")])
}


CreateFinalOdrData <- function() {
  setwd(PasteFileName("Gottlieb Revised Data Files 9-16-2010"))
  odr <- ReadInOdrData()

  setwd(PasteFileName("StudentIds_20101019"))
  s.id <- ReadInStudentIdData()

  merged <- unique(merge(odr, s.id))
  merged$School_NCESSchoolId <- sprintf("%012s", merged$School_NCESSchoolId)
  return(merged)
}

SaveImage <- function() {
  root1 <- "/Users/HumanImpactPartners/Documents/hias/school_discipline_hia"
  root2 <- "/data/workspaces/"
  save.image(paste(root1, root2, "pbis_uncleaned_data.RData", sep=""))
}


# READ IN FILES ################################################################
setwd(PasteFileName("Gottlieb Revised Data Files 9-16-2010"))
schools <- ReadCsvData("ECS_PrivateAltCodes.csv")
schools$School_NCESSchoolId_first <- sprintf("%012s", schools$School_NCESSchoolId_first)

# EBS Data
ebs <- ReadInPbisData("SAS")


# SET Data
set <- ReadInPbisData("SET")


# SWIS Start Years
start.years <- ReadInSwisStartYearData("swis_start_years")


# Colorado
colorado <- ReadInStateData("colorado")


# Illinois
illinois <- ReadInStateData("illinois")


# Maryland
maryland <- ReadInStateData("maryland")


# Montana
montana <- ReadInStateData("montana")


# Oregon
setwd(PasteFileName("oregon"))
inst.crosswalk <- ReadCsvData("Institution Crosswalk.csv")
names(inst.crosswalk)[2] <- "school_ncesschoolid"

oregon.attd <- read.csv("UCSF Data Request.csv", na.strings=c("NULL", "*"))
oregon.attd <- oregon.attd[oregon.attd$InstID %in% inst.crosswalk$InstID, ]
oregon.attd <- subset(oregon.attd,
                      SubGrpNm == "ASN" |
                      SubGrpNm == "BLK" |
                      SubGrpNm == "HIS" |
                      SubGrpNm == "IND" |
                      SubGrpNm == "TOT" |
                      SubGrpNm == "WHT")
oregon.attd$SubGrpNm <- factor(oregon.attd$SubGrpNm)


oregon.attd <- oregon.attd[, -5]
names(oregon.attd)[5] <- "attend.rate"
labels.or <- c("api", "afram", "latino", "aian", "total", "white")
oregon.attd.long <- transform(oregon.attd,
                              SubGrpNm = factor(SubGrpNm, labels=labels.or))

oregon.attd <- reshape(oregon.attd.long,
                       timevar=c("SubGrpNm"),
                       idvar=c("InstID",
                               "school_ncesschoolid",
                               "school_schoolyear"),
                       direction="wide")
name.attd.rate <- which(names(oregon.attd) == "attend.rate.total")
names(oregon.attd)[name.attd.rate] <- "attend.rate"

oregon.2004 <- read.csv("RCmediaSchools2004.csv", na.strings=c("NULL", "---"))
oregon.2004 <- oregon.2004[oregon.2004$InstID %in% inst.crosswalk$InstID, ]
oregon.2004$school_schoolyear <- 20042005

oregon.2005 <- read.csv("RCmediaSchools2005.csv", na.strings=c("NULL", "---"))
oregon.2005 <- oregon.2005[oregon.2005$InstID %in% inst.crosswalk$InstID, ]
oregon.2005$school_schoolyear <- 20052006

oregon.2006 <- read.csv("RCmediaSchools2006.csv", na.strings=c("NULL", "---"))
oregon.2006 <- oregon.2006[oregon.2006$InstID %in% inst.crosswalk$InstID, ]
oregon.2006$school_schoolyear <- 20062007

oregon.2007 <- read.csv("RCmediaSchools2007.csv", na.strings=c("NULL", "---"))
oregon.2007 <- oregon.2007[oregon.2007$InstID %in% inst.crosswalk$InstID, ]
oregon.2007$school_schoolyear <- 20072008

names.oregon <- c("InstID", "school_schoolyear", "AttdPct1", "DrpOutPct1",
                  "ESLStudPct")
names.oregon.2006 <- c("ReadStudExceedMeetAllGradePct1.PriorStnd",
                       "MathStudExceedMeetAllGradePct1.PriorStnd")
names.oregon.old.stnd <- c("ReadStudExceedMeetAllGradePct1",
                           "MathStudExceedMeetAllGradePct1")
# NOTE: 2006 has a variable named "ReadStudExceedMeetAllGradePct1.PriorStnd"
#   Criteria switched in 2006. Wait for call from Oregon to ask which variable
#   to use.

# BEGIN 
oregon.2004 <- oregon.2004[, c(names.oregon)]
oregon.2005 <- oregon.2005[, c(names.oregon)]
oregon.2006 <- oregon.2006[, c(names.oregon)]
oregon.2007 <- oregon.2007[, c(names.oregon)]
oregon <- rbind(oregon.2004, oregon.2005, oregon.2006, oregon.2007)
oregon <- merge(oregon, inst.crosswalk, all.x=TRUE)
oregon <- transform(oregon,
                    school.statelabel = as.factor("OR"),
                    attend.rate = as.numeric(gsub("%", "", AttdPct1)),
                    dropout.rate = as.numeric(gsub("%", "", DrpOutPct1)),
                    lep.perc = as.numeric(gsub("%", "", ESLStudPct)))
oregon <- subset(oregon, select=-c(AttdPct1:ESLStudPct))
oregon <- merge(oregon, oregon.attd, all=TRUE)
oregon$InstID <- NULL
oregon$school_schoolyear <- factor(oregon$school_schoolyear,
                                   labels=c("200405", "200506", "200607",
                                            "200708"))
oregon$school_ncesschoolid <- sprintf("%012s", oregon$school_ncesschoolid)
rm(inst.crosswalk, oregon.2004, oregon.2005, oregon.2006, oregon.2007,
   oregon.attd, oregon.attd.long, labels.or, name.attd.rate, names.oregon,
   names.oregon.2006, names.oregon.old.stnd)


# USE THIS DATA WHEN READY TO LOOK AT ASSESSMENTS ##############################
#oregon.2004 <- oregon.2004[, c(names.oregon, names.oregon.old.stnd)] 
#oregon.2004 <- transform(oregon.2004,
#                         reading.score.prior.stnd = ReadStudExceedMeetAllGradePct1,
#                         math.score.prior.stnd = MathStudExceedMeetAllGradePct1,
#                         reading.score.curr.stnd = NA,
#                         math.score.curr.stnd = NA)
#oregon.2004 <- transform(oregon.2004,
#                         ReadStudExceedMeetAllGradePct1 = NULL,
#                         MathStudExceedMeetAllGradePct1 = NULL)
#oregon.2005 <- oregon.2005[, c(names.oregon, names.oregon.old.stnd)]
#oregon.2005 <- transform(oregon.2005,
#                         reading.score.prior.stnd = ReadStudExceedMeetAllGradePct1,
#                         math.score.prior.stnd = MathStudExceedMeetAllGradePct1,
#                         reading.score.curr.stnd = NA,
#                         math.score.curr.stnd = NA)
#oregon.2005 <- transform(oregon.2005,
#                         ReadStudExceedMeetAllGradePct1 = NULL,
#                         MathStudExceedMeetAllGradePct1 = NULL)
#oregon.2006 <- oregon.2006[, c(names.oregon, names.oregon.2006,
#                               names.oregon.old.stnd)]
#oregon.2006 <- transform(oregon.2006,
#                         reading.score.prior.stnd = ReadStudExceedMeetAllGradePct1.PriorStnd,
#                         math.score.prior.stnd = MathStudExceedMeetAllGradePct1.PriorStnd,
#                         reading.score.curr.stnd = ReadStudExceedMeetAllGradePct1,
#                         math.score.curr.stnd = MathStudExceedMeetAllGradePct1)
#oregon.2006 <- transform(oregon.2006,
#                         ReadStudExceedMeetAllGradePct1.PriorStnd = NULL,
#                         MathStudExceedMeetAllGradePct1.PriorStnd = NULL,
#                         ReadStudExceedMeetAllGradePct1 = NULL,
#                         MathStudExceedMeetAllGradePct1 = NULL)
#oregon.2007 <- oregon.2007[, c(names.oregon, names.oregon.old.stnd)]
#oregon.2007 <- transform(oregon.2007,
#                         reading.score.prior.stnd = NA,
#                         math.score.prior.stnd = NA,
#                         reading.score.curr.stnd = ReadStudExceedMeetAllGradePct1,
#                         math.score.curr.stnd = MathStudExceedMeetAllGradePct1)
#oregon.2007 <- transform(oregon.2007,
#                         ReadStudExceedMeetAllGradePct1 = NULL,
#                         MathStudExceedMeetAllGradePct1 = NULL)
#
#oregon <- rbind(oregon.2004, oregon.2005, oregon.2006, oregon.2007)
#oregon <- merge(oregon, inst.crosswalk, all.x=TRUE)
#oregon <- transform(oregon,
#                    school_schoolyear = as.numeric(levels(school_schoolyear))[school_schoolyear],
#                    school.statelabel = as.factor("OR"),
#                    attend.rate = as.numeric(gsub("%", "", AttdPct1)),
#                    grad.rate = as.numeric(gsub("%", "", GradDgPct)),
#                    dropout.rate = as.numeric(gsub("%", "", DrpOutPct1)),
#                    lep.perc = as.numeric(gsub("%", "", ESLStudPct)),
#                    reading.score = NA,
#                    math.score = NA,
#                    reading.score.prior.stnd = as.numeric(gsub("[%>]", "", reading.score.prior.stnd)),
#                    math.score.prior.stnd = as.numeric(gsub("[%>]", "", math.score.prior.stnd)),
#                    reading.score.curr.stnd = as.numeric(gsub("[%>]", "", reading.score.curr.stnd)),
#                    math.score.curr.stnd = as.numeric(gsub("[%>]", "", math.score.curr.stnd)))
#oregon <- subset(oregon, select=-c(AttdPct1:ESLStudPct))
#oregon <- merge(oregon, oregon.attd, all=TRUE)
#oregon$InstID <- NULL
#oregon$school_schoolyear <- factor(oregon$school_schoolyear,
#                                   labels=c("200405", "200506", "200607",
#                                            "200708"))
#rm(oregon.2004, oregon.2005, oregon.2006, oregon.2007, oregon.attd)
# END ASSESSMENTS-SPECIFIC SECTION ############################################# 


# Rbind all states together
states.list <- list(colorado, illinois, maryland, montana, oregon)
states <- Reduce(function(x, y) merge(x, y, all=TRUE),
                 states.list, accumulate=FALSE)

states <- subset(states, !is.na(school_schoolyear))
rm(colorado, illinois, maryland, montana, oregon)

# Master data including all schools + EBS and SET scores
master <- merge(ebs, set, all=TRUE)
master <- unique(subset(master, select = School_SchoolYear:NCES_WHALF))
ebs <- subset(ebs, select = c(School_SchoolYear:School_NCESSchoolId,
                              SASurvey_EBSSurveyId:SASurvey_IndBehaviorMonitoredP))
set <- subset(set, select = c(School_SchoolYear:School_NCESSchoolId,
                              SET_Version:SET_Criterion8080))


# ODR Data #####################################################################
odr <- CreateFinalOdrData()

# Save image of workspace
SaveImage()





#filenames <- list.files(path="/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/data/pbis_data/Gottlieb Revised Data Files 9-16-2010/", full.names=TRUE)
#library(plyr)
#import.list <- llply(filenames, read.csv, header=TRUE, sep=",",
#                     na.string="NULL")
#
#data <- Reduce(function(x, y)
#                 merge(x, y, all=TRUE,
#                       by=c("School_EducationalOrgId", "School_SchoolYear")),
#                 import.list, accumulate=FALSE)


