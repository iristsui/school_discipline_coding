# CADRE Survey Data Analysis
# By Casey Tsui
# Format: R
# Last updated: 5/27/11


# Author Comment:
# This file loads, cleans and analyzes the CADRE survey data, imported from a 
#   source file downloaded from the Surveymonkey website.


library(Hmisc)

# FUNCTION CALLS ############################################################### 
Question13ByVar <- function(x) {
  rows <- nrow(x)
  num.skips <- ifelse(!is.na(table(x$skipped_q_13)[2]),
                      table(x$skipped_q_13)[2],
                      0)
  denom <- rows - num.skips
  q13 <- vector("list", 6)
  for (i in 1:6) {
    q13[[i]] <- table(x[, i+38]) / denom * 100
  }
  tab <- do.call("rbind", q13)
  row.names(tab) <- names(x)[39:44]
  colnames(tab) <- c("Response Percent")
  return(tab)
} 

Question14ByVar <- function(x) {
  rows <- nrow(x)
  num.skips <- ifelse(!is.na(table(x$skipped_q_14)[2]),
                      table(x$skipped_q_14)[2],
                      0)
  denom <- rows - num.skips
  q14 <- vector("list", 9)
  for (i in 1:9) {
    q14[[i]] <- sum(table(x[, i+45]), na.rm=TRUE) / denom * 100
  }
  tab <- do.call("rbind", q14)
  row.names(tab) <- names(x)[46:54]
  colnames(tab) <- c("Response Percent")
  return(tab)
} 

Question15ByVar <- function(x) {
  rows <- nrow(x)
  num.skips <- ifelse(!is.na(table(x$skipped_q_15)[2]),
                      table(x$skipped_q_15)[2],
                      0)
  denom <- rows - num.skips
  q15 <- vector("list", 6)
  for (i in 1:6) {
    q15[[i]] <- sum(table(x[, i+54]), na.rm=TRUE) / denom * 100
  }
  tab <- do.call("rbind", q15)
  row.names(tab) <- names(x)[55:60]
  colnames(tab) <- c("Response Percent")
  return(tab)
} 

CreateTablesXVars <- function(df) {
  for (i in 61:71) {
    total <- cbind(with(df, prop.table(table(df[, i])) * 100))
    colnames(total) <- "Total"
    crosstab.gender <- cbind(with(df,
                                  prop.table(table(df[, i],
                                                   df[, gender]), 2) * 100))
    crosstab.race <- cbind(with(df,
                                prop.table(table(df[, i],
                                                 df[, race]), 2) * 100))
    tab <- cbind(total, crosstab.gender, cross.tab.race)
    print(tab)
  }
}



# BODY
cadre <- read.csv("/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/data/cadre/survey/Results/Sheet_1_edited.csv",
                  header=TRUE, sep=",", na.strings="")
cadre <- upData(cadre, lowernames=TRUE)


# Subset
#cadre <- subset(cadre, select=-c(emailaddress:customdata))


cadre$race <- with(cadre,
                   ifelse(!is.na(african_american) & !is.na(latino),
                          "Multi",
                          ifelse(!is.na(african_american) & is.na(latino),
                                 "African-American/Black",
                                 ifelse(is.na(african_american) & !is.na(latino),
                                        "Latino/Hispanic",
                                        "Asian/Pacific Islander"))))
                                                


cadre <- transform(cadre,
                   disc_more_less_misbehav = factor(disc_more_less_misbehav,
                                                    levels=c("Less", "No effect",
                                                             "More")),
                   strict_involve_engagement = factor(strict_involve_engagement,
                                               levels=c("Less involved",
                                                        "Neither",
                                                        "More involved")))

for (i in 61:71) {
  cadre[, i] <- factor(cadre[, i],
                       levels=c("Strongly Agree", "Agree", "Neutral",
                                "Disagree", "Strongly Disagree"))
}

# Mean Number of years attending LAUSD
mean(cadre$years_attended_lausd, na.rm=TRUE)

# Gender - SD More/Less Misbehavior
with(cadre, table(gender, disc_more_less_misbehav))
prop.table(with(cadre, table(gender, disc_more_less_misbehav)), 1) * 100
# Race - SD More/Less Misbehavior
with(cadre, table(race, disc_more_less_misbehav))
prop.table(with(cadre, table(race, disc_more_less_misbehav)), 1) * 100


# Gender - Ever Suspended
with(cadre, table(gender, ever_suspended))
summary(with(cadre, table(gender, ever_suspended)))
prop.table(with(cadre, table(gender, ever_suspended)), 1) * 100
# Race - Ever Suspended
with(cadre, table(race, ever_suspended))
prop.table(with(cadre, table(race, ever_suspended)), 1) * 100

# Gender - Strict Discipline Affect Involvement
with(cadre, table(gender, strict_involve_engagement))
summary(with(cadre, table(gender, strict_involve_engagement)))
prop.table(with(cadre, table(gender, strict_involve_engagement)), 1) * 100
# Race - Strict Discipline Affect Involvement
with(cadre, table(race, strict_involve_engagement))
prop.table(with(cadre, table(race, strict_involve_engagement)), 1) * 100



cadre$skipped_q_13 <- ifelse(rowSums(is.na(cadre[, 39:44]) == TRUE,
                                     na.rm=TRUE) == 6,
                             1,
                             0)
cadre$skipped_q_14 <- ifelse(rowSums(is.na(cadre[, 46:54]) == TRUE,
                                     na.rm=TRUE) == 9,
                             1,
                             0)
cadre$skipped_q_15 <- ifelse(rowSums(is.na(cadre[, 55:60]) == TRUE,
                                     na.rm=TRUE) == 6,
                             1,
                             0)

cadre.list <- vector("list", 7)
cadre.list[[1]] <- cadre
cadre.list[[2]] <- subset(cadre, gender == "Female")
cadre.list[[3]] <- subset(cadre, gender == "Male")
cadre.list[[4]] <- subset(cadre, race == "African-American/Black")
cadre.list[[5]] <- subset(cadre, race == "Asian/Pacific Islander")
cadre.list[[6]] <- subset(cadre, race == "Latino/Hispanic")
cadre.list[[7]] <- subset(cadre, race == "Multi")
cadre.list.names <- c("Full",
                      rownames(table(cadre$gender)),
                      rownames(table(cadre$race)))
names(cadre.list) <- cadre.list.names



q13.table <- do.call("cbind", lapply(cadre.list, Question13ByVar))
colnames(q13.table) <- cadre.list.names
q14.table <- do.call("cbind", lapply(cadre.list, Question14ByVar))
colnames(q14.table) <- cadre.list.names
q15.table <- do.call("cbind", lapply(cadre.list, Question15ByVar))
colnames(q15.table) <- cadre.list.names
q.tables <- rbind(q13.table, q14.table, q15.table)


# Thumbs up/down Questions #####################################################
CreateTablesXVars(cadre, "gender")
CreateTablesXVars(cadre, "race")


################################################################################
#Likert <- function(x) {
#  factor(x,
#         levels=c("Strongly Agree", "Agree", "Neutral",
#                  "Disagree", "Strongly Disagree"),
#         ordered=TRUE)
#}
#
#
#
#likert <- list(c(cadre[, substr(names(cadre), 1, 2) == "x_"]))
