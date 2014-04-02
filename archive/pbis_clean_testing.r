#sink(file="/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/results/pbis_cleaning_output.txt")  # send output to file

# PBIS Cleaning TEST File
# By Casey Tsui
# Format: R
# Last updated: 10/4/10


# Author Comment: This file cleans the variables of interest.
  # This is where all the ugly stuff lives.
  # Takes care of missing values, merging data frames, handling outliers. 
  # THIS FILE ATTEMPTS TO LOOP ALL THE EBS AND SET DATA to streamline analysis

for (i in c(1:2)) {
  # i=1 chooses EBS
  # i=2 chooses SET
  for (j in c(1:2) {
    # j=1 chooses 3-year
    # j=2 chooses 4-year
    if (i == 1) {
      if (j == 1) {
        schools.and.surveys <- ebs.data[[1]]
        odrs <- ebs.data[[2]]
      } else {
        schools.and.surveys <- ebs.data[[3]]
        odrs <- ebs.data[[4]]
      }
    } else {
      if (j == 1) {
        schools.and.surveys <- set.data[[1]]
        odrs <- set.data[[2]]
      } else {
        schools.and.surveys <- set.data[[3]]
        odrs <- set.data[[4]]
      }
    }

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





setwd("/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/coding")
source("pbis_load.r")






  }
}










###########################################
###########################################
###########################################


