#sink(file="/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/results/pbis_func_output.txt") #send output to file


# PBIS Function File
# By Casey Tsui
# Format: R
# Last updated: 3/9/11


# Author Comment: This file contains the functions that are called in
# ~/pbis_do.txt
# Contains all of the functions needed to perform the actual analysis.
#   source()'ing this file should have no side effects other than loading up the
#   function definitions. This means that you can modify this file and reload it
#   without having to go back an repeat steps 1 & 2 which can take a long time
#   to run for large data sets.


# Create staff response rate for ebs
AggregateBySchool <- function(x, y, ...) {
  # Produces a dataset of x concatenated over the three year period and merges
  # it with the school data in long format.
  # Args:
  #   x: data frame where the aggregated variable-to-be lives
  #   y: new variable name for aggregated data in x
  m <- x
  year1 <- rep(200405, length(unique(m$school_educationalorgid)))
  year2 <- rep(200506, length(unique(m$school_educationalorgid)))
  year3 <- rep(200607, length(unique(m$school_educationalorgid)))
  #year4 <- rep(200708, length(unique(m$school_educationalorgid)))

  x1 <- cbind(year1,
              table(m$school_educationalorgid[m$school_schoolyear == 200405]))
  x2 <- cbind(year2,
              table(m$school_educationalorgid[m$school_schoolyear == 200506]))
  x3 <- cbind(year3,
              table(m$school_educationalorgid[m$school_schoolyear == 200607]))
  #x4 <- cbind(year4,
  #            table(m$school_educationalorgid[m$school_schoolyear == 200708]))
  xbind <- rbind(x1, x2, x3)
  #rm(year1, year2, year3, x1, x2, x3)
  colnames(xbind) <- c("year1", "y")
  y <- merge(a, xbind,
             by.x=c("school_educationalorgid", "school_schoolyear"),
             by.y=c("row.names", "year1"), all=TRUE) 
  # ebs.by.school has an ebs survey count variable per school per year
  #rm(xbind)
}






PropTable <- function(x, y) {
  # Outputs a proportion table for x and y
  # Args:
  #   x: data
  rx <- table(x, y)
  prop.table(rx, margin=2) * 100
}


RunModel1 <- function(x) {
  # Runs GEE Model 1
  #
  # Args:
  #   x: Data frame variable
  x1 <- x
  return(gee(x1 ~ school_schoolyear +
                  perc.in.place.sw.disc.sys +
                  perc.in.place.ncr.set +
                  perc.in.place.cr.set +
                  perc.in.place.ind.stud.sys +
                  male.prop +
                  grade.type,
                  id=school_educationalorgid,
                  data=lm.data, family=gaussian, corstr="AR-M", Mv=1))
}




#sink()
