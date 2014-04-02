CreateTable3 <-
function(x1) {
  # Args:
  #   x: Data frame
  years <- c("2004", "2005", "2006", "2007")
  table.3 <- vector("list", 5)
  table.3[[1]] <- c("EBS Schoolwide Discipline System",  # Names table rows
                    "EBS Nonclassroom Setting",
                    "EBS Classroom Setting",
                    "EBS Individual Student System",
                    "EBS Implementation Average",
                    "Percent of Schools Meeting EBS Criterion 8080",
                    "SET Expectations Defined",
                    "SET Expectations Taught",
                    "SET Reward System",
                    "SET Violation System",
                    "SET Monitoring and Evaluation",
                    "SET Leadership",
                    "SET District Support",
                    "SET Implementation Average",
                    "Percent of Schools Meeting SET Criterion 8080")

  for (i in 1:length(years)) {
    # Loops through 2004 to the last year and makes table.3."year" object
    #   for each year
    x <- subset(x1, year == i)
    school.denom <- with(x, length(unique(school_educationalorgid[year==i])))
    table.3[[i+1]] <- list(mean(x$perc.in.place.sw.disc.sys, na.rm=TRUE),
                           mean(x$perc.in.place.ncr.set, na.rm=TRUE),
                           mean(x$perc.in.place.cr.set, na.rm=TRUE),
                           mean(x$perc.in.place.ind.stud.sys, na.rm=TRUE),
                           mean(x$ebs.implementationaverage, na.rm=TRUE),
                           sum(x$ebs.criterion8080, na.rm=TRUE) / school.denom,
                           mean(x$set_expectationsdefined, na.rm=TRUE),
                           mean(x$set_expectationstaught, na.rm=TRUE),
                           mean(x$set_rewardsystem, na.rm=TRUE),
                           mean(x$set_violationssystem, na.rm=TRUE),
                           mean(x$set_monitoringevaluation, na.rm=TRUE),
                           mean(x$set_leadership, na.rm=TRUE),
                           mean(x$set_districtsupport, na.rm=TRUE),
                           mean(x$set_implementationaverage, na.rm=TRUE),
                           sum(x$set_criterion8080, na.rm=TRUE) / school.denom)
  }
  do.call("cbind", table.3)
}

