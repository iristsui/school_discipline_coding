CreateTable2 <-
function(x1) {
  years <- c("2004", "2005", "2006", "2007")
  table.2 <- vector("list", 5)
  table.2[[1]] <- c("Number of Total ODRs",  # Names table rows
                    "Number of In-school Suspensions",
                    "Number of Out-of-school Suspensions",
                    "Number of Expulsions",
                    "Number of Major ODRs",
                    "Number of Total ODRs",
                    "Proportion of Total ODRs that are ISS",
                    "Proportion of Total ODRs that are OSS",
                    "Proportion of Total ODRs that are Expulsions",
                    "Proportion of Total ODRs that are Major ODRs",
                    "Average Number of ODRs Among Students With At Least 1 Incident",
                    "Average Percent of Students with ODR")
                   #"Average Number of Days Suspended (ISS)",
                   #"Average Number of Days Suspended (OSS)",
                   #"Average Number of Days Suspended (ISS & OSS)",
                   #"Average Number of Days Expelled",
                   #"Average Graduation Rate",
                   #"Average Reading Score",
                   #"Average Math Score")

  for (i in 1:length(years)) {
    # Loops through 2004 to the last year and makes table.2."year" object
    #   for each year
    x <- subset(x1, year == i)
    total.odr <- sum(x$odr, na.rm=TRUE)
    table.2[[i+1]] <- list(sum(x$odr,            na.rm=TRUE),
                           sum(x$iss,            na.rm=TRUE),
                           sum(x$oss,            na.rm=TRUE),
                           sum(x$expul,          na.rm=TRUE),
                           sum(x$major,          na.rm=TRUE),
                           sum(x$odr,            na.rm=TRUE),
                           sum(x$iss, na.rm=TRUE) / total.odr * 100,
                           sum(x$oss, na.rm=TRUE) / total.odr * 100,
                           sum(x$expul, na.rm=TRUE) / total.odr * 100,
                           sum(x$major, na.rm=TRUE) / total.odr * 100,
                           mean(x$recid,         na.rm=TRUE),
                           mean(x$perc.stud.odr, na.rm=TRUE))
                           #mean(x$odr_dayssuspended[b$odr_admindecisionid==7], na.rm=TRUE),
                           #mean(x$odr_dayssuspended[b$odr_admindecisionid==8], na.rm=TRUE),
                           #mean(x$odr_dayssuspended, na.rm=TRUE),
                           #mean(x$odr_daysexpelled, na.rm=TRUE))
                           #mean(x$grad.rate,     na.rm=TRUE),
                           #mean(x$reading.score, na.rm=TRUE),
                           #mean(x$math.score,    na.rm=TRUE))
  }
  do.call("cbind", table.2)
}

