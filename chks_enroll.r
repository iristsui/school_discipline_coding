enroll <- read.csv("/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/data/chks/Enroll05_07_Kin_OSec.csv",
                 header=TRUE,
                 sep=",")

names(enroll) <- c("cds.code", "enroll2005", "enroll2006", "enroll2007",
                   "enroll2008")

enroll$cds.code <- with(enroll, sprintf("%014s", cds.code))

enroll <- reshape(enroll, idvar="cds.code",
                  direction="long",
                  varying=list(2:(length(enroll))),
                  v.names="enroll",
                  times=c("2005", "2006", "2007", "2008"),
                  timevar="schlyear")

enroll$schlyear <- ifelse(enroll$schlyear == "2006",
                          "2005",
                          ifelse(enroll$schlyear == "2008",
                                 "2007",
                                 enroll$schlyear))

enroll <- ddply(enroll, .(cds.code, schlyear),
                function(df)
                  c(enroll = mean(df$enroll, na.rm=TRUE)))

enroll$schlyear <- factor(enroll$schlyear,
                          levels=c("2005",  "2007"),
                          labels=c("2005-2007", "2007-2009"))


