# CSCS Load File
# By Casey Tsui
# Format: R
# Last updated: 12/22/11


library(plyr)

CscsPercAgree <- function(x) {
  x <- x[!is.na(x)]
  num = length(x[which(x == 1 | x == 2)])
  denom = length(x)
  perc = num / denom * 100
  return(perc)
}

CscsModSevereProb <- function(x) {
  x <- x[!is.na(x)]
  num = length(x[which(x == 3 | x == 4)])
  denom = length(x)
  perc = num / denom * 100
  return(perc)
}


AggBySchoolYear <- function(old, min, max, gap, func, sep="_") {
  first <- paste("q", varrange, sep="")
  new <- ddply(old, .(cds.code, district, schlyear, grade),
               colwise(func, c(usenames)))
  return(new)
}

################################################################################

cscs <- read.csv("/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/data/chks/cscs_hip.csv",
                 header=TRUE,
                 sep=",")

cscs <- subset(cscs,
               (sch_type == "MIDDLE" | sch_type == "HIGH SCHOOL") &
               !is.na(schlyear))

cscs <- transform(cscs,
                  tool = "cscs",
                  cntydist = sprintf("%07s", cntydist),
                  school = sprintf("%07s", school))


cscs$cds.code <- with(cscs, paste(cntydist, school, sep=""))
cscs$district <- factor(cscs$district)
cscs$schlyear <- factor(cscs$schlyear, levels=c(2005.2006, 2007.2008),
                        labels=c("2005-2007", "2007-2009"))

#cscs.agg <- ddply(cscs,
#                  c("cds.code", "schlyear"),
#                  function(df) 
#                    c(num.surveys = length(df$cds_code),
#                      num.7 = length(!is.na(df$hpdcs_7[which(df$hpdcs_7 == 1 | df$hpdcs_7 == 2)])),
#                      denom.7 = length(df$hpdcs_7[which(!is.na(df$hpdcs_7))]),
#                      perc.7 = length(!is.na(df$hpdcs_7[which(df$hpdcs_7 == 1 | df$hpdcs_7 == 2)])) / length(df$hpdcs_7[which(!is.na(df$hpdcs_7))])))



usenames.q1 <- paste("q", c(29:30, 33:38, 53:56), sep="")
usenames.q2 <- paste("q", c("57a", "57d", 58:63, "64x"), sep="")
usenames.hpdcs <- paste("hpdcs", c(5:7, 10, 18:20), sep="_")
#usenames <- c(usenames.q1, usenames.hpdcs)
cscs.agg.list <- list()
cscs.agg.list$q1 <- ddply(cscs, .(cds.code, district, schlyear), colwise(CscsPercAgree, c(usenames.q1)))
cscs.agg.list$q2 <- ddply(cscs, .(cds.code, district, schlyear), colwise(CscsModSevereProb, c(usenames.q2)))
cscs.agg.list$hpdcs <- ddply(cscs, .(cds.code, district, schlyear), colwise(CscsPercAgree, c(usenames.hpdcs)))

cscs.agg <- Reduce(function(x, y)
                   merge(x, y, all=TRUE,
                         by=c("cds.code", "district", "schlyear")),
                         cscs.agg.list, accumulate=FALSE)

# Descriptive Tables ###########################################################
table.cscs <- ddply(cscs.agg, .(district, schlyear),
                    colwise(function(df) return(mean(df, na.rm=TRUE)), c(names(cscs.agg[c(4:length(cscs.agg))]))))

table.cscs$district <- as.numeric(table.cscs$district)
table.cscs$schlyear <- as.numeric(table.cscs$schlyear)
t(table.cscs)


