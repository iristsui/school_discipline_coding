# CHKS Load File
# By Casey Tsui
# Format: R
# Last updated: 11/8/11


############
#bra t(stats(x))


library(Hmisc)
library(corpcor)
library(Matrix)
library(corrplot)
library(randomForest)

ChksPercAgree <- function(x) {
  x <- x[!is.na(x)]
  num = length(x[which(x == 4 | x == 5)])
  denom = length(x)
  perc = num / denom * 100
  return(perc)
}

ChksFirstTwoAnswers <- function(x) {
  x <- x[!is.na(x)]
  num = length(x[which(x == 1 | x == 2)])
  denom = length(x)
  perc = num / denom * 100
  return(perc)
}

ChksNotFirstAnswer <- function(x) {
  x <- x[!is.na(x)]
  num = length(x[which(x != 1)])
  denom = length(x)
  perc = num / denom * 100
  return(perc)
}

ChksPrettyMuchTrue <- function(x) {
  x <- x[!is.na(x)]
  num = length(x[which(x == 3 | x == 4)])
  denom = length(x)
  perc = num / denom * 100
  return(perc)
}


#ChksHowOldFirstTime <- function(x) {
#  x <- x[!is.na(x)]
#  num = length(x[which(x != 10)])
#  denom = length(x)
#  perc = num / denom * 100
#  return(perc)
#}
 
ChksHowManyStudents <- function(x) {
  x <- x[!is.na(x)]
  num = length(x[which(x >= 6)])
  denom = length(x)
  perc = num / denom * 100
  return(perc)
}

ChksDrugEffects <- function(x) {
  x <- x[!is.na(x)]
  num = length(x[which(x == 1)])
  denom = length(x)
  perc = num / denom * 100
  return(perc)
}

ChksBfGfViolence <- function(x) {
  x <- x[!is.na(x) & x != 1]
  num = length(x[which(x == 3)])
  denom = length(x)
  perc = num / denom * 100
  return(perc)
}



AggBySchoolYearGrade <- function(old, min, max, gap, func, sep="_") {
  varrange <- min:max
  first <- paste("a", varrange, sep="")
  second <- paste("a", varrange - gap, sep="")
  usenames <- paste(first, second, sep=sep)
  new <- ddply(old, .(cds.code, district, schlyear, grade),
               colwise(func, c(usenames)))
  return(new)
}




cor.prob <- function(X, dfr = nrow(X) - 2) {
	 R <- cor(X)
	 above <- row(R) < col(R)
	 r2 <- R[above]^2
	 Fstat <- r2 * dfr / (1 - r2)
	 R[above] <- 1 - pf(Fstat, 1, dfr)
	 R
}


cor.pval2 <- function(x, alternative="two-sided") {
  corMat <- cor(x, use=if (any(is.na(x))) "pairwise.complete.obs" else "all")
  df <- crossprod(!is.na(x)) - 2
  STATISTIC <- sqrt(df) * corMat / sqrt(1 - corMat^2)
  p <- pt(STATISTIC, df)
  p <- if (alternative == "less") {
         p
       } else if (alternative == "greater") {
         1 - p
       } else 2 * pmin(p, 1 - p)
  p
}

################################################################################

chks <- read.csv("/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/data/chks/chks_hip.csv",
                 header=TRUE,
                 sep=",")

chks <- upData(chks, lowernames=TRUE)

chks <- subset(chks,
               grade <= 11,
               select=c(schlclas:a126a108, a90a:a90m, dist:sexused, charter))

chks$tool <- "chks"

chks$cds.code <- with(chks, sprintf("%014s", cds_code))

chks <- subset(chks, schlyear > 2005)

##################
#chks.ld7 <- subset(chks, cds.code == "19647336058143" |
#                     cds.code == "19647336057962" |
#                     cds.code == "19647336061444" |
#                     cds.code == "19647331933118" |
#                     cds.code == "19647336058176" |
#                     cds.code == "19647331934454" |
#                     cds.code == "19647330112854" |
#                     cds.code == "19647331933001" |
#                     cds.code == "19647331935519" |
#                     cds.code == "19647336061527" |
#                     cds.code == "19647330114850")

chks$schlyear <- with(chks, ifelse(schlyear == 2006.2007,
                                   2005.2006,
                                   ifelse(schlyear == 2008.2009,
                                          2007.2008,
                                          schlyear)))
chks$schlyear <- factor(chks$schlyear, levels=c(2005.2006, 2007.2008),
                        labels=c("2005-2007", "2007-2009"))

chks$district <- with(chks, ifelse(dist == 61259,
                                   "Oakland Unified",
                                   ifelse(dist == 64733,
                                          "Los Angeles Unified",
                                          "Salinas Union High")))

# Recode all negatives to NA
chks[, c(7:143)] <- lapply(chks[, c(7:143)], function(x) ifelse(x < 0, NA, x))


chks.agg.list <- list()
chks.agg.list$cohesion <- AggBySchoolYearGrade(chks, 11, 15, 1, ChksPercAgree)
chks.agg.list$inschool.visible <- AggBySchoolYearGrade(chks, 16, 21, 1, ChksPrettyMuchTrue)
chks.agg.list$outschool.visible <- AggBySchoolYearGrade(chks, 25, 33, 1, ChksPrettyMuchTrue)
#chks.agg.list$age.first.drugs <- AggBySchoolYearGrade(chks, 56, 60, 11, ChksHowOldFirstTime)
chks.agg.list$past.30.days.drugs.1 <- AggBySchoolYearGrade(chks, 61, 66, 11, ChksNotFirstAnswer)
chks.agg.list$past.30.days.drugs.2 <- ddply(chks, .(cds.code, district, schlyear, grade),
                                            colwise(ChksNotFirstAnswer, paste("a", c(67:69, 71), sep="")))
chks.agg.list$past.30.days.drugs.3 <- AggBySchoolYearGrade(chks, 70, 70, 14, ChksNotFirstAnswer)
chks.agg.list$past.30.days.drugs.4 <- AggBySchoolYearGrade(chks, 72, 75, 15, ChksNotFirstAnswer)
#chks.agg.list$past.30.days.drugs.5 <- AggBySchoolYearGrade(chks, 76, 76, 13, ChksNotFirstAnswer)
chks.agg.list$risk.perception <- AggBySchoolYearGrade(chks, 78, 83, 14, ChksFirstTwoAnswers)
chks.agg.list$access.difficulty <- AggBySchoolYearGrade(chks, 84, 86, 14, ChksFirstTwoAnswers)
chks.agg.list$social.norms <- AggBySchoolYearGrade(chks, 87, 88, 14, ChksHowManyStudents)
chks.agg.list$drug.effects <- ddply(chks, .(cds.code, district, schlyear, grade),
                               colwise(ChksDrugEffects, paste("a90", letters[1:13], sep="")))
chks.agg.list$violence <- AggBySchoolYearGrade(chks, 100, 117, 18, ChksNotFirstAnswer)
chks.agg.list$safe.at.school <- AggBySchoolYearGrade(chks, 120, 120, 19, ChksNotFirstAnswer, "")
chks.agg.list$in.gangs <- AggBySchoolYearGrade(chks, 121, 121, 18, ChksNotFirstAnswer, "")
chks.agg.list$bfgfviolence <- AggBySchoolYearGrade(chks, 122, 122, 18, ChksBfGfViolence, "")
chks.agg.list$sad.hopeless <- AggBySchoolYearGrade(chks, 123, 123, 18, ChksNotFirstAnswer, "")
chks.agg.list$truancy <- AggBySchoolYearGrade(chks, 125, 125, 18, ChksNotFirstAnswer, "")


chks.agg <- Reduce(function(x, y)
                   merge(x, y, all=TRUE,
                         by=c("cds.code", "district", "schlyear", "grade")),
                         chks.agg.list, accumulate=FALSE)

#ggplot(chks.agg,
#       aes(x=factor(schlyear), y=a11_a10, group=factor(cds.code))) +
#       geom_line(aes(colour=factor(cds.code))) +
#       opts(legend.position="none")




wested <- merge(cscs.agg, chks.agg,
                by=c("cds.code", "district", "schlyear"),
                all=TRUE)

wested <- wested[which(!is.na(wested$grade)), ]

#wested <- merge(wested, enroll, by=c("cds.code", "schlyear"), all.x=TRUE)

#table.data <- ddply(wested, .(schlyear, district, grade),
#      colwise(function(df) return(mean(df, na.rm=TRUE)), c(names(wested.ousd[c(4:22, 24:length(wested.ousd))]))))
#table.data$district <- as.numeric(table.data$district)
#table.data <- with(table.data, table.data[order(grade, district), ])
#t(table.data)



################################################################################

# Average out the school years and grades for correlation analysis

CollapseByCdsCode <- function(x) {
  ddply(x, .(cds.code),
        colwise(function(df) mean(df, na.rm=TRUE),
                                c(names(wested[c(4:30, 32:length(wested))]))))
}

RecodeNan <- function(x) {
  ddply(x, .(cds.code),
        colwise(function(df) if (is.nan(df)) NA,
                                c(names(wested[c(4:30, 32:length(wested))]))))
}



CorPValuesColumn <- function(df, x) {
  p.x <- df
  var <- x
  index <- which(colnames(p.x) == var)
  p.x.1 <- p.x[, index]
  p.x.1 <- cbind(p.x.1[1:index])
  p.x.2 <- p.x[index, ]
  p.x.2 <- cbind(p.x.2[(index + 1):length(p.x.2)])
  p.x.p <- rbind(p.x.1, p.x.2)
  return(p.x.p)
}

# LOS ANGELES
wested.lausd <- subset(wested, district == "Los Angeles Unified")
wested.lausd.x <- CollapseByCdsCode(wested.lausd)
# Remove NaNs
wested.lausd.x[, 2:length(wested.lausd.x)] <- lapply(wested.lausd.x[, 2:length(wested.lausd.x)], function(x) ifelse(is.finite(x), x, NA))

# OAKLAND
wested.ousd <- subset(wested, district == "Oakland Unified")
wested.ousd.x <- CollapseByCdsCode(wested.ousd)

# SALINAS
wested.suhsd <- subset(wested, district == "Salinas Union High")
wested.suhsd.x <- CollapseByCdsCode(wested.suhsd)


CreateDistrictCorPlots <- function(x) {
  x <- cor(x[, 2:length(x)],
            use="pairwise.complete.obs",
            method="pearson")

  #x <- cor(wested[, 3:length(wested)], use="complete.obs")
  #corrplot(x, method="color", order="hclust")

  #x <- cov.wt(wested[, 2:length(wested)], wt=XXXX, method="unbiased", cor=TRUE)

  #http://www.scribd.com/doc/53207974/140/Pairwise-deletion

  x.x <- nearPD(x, corr=TRUE, keepDiag=TRUE, maxit=200)
  x.x <- as.matrix(x.x$mat)
  #p.x <- cor.prob(x.x)
  p.x <- cor.pval2(x.x)

  #corrplot(x.x, method="color", order="PCA")
  corrplot(x.x, method="color", order="PCA", title="CSCS/CHKS Correlation Matrix",
           p.mat=p.x, sig.level=0.05, insig="pch", pch.cex=0.1)


  #ggfluctuation(as.table(x.x[order(hclust(dist(x.x))$order), ])) +
  #              opts(legend.position="none") +
  #              labs(x="", y="")

  #x.x.x <- x.x[order(hclust(dist(p.x))$order), ]
  #p.x.x <- p.x[order(hclust(dist(p.x))$order), ]

  # Column for hpdcs_7 "zero tolerance"
  table.hpdcs_7 <- CorPValuesColumn(p.x, "hpdcs_7")
  cor.table <- cbind(as.numeric(sprintf("%0.4f", x.x[, "hpdcs_7"])),
                     as.numeric(sprintf("%0.4f", table.hpdcs_7)),
                     x.x[, "hpdcs_7"] >= 0,
                     table.hpdcs_7 < 0.05)
  colnames(cor.table) <- c("r", "p-value", "corr positive?", "95% significant")
  return(cor.table)
}

table.lausd <- CreateDistrictCorPlots(wested.lausd.x)
table.ousd <- CreateDistrictCorPlots(wested.ousd.x)
table.suhsd <- CreateDistrictCorPlots(wested.suhsd.x)
rbind(table.lausd, table.ousd, table.suhsd)


# Random Forest Models #########################################################
rf1 <- randomForest(hpdcs_5~.,
                    data=wested.lausd.x,
                    importance=TRUE,
                    na.action=na.omit)
rf2 <- randomForest(hpdcs_6~.,
                    data=wested.lausd.x,
                    importance=TRUE,
                    na.action=na.omit)
rf3 <- randomForest(hpdcs_7~.,
                    data=wested.lausd.x,
                    importance=TRUE,
                    na.action=na.omit)
rf4 <- randomForest(hpdcs_10~.,
                    data=wested.lausd.x,
                    importance=TRUE,
                    na.action=na.omit)
