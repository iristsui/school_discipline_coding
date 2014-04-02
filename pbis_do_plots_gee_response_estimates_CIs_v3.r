# Plots of GEE response variable point estimates
# By Casey Tsui
# Format: R
# Last updated: 6/12/12


# FUNCTION CALLS ###############################################################
LoadMasterData <- function(x=NULL) {
  dataset <- x
  root <- "/Users/HumanImpactPartners/Documents/hias/school_discipline_hia/data/pbis_data/stata_gee_estimates"
  setwd(paste(root, dataset, sep=""))

  for (i in 1:5) {
    for (j in 1:40) {
      file <- paste(paste("model", i, j, sep="_"), ".csv", sep="")
      data.new <- read.csv(file)
      data.new <- data.new[4, ]
      data.new$pbis.measure <- i
      data.new$outcome.var <- j
      if (j==1) data.old <- data.new
      else data.old <- rbind(data.old, data.new)
    }
    if (i==1) master.data <- data.old
    else master.data <- rbind(master.data, data.old)
  }
  master.data$pbis.measure <- factor(master.data$pbis.measure,
                                   labels=c("EBS continuous", "SET continuous",
                                            "EBS criterion", "SET criterion",
                                            "Combined continuous"))
  master.data$pbis.measure <- factor(master.data$pbis.measure,
                                   levels=c("EBS continuous", "EBS criterion",
                                            "SET continuous", "SET criterion",
                                            "Combined continuous"))
  master.data$outcome.var <- factor(master.data$outcome.var,
                                    labels=c("ISS", "OSS", "Afram OSS",
                                             "Asian OSS",
                                             "Latino OSS", "White OSS",
                                             "Major ODRs", "Afram Major ODRs",
                                             "Asian Major ODRs",
                                             "Latino Major ODRs",
                                             "White Major ODRs",
                                             "Total ODRs",
                                             "Num Stud", "Afram Num Stud",
                                             "Asian Num Stud",
                                             "Latino Num Stud",
                                             "White Num Stud",
                                             "Afram Major Over White",
                                             "Asian Major Over White",
                                             "Latino Major Over White",
                                             "Afram Major Disp Index",
                                             "Asian Major Disp Index",
                                             "Latino Major Disp Index",
                                             "Afram OSS Disp Index",
                                             "Asian OSS Disp Index",
                                             "Latino OSS Disp Index",
                                             "Recid",
                                             "Afram Recid", "Asian Recid",
                                             "Latino Recid", "White Recid",
                                             "Grad Rate", "Attend Rate",
                                             "% Students Unexcused",
                                             "Afram % Students Unexcused",
                                             "Asian % Students Unexcused",
                                             "Latino % Students Unexcused",
                                             "White % Students Unexcused",
                                             "% Proficient/Advanced Reading",
                                             "% Proficient/Advanced Math"))

  master.data <- transform(master.data,
                           exp_estimate = exp(estimate),
                           exp_min95 = exp(min95),
                           exp_max95 = exp(max95))
                           

  return(master.data)
}


SubsetMasterData <- function(x, y=NULL, z) {
  df <- x
  pbis <- y
  outcome <- z

  sub <- subset(df, as.numeric(outcome.var) == outcome)
  if (is.character(pbis)) sub <- subset(sub, substr(pbis.measure, 1, 3) == pbis)
  sub$pbis.measure <- factor(sub$pbis.measure)
  return(sub)
}


CreateRangePlot <- function(x, y=NULL, z) {
  df <- x
  pbis <- y
  outcome <- z
  df <- SubsetMasterData(df, pbis, outcome)

#  if (outcome <= 6) {
#    df <- transform(df,
#                    est = exp(estimate),
#                    min = exp(min95),
#                    max = exp(max95))
#    axes <- list(scale_y_continuous("Incidence Rate Ratio"),
#                 scale_x_discrete(""))
#  } else {
#    df <- transform(df,
#                    est = estimate,
#                    min = min95,
#                    max = max95)
#    axes <- list(scale_y_continuous("Percentage Points"),
#                 scale_x_discrete(""))
#  }

#  if (outcome <= 40) {
    df <- transform(df,
                    est = exp(estimate),
                    min = exp(min95),
                    max = exp(max95))

    if (outcome <= 26) {
      axes <- list(scale_y_continuous("Incidence Rate Ratio"),
                   scale_x_discrete(""),
                   geom_hline(aes(yintercept=1)))
    } else {
      axes <- list(scale_y_continuous("Odds Ratio"),
                   scale_x_discrete(""),
                   geom_hline(aes(yintercept=1)))
    }
#  } else {
#    df <- transform(df,
#                    est = estimate,
#                    min = min95,
#                    max = max95)
#    axes <- list(scale_y_continuous(""),
#                 scale_x_discrete(""),
#                 geom_hline(aes(yintercept=0)))
#  }


  p <- ggplot(df,
       aes(x=pbis.measure, y=est, ymin=min, ymax=max)) +
       geom_errorbar(aes(color=pbis.measure), width=0.5, size=2) +
       geom_point(aes(size=2)) + axes +
       opts(legend.position="none")
  return(p)
}


# FUNCTION CALLS ###############################################################
master.data <- LoadMasterData("/pbis_full")
#master.data <- LoadMasterData("/pbis_1_unit")
master.data.original <- LoadMasterData("/original")
master.data.hs.dropped <- LoadMasterData("/hs_dropped")
master.data.3.years.only <- LoadMasterData("/3_years_only")
master.data.hs.dropped.3.years.only <- LoadMasterData("/hs_dropped_3_years_only")



# Range plot for OSS - calculated from Stata
CreateRangePlot(master.data, "EBS", z=2) +
       opts(title="GEE estimates of Out-of-school Suspension Incidence Rate Ratios & 95% CI")


# Range plot for Major ODRs - calculated from Stata
CreateRangePlot(master.data, "EBS", z=7) +
       opts(title="GEE estimates of Major ODR Incidence Rate Ratios & 95% CI")


# Range plot for Graduation Rates - calculated from Stata
# From linear regression
CreateRangePlot(master.data, "EBS", z=32) +
       opts(title="GEE estimates of Graduation Rates & 95% CI")


# Range plot for Graduation Rates - calculated from R
# From binomial regression
CreateRangePlot(grad.rate.gees, "EBS", z=32) +
       opts(title="GEE estimates of Graduation Rate & 95% CI")


# Range plot for Attendance Rates
CreateRangePlot(master.data, "EBS", z=33) +
       opts(title="GEE estimates of Attendance Rates & 95% CI")




#ggplot(master.data[which(master.data$y==2), ],
#       aes(x=x, y=exp(estimate), ymin=exp(min95), ymax=exp(max95))) +
#       geom_crossbar(aes(fill=x)) + axes +
#       opts(title="Incidence Rate Ratio of Out-of-school Suspensions & 95% CI",
#            legend.position="none") +
#       geom_hline(aes(yintercept=1))
#
#ggplot(master.data[which(master.data$y==4), ],
#       aes(x=x, y=exp(estimate), ymin=exp(min95), ymax=exp(max95))) +
#       geom_crossbar(aes(fill=x)) + axes +
#       opts(title="Incidence Rate Ratio of Total ODRs & 95% CI",
#            legend.position="none") +
#       geom_hline(aes(yintercept=1))
#
#ggplot(master.data[which(master.data$y==4), ],
#       aes(x=x, y=exp(estimate), ymin=exp(min95), ymax=exp(max95))) +
#       geom_pointrange(aes(colour=x)) + axes +
#       opts(title="Incidence Rate Ratio of Total ODRs & 95% CI",
#            legend.position="none") +
#       geom_hline(aes(yintercept=1)) + geom_errorbar(aes(color=x), width=0.5)
#

# Plot both odr vars on same graph, for all pbis measures - facet=outcome
sub.data.odr <- subset(master.data,
                       (as.numeric(outcome.var) == 2 |
                        as.numeric(outcome.var) == 7) &
                       as.numeric(pbis.measure) != 5)

ggplot(sub.data.odr,
       aes(x=pbis.measure, y=exp(estimate), ymin=exp(min95), ymax=exp(max95))) +
       geom_errorbar(aes(colour=factor(pbis.measure)),
                     position=position_dodge(width=0.9),
                     width=0.5, size=2) +
       geom_point(aes(size=1.25), position=position_dodge(width=0.9))  +
       geom_hline(aes(yintercept=1)) + facet_grid(.~outcome.var) +
       scale_y_continuous("Incidence Rate Ratio") +
       scale_x_discrete("PBIS Measure") +
       opts(axis.text.x=theme_text(angle=90, hjust=1), legend.position="none",
            title="IRRs for PBIS measures, for each ODR variable")


# Plot both odr vars on same graph, for all pbis measures - facet=pbis
#ggplot(sub.data.odr,
#       aes(x=factor(outcome.var), y=exp(estimate), ymin=exp(min95), ymax=exp(max95))) +
#       geom_errorbar(aes(colour=factor(pbis.measure)),
#                     position=position_dodge(width=0.9),
#                     width=0.5, size=2) +
#       geom_point(aes(size=1.25), position=position_dodge(width=0.9))  +
#       geom_hline(aes(yintercept=1)) + facet_grid(.~pbis.measure) +
#       scale_y_continuous("Incidence Rate Ratio") +
#       scale_x_discrete("Disciplinary Referral") +
#       opts(axis.text.x=theme_text(angle=90, hjust=1), legend.position="none",
#            title="IRRs for ODR variables, for each PBIS measure")


# Plot both odr vars on same graph, for all pbis measures - facet=pbis
# FLIP COORDS
ggplot(sub.data.odr,
       aes(x=factor(outcome.var), y=exp(estimate), ymin=exp(min95), ymax=exp(max95))) +
       geom_errorbar(aes(colour=factor(pbis.measure)),
                     position=position_dodge(width=0.9),
                     width=0.5, size=2) +
       geom_point(aes(size=1.25), position=position_dodge(width=0.9))  +
       geom_hline(aes(yintercept=1)) + facet_grid(pbis.measure~.) +
       scale_y_continuous("Incidence Rate Ratio") +
       scale_x_discrete("Disciplinary Referral") +
       coord_flip() +
       opts(axis.text.x=theme_text(angle=90, hjust=1), legend.position="none",
            title="IRRs for ODR variables, for each PBIS measure")


# OR variables############################################################ 
#master.data.x <- master.data[-which(master.data$outcome.var == "Grad Rate"), c(2,5,6:9)]
#master.data.x$outcome.var <- as.numeric(as.character(master.data.x$outcome.var))
#master.data.x <- rbind(master.data.x, grad.rate.gees)

sub.data.percs <- subset(master.data,
                         (as.numeric(outcome.var) == 27 |
                          as.numeric(outcome.var) == 13 |  
                          as.numeric(outcome.var) == 34 |  
                          as.numeric(outcome.var) == 39 |  
                          as.numeric(outcome.var) == 40) &
                         as.numeric(pbis.measure) != 5)

#ggplot(sub.data.percs,
#       aes(x=factor(outcome.var), y=exp(estimate), ymin=exp(min95), ymax=exp(max95))) +
#       geom_errorbar(aes(colour=factor(pbis.measure)),
#                     position=position_dodge(width=0.9),
#                     width=0.5, size=2) +
#       geom_point(aes(size=1.25), position=position_dodge(width=0.9))  +
#       geom_hline(aes(yintercept=1)) + facet_grid(.~pbis.measure) +
#       scale_y_continuous("Odds Ratio") +
#       scale_x_discrete("Outcome Measure") +
#       opts(axis.text.x=theme_text(angle=90, hjust=1), legend.position="none",
#            title="ORs for Disciplinary and Educational Percents, for each PBIS measure")


ggplot(sub.data.percs,
       aes(x=factor(outcome.var), y=exp(estimate), ymin=exp(min95), ymax=exp(max95))) +
       geom_errorbar(aes(colour=factor(pbis.measure)),
                     position=position_dodge(width=0.9),
                     width=0.5, size=2) +
       geom_point(aes(size=1.25), position=position_dodge(width=0.9))  +
       geom_hline(aes(yintercept=1)) + facet_grid(pbis.measure~.) +
       scale_y_continuous("Odds Ratio") +
       scale_x_discrete("Outcome Measure") +
       coord_flip() +
       opts(axis.text.x=theme_text(angle=90, hjust=1), legend.position="none",
            title="ORs for Disciplinary and Educational Percents, for each PBIS measure")


#ggplot(master.data.x[which(as.numeric(master.data.x$outcome.var) >= 5 & as.numeric(master.data.x$outcome.var) <= 9), ],
#       aes(x=factor(outcome.var), y=exp(estimate), ymin=exp(min95), ymax=exp(max95))) +
#       geom_errorbar(aes(colour=factor(pbis.measure)),
#                     position=position_dodge(width=0.9),
#                     width=0.5, size=2) +
#       geom_point(aes(size=1.25), position=position_dodge(width=0.9))  +
#       geom_hline(aes(yintercept=1)) + facet_grid(.~pbis.measure) +
#       scale_y_continuous("Odds Ratio") +
#       scale_x_discrete("Outcome Measure") +
#       opts(axis.text.x=theme_text(angle=90, hjust=1), legend.position="none",
#            title="ORs for Disciplinary and Educational Percents, for each PBIS measure")


# Racial Disproportionality
sub.data.racial <- subset(master.data,
                         (as.numeric(outcome.var) >= 21 &
                          as.numeric(outcome.var) <= 26 &  
                          as.numeric(outcome.var) != 22 &  
                          as.numeric(outcome.var) != 25) &  
                          as.numeric(pbis.measure) != 5)




# Facet=outcome
ggplot(sub.data.racial,
       aes(x=factor(pbis.measure), y=exp(estimate), ymin=exp(min95), ymax=exp(max95))) +
       geom_errorbar(aes(colour=factor(pbis.measure)),
                     position=position_dodge(width=0.9),
                     width=0.5, size=2) +
       geom_point(aes(size=1.25), position=position_dodge(width=0.9))  +
       geom_hline(aes(yintercept=1)) + facet_grid(.~outcome.var) +
       scale_y_continuous("Ratio Estimate") +
       scale_x_discrete("PBIS Measure") +
       opts(axis.text.x=theme_text(angle=90, hjust=1), legend.position="none",
            title="ORs for Racial Disparity Ratios, for each PBIS measure")

# Facet=pbis
#ggplot(sub.data.racial,
#       aes(x=factor(outcome.var), y=exp(estimate), ymin=exp(min95), ymax=exp(max95))) +
#       geom_errorbar(aes(colour=factor(pbis.measure)),
#                     position=position_dodge(width=0.9),
#                     width=0.5, size=2) +
#       geom_point(aes(size=1.25), position=position_dodge(width=0.9))  +
#       geom_hline(aes(yintercept=1)) + facet_grid(.~pbis.measure) +
#       scale_y_continuous("Odds Ratio") +
#       scale_x_discrete("PBIS Measure") +
#       opts(axis.text.x=theme_text(angle=90, hjust=1), legend.position="none",
#            title="ORs for Racial Disparity Ratios, for each PBIS measure")

ggplot(sub.data.racial,
       aes(x=factor(outcome.var), y=exp(estimate), ymin=exp(min95), ymax=exp(max95))) +
       geom_errorbar(aes(colour=factor(pbis.measure)),
                     position=position_dodge(width=0.9),
                     width=0.5, size=2) +
       geom_point(aes(size=1.25), position=position_dodge(width=0.9))  +
       geom_hline(aes(yintercept=1)) + facet_grid(pbis.measure~.) +
       scale_y_continuous("Incidence Rate Ratio") +
       scale_x_discrete("PBIS Measure") +
       coord_flip() +
       opts(axis.text.x=theme_text(angle=90, hjust=1), legend.position="none",
            title="IRRs for Racial Disparity Ratios, for each PBIS measure")


# For Laura, 4/24/11
#ggplot(master.data[which(as.numeric(master.data$pbis.measure) %% 2 != 0 & master.data$outcome.var == "Latino Major Over White"), ],
#       aes(x=factor(outcome.var), y=estimate, ymin=min95, ymax=max95)) +
#       geom_errorbar(aes(colour=factor(pbis.measure)),
#                     position=position_dodge(width=0.9),
#                     width=0.5, size=2) +
#       geom_point(aes(size=1.25), position=position_dodge(width=0.9))  +
#       geom_hline(aes(yintercept=0)) + facet_grid(.~pbis.measure) +
#       scale_y_continuous("Point Estimate") +
#       scale_x_discrete("Disciplinary Referral") +
#       opts(axis.text.x=theme_text(angle=90, hjust=1), legend.position="none",
#            title="GEE estimates for Latino Major ODR Ratio Over White, for each PBIS measure")

mas <- rbind(sub.data.odr, sub.data.percs, sub.data.racial)
mas <- mas[which(as.numeric(mas$pbis.measure) %% 2 == 1), ]
mas$pbis.measure <- factor(mas$pbis.measure)
levels(mas$pbis.measure) <- c("EBS", "SET")
mas$outcome.var <- factor(mas$outcome.var)
mas$type <- c(rep("IRR", 4), rep("OR", 18))
mas$outcome.var <- factor(mas$outcome.var,
                          levels=levels(mas$outcome.var)[c(2, 1, 8, 3, 9, 4:7, 10:11)],
                          labels=c("Major ODRs", "Out-of-school Suspensions",
                                   "Recidivism",
                                   "% Students with At Least 1 ODR",
                                   "% With Unexcused Absences",
                                   "Af-Am Major ODR Disparity Index",
                                   "Latino Major ODR Disparity Index",
                                   "Af-Am OSS Disparity Index",
                                   "Latino OSS Disparity Index",
                                   "% Proficient/Advanced Reading",
                                   "% Proficient/Advanced Math"))
flevels <- levels(mas$outcome.var)

ggplot(mas[which(mas$pbis.measure == "EBS"), ],
       aes(x=factor(outcome.var), y=exp(estimate), ymin=exp(min95), ymax=exp(max95), shape=type)) +
       geom_pointrange() +
#       geom_errorbar(aes(colour=factor(pbis.measure)),
#                     position=position_dodge(width=0.9),
#                     width=0.5, size=1.75) +
#       geom_point(position=position_dodge(width=0.9))  +
       geom_hline(aes(yintercept=1)) +  #facet_grid(type~.) +
       scale_x_discrete("", limits=rev(flevels)) +
       scale_y_log("Log Ratio", 
                   breaks=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 1.5, 2, 3, 4, 5, 6, 7),
                   labels=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 1.5, 2, 3, 4, 5, 6, 7)) +
       scale_shape_discrete("") +
       coord_flip() + 
       theme_bw() +
       opts(legend.position=c(0.9, 0.75),
#            title="Figure 2: Incidence rate ratio and odds ratio estimates for disciplinary \n and academic outcomes using generalized estimating equations",
            axis.title.x = theme_text(hjust=0.65, vjust=-0))


################################################################################
#master.data$outcome.var.2 <- factor(master.data$outcome.var,
#                                    levels=c("Major ODRs", "OSS",
#                                           "% Students Unexcused",
#                                           "Afram over White",
#                                           "Latino over White", 
#                                           "Grad Rate",
#                                           "ISS", "Total ODRs", "Recid",
#                                           "Num Stud", "Attend Rate",
#                                           "Asian over White"))
#
#ggplot(master.data[which(as.numeric(master.data$outcome.var.2) >= 4 & as.numeric(master.data$outcome.var.2) <= 5), ],
#       aes(x=factor(outcome.var.2), y=estimate, ymin=min95, ymax=max95)) +
#       geom_errorbar(aes(colour=factor(pbis.measure)),
#                     position=position_dodge(width=0.9),
#                     width=0.5, size=2) +
#       geom_point(aes(size=1.25), position=position_dodge(width=0.9))  +
#       geom_hline(aes(yintercept=1)) + 
#       scale_y_continuous("Ratio Estimate") +
#       scale_x_discrete("PBIS Measure") +
#       opts(axis.text.x=theme_text(angle=90, hjust=1), legend.position="none",
#            title="GEE estimates for Racial Disproportionality Outcomes Under Each PBIS Measure") +
#       facet_grid(.~pbis.measure)
