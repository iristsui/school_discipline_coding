# Plots of Disciplinary Outcomes by Race
# By Casey Tsui
# Format: R
# Last updated: 5/24/11

racial.oss <- subset(master.data,
                     as.numeric(outcome.var) >= 2 &
                     as.numeric(outcome.var) <= 6 &
                     as.numeric(outcome.var) != 4 &
                     as.numeric(pbis.measure) != 5)

ggplot(racial.oss[which(as.numeric(racial.oss$pbis.measure) %% 2 != 0), ],
       aes(x=factor(outcome.var), y=exp(estimate), ymin=exp(min95), ymax=exp(max95))) +
       geom_errorbar(aes(colour=factor(pbis.measure)),
                     position=position_dodge(width=0.9),
                     width=0.5, size=2) +
       geom_point(aes(size=1.25), position=position_dodge(width=0.9))  +
       geom_hline(aes(yintercept=1)) + facet_grid(.~pbis.measure) +
       scale_y_continuous("Incidence Rate Ratio") +
       scale_x_discrete("Disciplinary Referral") +
       opts(axis.text.x=theme_text(angle=90, hjust=1), legend.position="none",
            title="IRRs for OSS by Race, for each PBIS measure")



racial.major <- subset(master.data,
                     as.numeric(outcome.var) >= 7 &
                     as.numeric(outcome.var) <= 11 &
                     as.numeric(outcome.var) != 9 &
                     as.numeric(pbis.measure) != 5)


ggplot(racial.major[which(as.numeric(racial.major$pbis.measure) %% 2 != 0), ],
       aes(x=factor(outcome.var), y=exp(estimate), ymin=exp(min95), ymax=exp(max95))) +
       geom_errorbar(aes(colour=factor(pbis.measure)),
                     position=position_dodge(width=0.9),
                     width=0.5, size=2) +
       geom_point(aes(size=1.25), position=position_dodge(width=0.9))  +
       geom_hline(aes(yintercept=1)) + facet_grid(.~pbis.measure) +
       scale_y_continuous("Incidence Rate Ratio") +
       scale_x_discrete("Disciplinary Referral") +
       opts(axis.text.x=theme_text(angle=90, hjust=1), legend.position="none",
            title="IRRs for Major ODRs by Race, for each PBIS measure")


#racial.recid <- subset(master.data,
#                     as.numeric(outcome.var) >= 13 &
#                     as.numeric(outcome.var) <= 17)
#
#ggplot(racial.recid[which(as.numeric(racial.recid$pbis.measure) %% 2 != 0), ],
#       aes(x=factor(outcome.var), y=exp(estimate), ymin=exp(min95), ymax=exp(max95))) +
#       geom_errorbar(aes(colour=factor(pbis.measure)),
#                     position=position_dodge(width=0.9),
#                     width=0.5, size=2) +
#       geom_point(aes(size=1.25), position=position_dodge(width=0.9))  +
#       geom_hline(aes(yintercept=1)) + facet_grid(.~pbis.measure) +
#       scale_y_continuous("Incidence Rate Ratio") +
#       scale_x_discrete("Disciplinary Referral") +
#       opts(axis.text.x=theme_text(angle=90, hjust=1), legend.position="none",
#            title="IRRs for Major ODRs by Race, for each PBIS measure")

racial.num.stud <- subset(master.data,
                          as.numeric(outcome.var) >= 13 &
                          as.numeric(outcome.var) <= 17 &
                          as.numeric(pbis.measure) != 5)


ggplot(racial.num.stud[which(as.numeric(racial.num.stud$pbis.measure) %% 2 != 0), ],
       aes(x=factor(outcome.var), y=exp(estimate), ymin=exp(min95), ymax=exp(max95))) +
       geom_errorbar(aes(colour=factor(pbis.measure)),
                     position=position_dodge(width=0.9),
                     width=0.5, size=2) +
       geom_point(aes(size=1.25), position=position_dodge(width=0.9))  +
       geom_hline(aes(yintercept=1)) + facet_grid(.~pbis.measure) +
       scale_y_continuous("Incidence Rate Ratio") +
       scale_x_discrete("Disciplinary Referral") +
       opts(axis.text.x=theme_text(angle=90, hjust=1), legend.position="none",
            title="IRRs for Num Stud w/ Major ODR by Race, for each PBIS measure")



racial.perc.unexcused <- subset(master.data,
                                as.numeric(outcome.var) >= 34 &
                                as.numeric(outcome.var) <= 38 &
                                as.numeric(pbis.measure) != 5)


ggplot(racial.perc.unexcused[which(as.numeric(racial.perc.unexcused$pbis.measure) %% 2 != 0), ],
       aes(x=factor(outcome.var), y=exp(estimate), ymin=exp(min95), ymax=exp(max95))) +
       geom_errorbar(aes(colour=factor(pbis.measure)),
                     position=position_dodge(width=0.9),
                     width=0.5, size=2) +
       geom_point(aes(size=1.25), position=position_dodge(width=0.9))  +
       geom_hline(aes(yintercept=1)) + facet_grid(.~pbis.measure) +
       scale_y_continuous("Incidence Rate Ratio") +
       scale_x_discrete("Disciplinary Referral") +
       opts(axis.text.x=theme_text(angle=90, hjust=1), legend.position="none",
            title="IRRs for Percent Unexcused by Race, for each PBIS measure")

