# Do file for racial disproportionality
# By Casey Tsui
# Format: R
# Last updated 4/4/11


# FUNCTION DEFINITIONS ######################################################### 
# Create counts of total odrs and oss by race
PlotRacialDisproportionality <- function(x, y) {
  df <- transform(x, outcome = x[, y])
  p <- ggplot(df,
       aes(x=factor(school_schoolyear), y=outcome, group=factor(race))) +
       geom_line(aes(colour=factor(race))) +
       scale_colour_discrete(name="") +
       scale_x_discrete("Year")
  return(p)
}


# FUNCTION CALLS ###############################################################
PlotRacialDisproportionality(races.odr.z, "major.over.white") +
       opts(title="Racial Disproportionality of Major ODRs over Time") +
       scale_y_continuous("Proportion of Major ODRs by Racial Group over Proportion of Major ODRs by Whites")


PlotRacialDisproportionality(races.odr.z, "major.ratio") +
       opts(title="Racial Disproportionality of Major ODRs over Time") +
       scale_y_continuous("Proportion of Major ODRs over Proportion Enrolled")


PlotRacialDisproportionality(races.odr.z, "odr.ratio") +
       opts(title="Racial Disproportionality of ODRs over Time") +
       scale_y_continuous("Proportion of ODRs over Proportion Enrolled")


PlotRacialDisproportionality(races.odr.z, "oss.ratio") +
       opts(title="Racial Disproportionality of OSSs over Time") +
       scale_y_continuous("Proportion of OSSs over Proportion Enrolled")


# NAIVE ANOVA###################################################################
with(races.odr.x, summary(aov(afram.odr.ratio ~ year)))
with(races.odr.x, summary(aov(latino.odr.ratio ~ year)))
with(races.odr.x, summary(aov(afram.oss.ratio ~ year)))
with(races.odr.x, summary(aov(latino.oss.ratio ~ year)))


# BOXPLOTS #####################################################################
ggplot(races.odr.x, aes(factor(year), afram.odr.ratio)) + geom_boxplot()
ggplot(races.odr.x, aes(factor(year), latino.odr.ratio)) + geom_boxplot()
ggplot(races.odr.x, aes(factor(year), afram.oss.ratio)) + geom_boxplot()
gplot(races.odr.x, aes(factor(year), latino.oss.ratio)) + geom_boxplot()


ggplot(races.odr.z.frpm,
       aes(x=factor(school_schoolyear), y=major.over.white, group=factor(race))) +
       geom_line(aes(colour=factor(race))) +
       scale_colour_discrete(name="") +
       scale_x_discrete("Year") +  facet_grid(.~frpm.cat) +
       scale_y_continuous("Ratio of Major ODRs Compared to Whites") +
       opts(title="Ratio of Major ODRs Compared to Whites, By FRPM Tertile")
       
