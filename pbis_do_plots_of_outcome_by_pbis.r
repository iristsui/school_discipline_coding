# Plots of outcome variables by PBIS measure
# By Casey Tsui
# Format: R
# Last updated: 6/12/12

# Graph PBIS implementation averages by school type

lm.data$high.school <- factor(lm.data$high.school,
                              labels=c("Middle Schools", "High Schools"))

x <- lm.data[, c("school_schoolyear", "ebs.implementationaverage", "high.school")]
x$pbis <- "EBS"
names(x)[2] <- "implementationaverage"
y <- lm.data[, c("school_schoolyear", "set.implementationaverage", "high.school")]
y$pbis <- "SET"
names(y)[2] <- "implementationaverage"
boxplot.data <- rbind(x, y)



#p <- ggplot(lm.data, aes(x=factor(school_schoolyear), y=ebs.implementationaverage)) +
#       geom_boxplot(aes(fill=factor(high.school))) +
#       opts(title="EBS implementation averages by school grade type over time") +
#       xlab("Year") +
#       ylab("EBS Implementation Average") +
#       scale_fill_discrete(name="School Type")
#q <- ggplot(lm.data, aes(x=factor(school_schoolyear), y=set.implementationaverage)) +
#       geom_boxplot(aes(fill=factor(high.school))) +
#       opts(title="SET implementation averages by school grade type over time") +
#       xlab("Year") +
#       ylab("SET Implementation Average") +
#       scale_fill_discrete(name="School Type")

levels(boxplot.data$school_schoolyear) <- c("2004-05", "2005-06", "2006-07", "2007-08")

# BOXPLOT FOR MIDDLE VS HIGH- EBS AND SET
#ggplot(boxplot.data, aes(x=factor(school_schoolyear), y=implementationaverage)) +
#       geom_boxplot(aes(fill=factor(pbis))) +
#       opts(title="PBIS implementation averages by school grade type over time") +
#       facet_grid(.~high.school) +
#       xlab("Year") +
#       ylab("PBIS Implementation Average") +
#       geom_text(aes(x="2006-07", y=50, label="EBS", colour="EBS", size=1)) +
#       geom_text(aes("2005-06", 95, label="SET", colour="SET", size=1)) +
#       scale_fill_discrete(name="School Type") +
#       opts(legend.position="none")


# BOXPLOT FOR MIDDLE VS HIGH- EBS
ggplot(boxplot.data[which(boxplot.data$pbis == "EBS"), ],
       aes(x=factor(school_schoolyear), y=implementationaverage)) +
       geom_boxplot() +
#       opts(title="Figure 1: EBS implementation by school grade type over time") +
       facet_grid(.~high.school) +
       xlab("Year") +
       ylab("EBS Implementation Average") +
       scale_fill_discrete(name="School Type") +
       theme_bw() +
       opts(legend.position="none",
            axis.title.x=theme_text(vjust=0, hjust=0.525))

#LINE PLOT BY YEAR #######################################
#USED THIS FOR LAURA'S 4/14 presentation #######################################
lm.data.ebs <- ddply(lm.data[which(lm.data$i.ebs == 1), ],
                   c("year", "pbis"),
                      function(df)
                        c(implementationaverage = mean(df$ebs.implementationaverage, na.rm=TRUE)))
lm.data.ebs <- lm.data.ebs[which(lm.data.ebs$pbis == 1), ]
lm.data.set <- ddply(lm.data[which(lm.data$i.set == 1), ],
                   c("year", "pbis"),
                      function(df)
                        c(implementationaverage = mean(df$set.implementationaverage, na.rm=TRUE)))
lm.data.set <- lm.data.set[which(lm.data.set$pbis == 2), ]
lm.data.pbis <- rbind(lm.data.ebs, lm.data.set)

lm.data.pbis$year <- factor(lm.data.pbis$year,
                            labels=c("2004", "2005", "2006", "2007"))
lm.data.pbis$pbis <- factor(lm.data.pbis$pbis,
                            labels=c("EBS", "SET"))

ggplot(lm.data.pbis[which(!is.na(lm.data.pbis$year)), ],  # Change this when you add in demographics data for missing school in IL - "Murphysboro HS" in 200506
       aes(x=factor(year), y=implementationaverage, group=factor(pbis))) +
       geom_line(aes(colour=factor(pbis)), size=2) +
       opts(title="EBS and SET implementation averages over time") +
       xlab("Year") +
       ylab("Implementation Average") +
       geom_text(aes("2005", 67, label="EBS", color="EBS")) +
       geom_text(aes("2005", 80, label="SET", color="SET")) +
       opts(legend.position="none")




#LINE PLOT BY GRADE TYPE AND YEAR ##############################################
lm.data.ebs <- ddply(lm.data[which(lm.data$i.ebs == 1), ],
                   c("year", "pbis", "high.school"),
                      function(df)
                        c(implementationaverage = mean(df$ebs.implementationaverage, na.rm=TRUE)))
lm.data.ebs <- lm.data.ebs[which(lm.data.ebs$pbis == 1), ]
lm.data.set <- ddply(lm.data[which(lm.data$i.set == 1), ],
                   c("year", "pbis", "high.school"),
                      function(df)
                        c(implementationaverage = mean(df$set.implementationaverage, na.rm=TRUE)))
lm.data.set <- lm.data.set[which(lm.data.set$pbis == 2), ]
lm.data.pbis <- rbind(lm.data.ebs, lm.data.set)

lm.data.pbis$high.school <- factor(lm.data.pbis$high.school,
                                   labels=c("MS", "HS"))
lm.data.pbis$year <- factor(lm.data.pbis$year,
                            labels=c("2004", "2005", "2006", "2007"))
lm.data.pbis$pbis <- factor(lm.data.pbis$pbis,
                            labels=c("EBS", "SET"))

ggplot(lm.data.pbis,
       aes(x=factor(year), y=implementationaverage, group=factor(pbis))) +
       geom_line(aes(colour=factor(pbis))) +
       opts(title="EBS and SET implementation averages by school type over time") +
       xlab("Year") +
       ylab("SET Implementation Average") +
       scale_colour_discrete(name="PBIS Fidelity Tool Used") + facet_grid(.~high.school)




################################################################################
lm.data.odr <- ddply(lm.data,
                     c("year", "pbis"),
                       function(df)
                         c(iss         = mean(df$iss,         na.rm=TRUE),
                           oss         = mean(df$oss,         na.rm=TRUE),
                           major       = mean(df$major,       na.rm=TRUE),
                           odr         = mean(df$odr,         na.rm=TRUE),
                           recid       = mean(df$recid,       na.rm=TRUE),
                           num.stud    = mean(df$num.stud,    na.rm=TRUE),
                           grad.rate   = mean(df$grad.rate,   na.rm=TRUE),
                           attend.rate = mean(df$attend.rate, na.rm=TRUE),
                           iss.100     = mean(df$iss.100,     na.rm=TRUE),
                           oss.100     = mean(df$oss.100,     na.rm=TRUE),
                           major.100   = mean(df$major.100,   na.rm=TRUE),
                           odr.100     = mean(df$odr.100,     na.rm=TRUE),
                           ebs.implementationaverage = mean(df$ebs.implementationaverage, na.rm=TRUE),
                           set_implementationaverage = mean(df$set_implementationaverage, na.rm=TRUE)))
lm.data.odr$year <- factor(c("2004", "2005", "2006", "2007"))
lm.data.odr$pbis <- factor(c("EBS only", "SET only", "EBS and SET"))
levels(factor(lm.data.odr$pbis)) <- c("EBS only", "SET only", "EBS and SET")


# Graph Response Variables by PBIS type
ggplot(lm.data.odr, aes(x=factor(year), y=odr.100, group=factor(pbis))) +
       geom_line(aes(colour=factor(pbis))) +
       opts(title="Mean Disc. Referrals per 100 students by PBIS type") +
       xlab("Year") +
       ylab("Mean Disciplinary Referrals per 100 students") +
       scale_colour_discrete(name="PBIS Type")


#####USING PBIS INCORPORATED####
lm.data.odr.inc <- ddply(lm.data,
                     c("year", "pbis_incorporated"),
                       function(df)
                         c(iss         = mean(df$iss,         na.rm=TRUE),
                           oss         = mean(df$oss,         na.rm=TRUE),
                           major       = mean(df$major,       na.rm=TRUE),
                           odr         = mean(df$odr,         na.rm=TRUE),
                           recid       = mean(df$recid,       na.rm=TRUE),
                           num.stud    = mean(df$num.stud,    na.rm=TRUE),
                           grad.rate   = mean(df$grad.rate,   na.rm=TRUE),
                           attend.rate = mean(df$attend.rate, na.rm=TRUE),
                           iss.100     = mean(df$iss.100,     na.rm=TRUE),
                           oss.100     = mean(df$oss.100,     na.rm=TRUE),
                           major.100   = mean(df$major.100,   na.rm=TRUE),
                           odr.100     = mean(df$odr.100,     na.rm=TRUE),
                           ebs.implementationaverage = mean(df$ebs.implementationaverage, na.rm=TRUE),
                           set_implementationaverage = mean(df$set_implementationaverage, na.rm=TRUE)))
lm.data.odr.inc$year <- factor(c("2004", "2005", "2006", "2007"))
lm.data.odr.inc$pbis_incorporated <- factor(c("EBS", "SET"))


# Graph Response Variables by PBIS type
ggplot(lm.data.odr.inc, aes(x=factor(year), y=ebs.implementationaverage, group=factor(pbis_incorporated))) +
       geom_line(aes(colour=factor(pbis_incorporated))) +
       opts(title="EBS Implementation Average by PBIS type") +
       xlab("Year") +
       ylab("EBS Implementation Average") +
       scale_colour_discrete(name="PBIS Type")

