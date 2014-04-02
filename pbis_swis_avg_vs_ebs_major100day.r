Line Chart for comparing SWIS average major/100/day to our sample
By Casey Tsui
Format: R
Last updated: 6/27/11

################################################################################

x <- read.csv("/Users/HumanImpactPartners/documents/hias/school_discipline_hia/data/swis_averages/swis_test_for_sig_assoc.csv",
              sep=",", header=TRUE)


x$high.school <- factor(x$high.school,
                        levels=c("MS", "HS"),
                        labels=c("Middle Schools", "High Schools"))
x$factor <- factor(x$factor,
                   levels=c(0, 1),
                   labels=c("SWIS Average", "Sample Data"))

#x$school_schoolyear <- factor(x$school_schoolyear,
#                              levels=c(200405, 200506, 200607, 200708),
#                              labels=c("2004", "2005", "2006", "2007"))
#x$school_schoolyear <- as.integer(as.character(x$school_schoolyear))
#x$school_schoolyear <- sprintf("%4d", x$school_schoolyear)

ggplot(x,
       aes(x=school_schoolyear, y=mean_major.100.day)) +
       geom_line(aes(colour = factor(factor))) +
       facet_grid(.~high.school) + scale_colour_discrete("") +
       opts(title="Major ODRs: SWIS Average versus PBIS Sample") +
       ylab("Mean Major ODRs per 100 Students Per Instructional Day") +
       xlab("School Year") + 
       scale_x_continuous(breaks=c(2004, 2005, 2006, 2007),
                          labels=c("2004", "2005", "2006", "2007")) +
       scale_y_continuous(limits=c(0, 1.3))
