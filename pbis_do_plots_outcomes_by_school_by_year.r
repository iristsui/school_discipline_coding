# PBIS Plots of outcome variables by school by year
# By Casey Tsui
# Format: R
# Last updated: 3/10/11

ggplot(lm.data, aes(x=factor(year), y=odr, group=school_educationalorgid)) +
       geom_point() + facet_wrap(~school_educationalorgid) +
       opts(title="Mean ODRs by School per Year") +
       scale_y_continuous(name="Mean Disciplinary Referrals") +
       scale_x_discrete(name="Year")

