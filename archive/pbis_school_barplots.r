


barplot(table(a.new$school.gradetypeid, a.new$school_schoolyear),
        col=rainbow(3), xlab="School Year", ylab="Number of schools",
        main="Sample size of schools for each year", ylim=c(0,120))
legend("topright", c("Middle", "High", "Mixed"), cex=1.0, bty="n",
       fill=rainbow(3))

barplot(table(a.new$school.gradetypeid, a.new$school_schoolyear),
        col=rainbow(3), xlab="School Year", ylab="Number of schools",
        main="Sample size of schools for each year", beside=TRUE, ylim=c(0,60))
legend("topright", c("Middle", "High", "Mixed"), cex=1.0, bty="n",
       fill=rainbow(3))
