
lm.ebs <- subset(lm.data, i.ebs == 1)
lm.ebs.model <- subset(lm.ebs,
                 select=c(major, oss, recid, num.stud, perc.unexcused,
                          afram.major.disp.index, asian.major.disp.index, latino.major.disp.index,
                          afram.oss.disp.index, asian.oss.disp.index, latino.oss.disp.index,
                          reading.score, math.score,
                          ebs.implementationaverage,
                          school_educationalorgid, year, high.school,
                          male.perc, afram.perc, asian.perc, latino.perc,
                          frpm.perc, nces_member, stud.per.fte, urbanicity))

lm.ebs.mice <- mice(lm.ebs.model)
