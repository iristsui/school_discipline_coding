a.old <- a[which(a$i.ebs.3yr==1), ]
a.old <- unique(a.old)


a.master <- a
a <- a.old


a$school.gradetypeid <- ifelse(a$nces_gslo == "09" & a$nces_gshi == "12",
                               2,          # 2=all high schools 9-12
                               ifelse((a$nces_gslo == "06" |
                                       a$nces_gslo == "07") &
                                       a$nces_gshi == "08",
                                       1,   # 1=all middle schools 6-8 or 7-8
                                       3))  # 3=mixed schools

# Keep only those schools that maintained grade type through all years
g <- table(a$school_educationalorgid, a$school.gradetypeid)
g <- g[(g[, 1] >= 3 | g[, 2] >= 3 | g[, 3] >= 3), ]
a <- KeepConsistentGrades(a, g)
b <- KeepConsistentGrades(b, g)
c <- KeepConsistentGrades(c, g)




n <- glm(major.odr.100 ~ perc.in.place.sw.disc.sys +
perc.in.place.ncr.set +
perc.in.place.cr.set +
perc.in.place.indiv.stud.sys +
                 school.gradetypeid +
                 grade.06.prop +
                 grade.07.prop +
                 grade.08.prop +
                 male.prop +
                 afram.prop +
                 asian.prop +
                 latino.prop +
                 percent.frpm +
                 nces_member +
                 school.id +
                 district.id +
                 fte.per.student,
                 family=gaussian(link="identity"), data=lm.data, subset=school.gradetypeid==1)

