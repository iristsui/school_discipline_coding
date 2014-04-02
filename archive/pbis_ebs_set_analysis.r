both <- subset(a, i.ebs == 1 & i.set == 1)
gen.names <- c("school_educationalorgid", "school_schoolyear",
               "school.gradetypeid")
both.names <- names(both[, (substr(names(a), 1, 3) == "sas" &
                           (substr(names(a), nchar(names(a)),
                            nchar(names(a))) == "s" |
                            substr(names(a), nchar(names(a)) - 1,
                            nchar(names(a))) == "id")) |
                           substr(names(a), 1, 3) == "set"])
both.names <- c(gen.names, both.names)
both <- both[both.names]

# Sample size
length(unique(both$school_educationalorgid))

names <- c("school_educationalorgid", "school.gradetypeid")
both.unique <- unique(both[names])





sw.sub <- subset(both,
                 select=sasurvey_studentexpectationss:sasurvey_districtreports)
ncr.sub <- subset(both,
                  select=sasurvey_ncrbehaviorss:sasurvey_ncrallstaffs)
cr.sub <- subset(both,
                 select=sasurvey_crbehaviorsdefineds:sasurvey_crtransitionsefficients)
ind.sub <- subset(both,
                  select=sasurvey_indregularassessmentss:sasurvey_indbehaviormonitoreds)


both$perc.in.place.sw.disc.sys  <- rowSums(sw.sub  == 2, na.rm=TRUE) / 18
both$perc.in.place.ncr.set      <- rowSums(ncr.sub == 2, na.rm=TRUE) /  9
both$perc.in.place.cr.set       <- rowSums(cr.sub  == 2, na.rm=TRUE) / 11
both$perc.in.place.ind.stud.sys <- rowSums(ind.sub == 2, na.rm=TRUE) /  8


cor(both$perc.in.place.sw.disc.sys, both$set_expectationsdefined)
cor(both$perc.in.place.ncr.set, both$set_criterion8080)
[1] 0.06258212
> cor(both$perc.in.place.cr.set, both$set_criterion8080)
[1] 0.01094143
> cor(both$perc.in.place.ind.stud.sys, both$set_criterion8080)


for (i in 99:102) {
  print(paste("*****", names(both[i]), "*****", sep=" "))
  for (j in 9:15) {
    print(names(both[j]))
    print(cor(both[, i], both.set[, j]))
  }
}


hist(both$ebs_implementationaverage, main="EBS implementation average percent")
hist(both$set_implementationaverage, main="SET implementation average percent")

qqplot(both[
