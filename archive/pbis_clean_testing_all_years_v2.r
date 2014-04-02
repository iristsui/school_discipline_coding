# PRELIMINARY CLEANING AND RECODING ############################################
# Convert all variable names to lowercase
#schools <- upData(schools, lowernames=TRUE)

master.data <- lapply(master.data, upData, lowernames=TRUE)

schools1 <- schools

for (i in c(1, 3, 5, 7)) {
  pbis.test.mode <- ifelse(i < 5, "ebs", "set")

  schools.and.surveys <- master.data[[i]]
  odrs <- master.data[[i + 1]]

  # SAVE ALL SCHOOL-LEVEL VARIABLES AS OBJECT "A"
  a <- MakeA(schools.and.surveys)

  # SAVE ALL EBS MEASURE VARIABLES AS OBJECT "B.EBS"
  b.ebs <- MakeB(schools.and.surveys, "ebs")

  # SAVE ALL FIDELITY MEASURE VARIABLES AS OBJECT "B.SET"
  b.set <- MakeB(schools.and.surveys, "set")


  # SAVE ALL ODR VARIABLES AS OBJECT "C"
  c <- MakeC(odrs)

  # Remove initial datasets to free up memory and increase computational speed
  rm(schools.and.surveys, odrs)

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
  rm(g)

  test.merge.data <- data.frame(unique(a[, c("school_educationalorgid", "school_ncesschoolid", "school_statelabel")]))
  test.merge.data$indicator <- 1

  schools1 <- merge(schools1, test.merge.data,
                   by="school_educationalorgid",
                   all.y=TRUE)
}

schools1$ebs.set.3yr <- ifelse(schools1$ebs.a.3yr == 1 &
                               schools1$set.a.3yr == 1,
                               1,
                               0)
schools1$ebs.set.4yr <- ifelse(schools1$ebs.a.4yr == 1 &
                               schools1$set.a.4yr == 1,
                               1,
                               0)

write.csv(schools1, "schools1.csv")
