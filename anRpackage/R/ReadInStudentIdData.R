ReadInStudentIdData <-
function() {
  s.id <- vector("list", 4)
  s.id[[1]] <- ReadCsvData("SAS-StudentId_200405to200607.csv")
  s.id[[2]] <- ReadCsvData("SAS-StudentId_200405to200708.csv")
  s.id[[3]] <- ReadCsvData("SET-StudentId_200405to200607.csv")
  s.id[[4]] <- ReadCsvData("SET-StudentId_200405to200708.csv")
  s.id <- unique(do.call("rbind", s.id))
  return(s.id[, c("ODR_ReferralId", "Student_StudentId")])
}

