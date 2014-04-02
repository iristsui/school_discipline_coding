ReadInPbisData <-
function(x) {
  pbis <- vector("list", 2)
  file1 <- paste(x, "-SurveyData_200405to200607.csv", sep="")
  file2 <- paste(x, "-SurveyData_200405to200708.csv", sep="")
  pbis[[1]] <- ReadCsvData(file1)
  pbis[[2]] <- ReadCsvData(file2)
  return(unique(do.call("rbind", pbis)))
}

