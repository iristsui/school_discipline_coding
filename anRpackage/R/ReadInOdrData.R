ReadInOdrData <-
function() {
  odr <- vector("list", 4)
  odr[[1]] <- ReadCsvData("SAS-ODRData_200405to200607.csv") 
  odr[[2]] <- ReadCsvData("SAS-ODRData_200405to200708.csv")
  odr[[3]] <- ReadCsvData("SET-ODRData_200405to200607.csv")
  odr[[4]] <- ReadCsvData("SET-ODRData_200405to200708.csv")
  odr <- lapply(odr, function(x) return(x[, 1:24]))
  return(unique(do.call("rbind", odr)))
}

