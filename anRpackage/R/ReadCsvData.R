ReadCsvData <-
function(x) {
  return(read.csv(x, header=TRUE, sep=",", na.strings="NULL"))
}

