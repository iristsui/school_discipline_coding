EthnicityMeans <-
function(x) {
  # Computes the means over 4 years and outputs a table
  # Args:
  #   x: data frame variable name that is to be aggregated over eth. id
  rx <- summarize(x, c$student_ethnicityid, mean, na.rm=TRUE)
  return(rx)
}

