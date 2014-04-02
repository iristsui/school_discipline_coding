SchoolMeans <-
function(x) {
  # Computes the means over 4 years and outputs a table
  # Args:
  #   x: data frame variable name that is to be aggregated over school id
  rx <- summarize(x, a$school_educationalorgid, mean, na.rm=TRUE)
  return(rx)
}

