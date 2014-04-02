YearMeans <-
function(x) {
  # Computes the means over 4 years and outputs a table
  # Args:
  #   x: data frame variable name that is to be aggregated over year
  rx <- summarize(x, c$school_schoolyear, mean, na.rm=TRUE)
  return(rx)
}

