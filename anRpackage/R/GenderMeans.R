GenderMeans <-
function(x) {
  # Computes the means over 4 years and outputs a table
  # Args:
  #   x: data frame variable name that is to be aggregated over gender id
  rx <- summarize(x, c$student_genderid, mean, na.rm=TRUE)
  return(rx)
}

