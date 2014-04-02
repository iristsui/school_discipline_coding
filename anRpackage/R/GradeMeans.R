GradeMeans <-
function(x) {
  # Computes the means over 4 years and outputs a table
  # Args:
  #   x: data frame variable name that is to be aggregated over grade id
  rx <- summarize(x, c$student_gradeid, mean, na.rm=TRUE)
  return(rx)
}

