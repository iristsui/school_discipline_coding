SubsetByNames <-
function(x, y) {
  # Args:
  #   x: Data frame
  #   y: Name bounds
  var1 <- which(names(x) == y[1])
  var2 <- which(names(x) == y[2])
  return(x[, var1:var2])
}

