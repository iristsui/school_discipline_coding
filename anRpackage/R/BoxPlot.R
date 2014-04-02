BoxPlot <-
function(x, y, ...) {
  # Produces box plot of 
  # Args:
  #   x: data frama variable name that is to be plotted over year
  rx <- boxplot(y ~ x)
  return(rx)
}

