PropTable <-
function(x, y) {
  # Outputs a proportion table for x and y
  # Args:
  #   x: data
  rx <- table(x, y)
  prop.table(rx, margin=2) * 100
}

