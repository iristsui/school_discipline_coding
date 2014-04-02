CenterVar <-
function(x) {
  # Args:
  #   x: Data frame variable
  overall.mean <- mean(x, na.rm=TRUE)
  centered.var <- ifelse(!is.na(x),
                         x - overall.mean,
                         NA)
  return(centered.var)
}

