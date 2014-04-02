DepVarCorrs <-
function(x) {
  # Args:
  #   x: data frame variable - pbis measure
  lm.data.wide <- lm.data[which(!is.na(x)), c(id.names, disc.var.names)]
  lm.data.wide <- ReshapeWide(lm.data.wide)
  PrintCors(lm.data.wide)
}

