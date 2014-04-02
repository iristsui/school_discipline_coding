ReshapeWide <-
function(x) {
  # Args:
  #   x: Data frame
  reshape(x, v.names=disc.var.names,
             timevar="school_schoolyear",
             idvar="school_educationalorgid",
             direction="wide", sep=".")
}

