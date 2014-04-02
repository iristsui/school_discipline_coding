DropSingleMixedYearOR <-
function(x) {
  # Args:
  #   x: Data frame object
  x <- x[-which((x$school_educationalorgid == "327" |
               x$school_educationalorgid == "328")
               & x$school_schoolyear == "200405"), ]
}

