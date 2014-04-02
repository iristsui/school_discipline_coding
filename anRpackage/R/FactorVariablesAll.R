FactorVariablesAll <-
function(x) {
  # Factors variables that are found in all 4 data sets
  #
  # Args:
  #   x: Data frame variable
  x <- transform(x, school_schoolyear       = factor(school_schoolyear,
                                                     ordered=TRUE),
                    school_educationalorgid = factor(school_educationalorgid,
                                                     ordered=TRUE),
                    school_ncesschoolid     = factor(school_ncesschoolid,
                                                     ordered=TRUE))
}

