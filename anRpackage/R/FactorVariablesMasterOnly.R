FactorVariablesMasterOnly <-
function(x) {
  # Factors variables that are found in only the master data set
  #
  # Args:
  #   x: Data frame variable
  x <- transform(x,
                 school_lowgradeid      = factor(school_lowgradeid),
                 school_lowgradelabel   = factor(school_lowgradelabel),
                 school_highgradeid     = factor(school_highgradeid),
                 school_highgradelabel  = factor(school_highgradelabel),
                 school_schoolyear_nces = factor(school_schoolyear_nces),
                 nces_ncessch           = factor(nces_ncessch),
                 nces_type              = factor(nces_type),
                 nces_locale            = factor(nces_locale),
                 nces_ulocal            = factor(nces_ulocal),
                 nces_gslo              = factor(nces_gslo),
                 nces_gshi              = factor(nces_gshi),
                 nces_level             = factor(nces_level))
}

