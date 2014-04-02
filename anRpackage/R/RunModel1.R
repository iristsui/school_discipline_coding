RunModel1 <-
function(x) {
  # Runs GEE Model 1
  #
  # Args:
  #   x: Data frame variable
  x1 <- x
  return(gee(x1 ~ school_schoolyear +
                  perc.in.place.sw.disc.sys +
                  perc.in.place.ncr.set +
                  perc.in.place.cr.set +
                  perc.in.place.ind.stud.sys +
                  male.prop +
                  grade.type,
                  id=school_educationalorgid,
                  data=lm.data, family=gaussian, corstr="AR-M", Mv=1))
}

