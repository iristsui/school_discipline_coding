RemoveAltSchools <-
function(x) {
  # Args:
  #   x: Data frame
  alt.criteria <- which(schools$Filter_School_IsAltJJ_max==1)
  alts <- schools$School_EducationalOrgId[alt.criteria] 
  return(x[(!x$school_educationalorgid %in% alts), ])
}

