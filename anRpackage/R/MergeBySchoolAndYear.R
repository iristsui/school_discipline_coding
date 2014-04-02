MergeBySchoolAndYear <-
function(x, y) {
  x1 <- x
  y1 <- y
  return(merge(x1, y1, 
               by=c("school_educationalorgid", "school_schoolyear"),
               all=TRUE))
}

