CreateFinalOdrData <-
function() {
  setwd(PasteFileName("Gottlieb Revised Data Files 9-16-2010"))
  odr <- ReadInOdrData()

  setwd(PasteFileName("StudentIds_20101019"))
  s.id <- ReadInStudentIdData()

  return(unique(merge(odr, s.id)))
}

