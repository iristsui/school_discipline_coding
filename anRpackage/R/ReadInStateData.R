ReadInStateData <-
function(state) {
  state <- as.character(state)
  setwd(PasteFileName(state))
  demo.file <- paste(state, "demographics.csv", sep="_")
  demographics <- ReadCsvData(demo.file)
  #assess.file <- paste(state, "assessments.csv", sep="_")
  #assessments <- ReadCsvData(assess.file)
  state.data <- demographics  # merge with assessments later
  return(state.data)
}

