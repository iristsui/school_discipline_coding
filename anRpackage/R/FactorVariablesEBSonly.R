FactorVariablesEBSonly <-
function(x) {
  x <- transform(x,
                 sasurvey_selfassessmentid = factor(sasurvey_selfassessmentid,
                                                    ordered=TRUE),
                 sasurvey_ebssurveyid   = factor(sasurvey_ebssurveyid,
                                                 ordered=TRUE),
                 sasurvey_schoolbeginyear  = factor(sasurvey_schoolbeginyear,
                                                    ordered=TRUE),
                 sasurvey_datecompleted = factor(sasurvey_datecompleted),
                 sasurvey_respondentoccupationid = factor(sasurvey_respondentoccupationid))
}

