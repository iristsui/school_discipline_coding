FactorVariablesSETonly <-
function(x) {
  x <- transform(x, set_setid = factor(set_setid, ordered=TRUE))
}

