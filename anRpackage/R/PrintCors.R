PrintCors <-
function(x) {
  iss.cors <- x[, which(substr(names(x), 1, 3) == "iss")]
  print(cor(iss.cors, use="complete.obs"))

  oss.cors <- x[, which(substr(names(x), 1, 3) == "oss")]
  print(cor(oss.cors, use="complete.obs"))

#  expul.cors <- x[, which(substr(names(x), 1, 5) == "expul")]
#  cor(expul.cors, use="complete.obs")

  major.cors <- x[, which(substr(names(x), 1, 5) == "major")]
  print(cor(major.cors, use="complete.obs"))

  odr.cors <- x[, which(substr(names(x), 1, 3) == "odr")]
  print(cor(odr.cors, use="complete.obs"))

  recid.cors <- x[, which(substr(names(x), 1, 5) == "recid")]
  print(cor(recid.cors, use="complete.obs"))

  num.stud.cors <- x[, which(substr(names(x), 1, 8) == "num.stud")]
  print(cor(num.stud.cors, use="complete.obs"))

#  stud.repeats.cors <- x[, which(substr(names(x), 1, 12) == "stud.repeats")]
#  print(cor(stud.repeats.cors, use="complete.obs"))

  grad.cors <- x[, which(substr(names(x), 1, 9) == "grad.rate")]
  print(cor(grad.cors, use="complete.obs"))

  attend.cors <- x[, which(substr(names(x), 1, 11) == "attend.rate")]
  print(cor(attend.cors, use="complete.obs"))
}

