FitDistr <-
function(x) {
  # Args:
  #   x: String of response variable to fit
  dists <- c("normal", "logistic", "lognormal", "Poisson", "negative binomial")
  dep.var <- lm.data[, x]
  for (i in seq(along=dists)) {
    print(paste(dists[i], ": ", fitdistr(na.exclude(dep.var), dists[i])$loglik))
  }
}

