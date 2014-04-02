\name{PrintCors}
\alias{PrintCors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PrintCors(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
