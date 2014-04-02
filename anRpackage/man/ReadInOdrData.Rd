\name{ReadInOdrData}
\alias{ReadInOdrData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ReadInOdrData()
}
%- maybe also 'usage' for other objects documented here.
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
function() {
  odr <- vector("list", 4)
  odr[[1]] <- ReadCsvData("SAS-ODRData_200405to200607.csv") 
  odr[[2]] <- ReadCsvData("SAS-ODRData_200405to200708.csv")
  odr[[3]] <- ReadCsvData("SET-ODRData_200405to200607.csv")
  odr[[4]] <- ReadCsvData("SET-ODRData_200405to200708.csv")
  odr <- lapply(odr, function(x) return(x[, 1:24]))
  return(unique(do.call("rbind", odr)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
