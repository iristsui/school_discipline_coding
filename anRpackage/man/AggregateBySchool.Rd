\name{AggregateBySchool}
\alias{AggregateBySchool}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AggregateBySchool(x, y, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function(x, y, ...) {
  # Produces a dataset of x concatenated over the three year period and merges
  # it with the school data in long format.
  # Args:
  #   x: data frame where the aggregated variable-to-be lives
  #   y: new variable name for aggregated data in x
  m <- x
  year1 <- rep(200405, length(unique(m$school_educationalorgid)))
  year2 <- rep(200506, length(unique(m$school_educationalorgid)))
  year3 <- rep(200607, length(unique(m$school_educationalorgid)))
  #year4 <- rep(200708, length(unique(m$school_educationalorgid)))

  x1 <- cbind(year1,
              table(m$school_educationalorgid[m$school_schoolyear == 200405]))
  x2 <- cbind(year2,
              table(m$school_educationalorgid[m$school_schoolyear == 200506]))
  x3 <- cbind(year3,
              table(m$school_educationalorgid[m$school_schoolyear == 200607]))
  #x4 <- cbind(year4,
  #            table(m$school_educationalorgid[m$school_schoolyear == 200708]))
  xbind <- rbind(x1, x2, x3)
  #rm(year1, year2, year3, x1, x2, x3)
  colnames(xbind) <- c("year1", "y")
  y <- merge(a, xbind,
             by.x=c("school_educationalorgid", "school_schoolyear"),
             by.y=c("row.names", "year1"), all=TRUE) 
  # ebs.by.school has an ebs survey count variable per school per year
  #rm(xbind)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
