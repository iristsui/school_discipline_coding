\name{CreateTable3}
\alias{CreateTable3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CreateTable3(x1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{
%%     ~~Describe \code{x1} here~~
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
function(x1) {
  # Args:
  #   x: Data frame
  years <- c("2004", "2005", "2006", "2007")
  table.3 <- vector("list", 5)
  table.3[[1]] <- c("EBS Schoolwide Discipline System",  # Names table rows
                    "EBS Nonclassroom Setting",
                    "EBS Classroom Setting",
                    "EBS Individual Student System",
                    "EBS Implementation Average",
                    "Percent of Schools Meeting EBS Criterion 8080",
                    "SET Expectations Defined",
                    "SET Expectations Taught",
                    "SET Reward System",
                    "SET Violation System",
                    "SET Monitoring and Evaluation",
                    "SET Leadership",
                    "SET District Support",
                    "SET Implementation Average",
                    "Percent of Schools Meeting SET Criterion 8080")

  for (i in 1:length(years)) {
    # Loops through 2004 to the last year and makes table.3."year" object
    #   for each year
    x <- subset(x1, year == i)
    school.denom <- with(x, length(unique(school_educationalorgid[year==i])))
    table.3[[i+1]] <- list(mean(x$perc.in.place.sw.disc.sys, na.rm=TRUE),
                           mean(x$perc.in.place.ncr.set, na.rm=TRUE),
                           mean(x$perc.in.place.cr.set, na.rm=TRUE),
                           mean(x$perc.in.place.ind.stud.sys, na.rm=TRUE),
                           mean(x$ebs.implementationaverage, na.rm=TRUE),
                           sum(x$ebs.criterion8080, na.rm=TRUE) / school.denom,
                           mean(x$set_expectationsdefined, na.rm=TRUE),
                           mean(x$set_expectationstaught, na.rm=TRUE),
                           mean(x$set_rewardsystem, na.rm=TRUE),
                           mean(x$set_violationssystem, na.rm=TRUE),
                           mean(x$set_monitoringevaluation, na.rm=TRUE),
                           mean(x$set_leadership, na.rm=TRUE),
                           mean(x$set_districtsupport, na.rm=TRUE),
                           mean(x$set_implementationaverage, na.rm=TRUE),
                           sum(x$set_criterion8080, na.rm=TRUE) / school.denom)
  }
  do.call("cbind", table.3)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
