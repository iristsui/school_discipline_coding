\name{CreateTable1}
\alias{CreateTable1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CreateTable1(x1)
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
  table.1 <- vector("list", 5)
  table.1[[1]] <- c("Number of Total Schools",  # Names table rows
                    "Number of Middle Schools",
                    "Number of High Schools",
                    "Number of Mixed Grade Schools",
                    "Number of CO schools",
                    "Number of IL schools",
                    "Number of MD schools",
                    "Number of MT schools",
                    "Number of OR schools",
                    "Number of Schools in City Setting",
                    "Number of Schools in Suburban/Fringe Setting",
                    "Number of Schools in Town Setting",
                    "Number of Schools in Rural Setting",
                    "Number of School Districts",
                    "Average Enrollment",
                    "Percent Male Enrollment",
                    "Percent African American Enrollment",
                    "Percent American Indian/Alaska Native Enrollment",
                    "Percent Asian American Enrollment",
                    "Percent Latino Enrollment",
                    "Percent Caucasian Enrollment",
                    "Percent Free and Reduced Priced Meals",
                    "Average Number of FTE per 100 Students")

  for (i in 1:length(years)) {
    # Loops through 2004 to the last year and makes table.1."year" object
    #   for each year
    x <- subset(x1, year == i)
    table.1[[i+1]] <- list(with(x, length(unique(school_educationalorgid))),
                           with(x, length(unique(school_educationalorgid[grade.type == "MS"]))),
                           with(x, length(unique(school_educationalorgid[grade.type == "HS"]))),
                           with(x, length(unique(school_educationalorgid[grade.type == "Mix"]))),
                           with(x, length(unique(school_educationalorgid[school.statelabel.x == "CO"]))),
                           with(x, length(unique(school_educationalorgid[school.statelabel.x == "IL"]))),
                           with(x, length(unique(school_educationalorgid[school.statelabel.x == "MD"]))),
                           with(x, length(unique(school_educationalorgid[school.statelabel.x == "MT"]))),
                           with(x, length(unique(school_educationalorgid[school.statelabel.x == "OR"]))),
                           with(x, length(unique(school_educationalorgid[urbanicity == "City"]))),
                           with(x, length(unique(school_educationalorgid[urbanicity == "Suburb/Fringe"]))),
                           with(x, length(unique(school_educationalorgid[urbanicity == "Town"]))),
                           with(x, length(unique(school_educationalorgid[urbanicity == "Rural"]))),
                           with(x, length(unique(school_districtid))),
                           with(x, mean(nces_member,     na.rm=TRUE)),
                           with(x, mean(male.prop,       na.rm=TRUE) * 100),
                           with(x, mean(afram.prop,      na.rm=TRUE) * 100),
                           with(x, mean(aian.prop,       na.rm=TRUE) * 100),
                           with(x, mean(asian.prop,      na.rm=TRUE) * 100),
                           with(x, mean(latino.prop,     na.rm=TRUE) * 100),
                           with(x, mean(white.prop,      na.rm=TRUE) * 100),
                           with(x, mean(frpm.prop,       na.rm=TRUE)),
                           with(x, mean(fte.100.student, na.rm=TRUE)))
  }
  do.call("cbind", table.1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
