\name{organise.contingencies.descriptives}
\alias{organise.contingencies.descriptives}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 ~~ Descriptives for Fixation Contingencies ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
organise.contingencies.descriptives(fixreport_df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fixreport_df}{
%%     ~~Describe \code{fixreport_df} here~~
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
function (fixreport_df) 
{
    fDT <- data.table(fixreport_df)
    setkey(fDT, RECORDING_SESSION_LABEL, TRIAL_INDEX)
    summary_DT <- fDT[, list(FIXATION_COUNT = length(TRIAL_INDEX)), 
        list(FIXATION_CONTINGENCY)]
    return(data.frame(summary_DT))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
