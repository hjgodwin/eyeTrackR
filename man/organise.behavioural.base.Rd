\name{organise.behavioural.base}
\alias{organise.behavioural.base}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 ~~ Organise Behavioural Results ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
organise.behavioural.base(fixreport_df, grouping_column_list, response_period_start = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fixreport_df}{
%%     ~~Describe \code{fixreport_df} here~~
}
  \item{grouping_column_list}{
%%     ~~Describe \code{grouping_column_list} here~~
}
  \item{response_period_start}{
%%     ~~Describe \code{response_period_start} here~~
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
function (fixreport_df, grouping_column_list, response_period_start = "") 
{
    fix_DT <- data.table(fixreport_df)
    setkey(fix_DT, RECORDING_SESSION_LABEL, TRIAL_INDEX)
    aggExpr <- "list(RECORDING_SESSION_LABEL, TRIAL_INDEX "
    if (length(grouping_column_list) > 0) {
        for (i in seq(1:length(grouping_column_list))) {
            aggExpr <- paste(aggExpr, ", ", grouping_column_list[i], 
                sep = "")
        }
    }
    aggExpr <- paste(aggExpr, ")", sep = "")
    aggExprParsed <- parse(text = aggExpr)
    if (response_period_start == "") {
        b_DT <- fix_DT[, list(RESPONSE_TIME = RESPONSE_TIME[1], 
            OUTCOME = OUTCOME[1]), eval(aggExprParsed)]
    }
    if (response_period_start != "") {
        rtExpr <- "list('RESPONSE_TIME' = RESPONSE_TIME[1] - "
        rtExpr <- paste(rtExpr, response_period_start, "[1],", 
            sep = "")
        rtExpr <- paste(rtExpr, "'OUTCOME' = OUTCOME[1])", sep = "")
        rtExprParsed <- parse(text = rtExpr)
        b_DT <- fix_DT[, eval(rtExprParsed), eval(aggExprParsed)]
    }
    print(b_DT)
    write.table(data.frame(b_DT), "behavioural_data.txt", row.names = FALSE)
    return(b_DT)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
