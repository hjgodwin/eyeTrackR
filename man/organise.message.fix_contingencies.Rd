\name{organise.message.fix_contingencies}
\alias{organise.message.fix_contingencies}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 ~~ Message contingencies ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
organise.message.fix_contingencies(fixreport_df, ordered_message_list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fixreport_df}{
%%     ~~Describe \code{fixreport_df} here~~
}
  \item{ordered_message_list}{
%%     ~~Describe \code{ordered_message_list} here~~
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
function (fixreport_df, ordered_message_list) 
{
    for (i in 1:length(ordered_message_list)) {
        if (i == 1) {
            eventBasePreExpr <- paste("fixreport_df$WITHIN_PRE_", 
                ordered_message_list[i], "<-'FALSE'", sep = "")
            eventBasePreParsed <- parse(text = eventBasePreExpr)
            eval(eventBasePreParsed)
            preExpr <- paste("fixreport_df$WITHIN_PRE_", ordered_message_list[i], 
                "[(fixreport_df$CURRENT_FIX_START <= ", "fixreport_df$", 
                ordered_message_list[i], ") |", "(fixreport_df$CURRENT_FIX_END <= ", 
                "fixreport_df$", ordered_message_list[i], ")", 
                "]<- TRUE", sep = "")
            preExprParsed <- parse(text = preExpr)
            eval(preExprParsed)
        }
        eventBaseExpr <- paste("fixreport_df$WITHIN_", ordered_message_list[i], 
            "<-'FALSE'", sep = "")
        eventBaseParsed <- parse(text = eventBaseExpr)
        eval(eventBaseParsed)
        print(ordered_message_list[i])
        if (i < length(ordered_message_list)) {
            colExpr <- paste("fixreport_df$WITHIN_", ordered_message_list[i], 
                "[(fixreport_df$CURRENT_FIX_START >= ", "fixreport_df$", 
                ordered_message_list[i], " & fixreport_df$CURRENT_FIX_START <= ", 
                "fixreport_df$", ordered_message_list[i + 1], 
                ") | ", "(fixreport_df$CURRENT_FIX_END >= ", 
                "fixreport_df$", ordered_message_list[i], " & fixreport_df$CURRENT_FIX_END <= ", 
                "fixreport_df$", ordered_message_list[i + 1], 
                ")", " | ", "(fixreport_df$CURRENT_FIX_START < ", 
                "fixreport_df$", ordered_message_list[i], " & ", 
                "fixreport_df$CURRENT_FIX_END > ", "fixreport_df$", 
                ordered_message_list[i + 1], ")", "]<- TRUE", 
                sep = "")
        }
        if (i == length(ordered_message_list)) {
            colExpr <- paste("fixreport_df$WITHIN_", ordered_message_list[i], 
                "[(fixreport_df$CURRENT_FIX_START >= ", "fixreport_df$", 
                ordered_message_list[i], ") |", "(fixreport_df$CURRENT_FIX_END >= ", 
                "fixreport_df$", ordered_message_list[i], ")", 
                "]<- TRUE", sep = "")
        }
        colExprParsed <- parse(text = colExpr)
        eval(colExprParsed)
    }
    fixreport_df$FIXATION_CONTINGENCY <- ""
    ordered_message_list <- c(paste("PRE_", ordered_message_list[1], 
        sep = ""), ordered_message_list)
    for (i in seq(1:length(ordered_message_list))) {
        comparisonExpr <- paste("fixreport_df$FIXATION_CONTINGENCY[fixreport_df$WITHIN_", 
            ordered_message_list[i], " == TRUE & fixreport_df$FIXATION_CONTINGENCY!=''] ", 
            " <- ", "paste(", "fixreport_df$FIXATION_CONTINGENCY[fixreport_df$WITHIN_", 
            ordered_message_list[i], " == TRUE & fixreport_df$FIXATION_CONTINGENCY!=''], ", 
            "'__", ordered_message_list[i], "', ", "sep='')", 
            sep = "")
        comaprisonExprParsed <- parse(text = comparisonExpr)
        eval(comaprisonExprParsed)
        comparisonExpr <- paste("fixreport_df$FIXATION_CONTINGENCY[fixreport_df$WITHIN_", 
            ordered_message_list[i], " == TRUE & fixreport_df$FIXATION_CONTINGENCY==''] ", 
            " <- ", "paste(", "fixreport_df$FIXATION_CONTINGENCY[fixreport_df$WITHIN_", 
            ordered_message_list[i], " == TRUE & fixreport_df$FIXATION_CONTINGENCY==''], ", 
            "'", ordered_message_list[i], "', ", "sep='')", sep = "")
        comaprisonExprParsed <- parse(text = comparisonExpr)
        eval(comaprisonExprParsed)
    }
    fixreport_df$FIXATION_CONTINGENCY[fixreport_df$FIXATION_CONTINGENCY == 
        ""] <- "UNCLASSIFIED"
    return(fixreport_df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
