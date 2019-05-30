\name{organise.responses.markup}
\alias{organise.responses.markup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ~~Marks up the responses for each trial into the fixation report ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
organise.responses.markup(fixreport_df, correct_answer_column)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fixreport_df}{
%%     ~~Describe \code{fixreport_df} here~~
}
  \item{correct_answer_column}{
%%     ~~Describe \code{correct_answer_column} here~~
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
function (fixreport_df, correct_answer_column) 
{
    fix_DT <- data.table(fixreport_df)
    setkey(fix_DT, RECORDING_SESSION_LABEL, TRIAL_INDEX)
    aggExpr <- paste("list(RECORDING_SESSION_LABEL, TRIAL_INDEX, ", 
        correct_answer_column, ")", sep = "")
    aggExprParsed <- parse(text = aggExpr)
    resp_DT <- fix_DT[CURRENT_FIX_BUTTON_0_PRESS != "." | CURRENT_FIX_BUTTON_1_PRESS != 
        "." | CURRENT_FIX_BUTTON_2_PRESS != "." | CURRENT_FIX_BUTTON_3_PRESS != 
        "." | CURRENT_FIX_BUTTON_4_PRESS != "." | CURRENT_FIX_BUTTON_5_PRESS != 
        "." | CURRENT_FIX_BUTTON_6_PRESS != "." | CURRENT_FIX_BUTTON_7_PRESS != 
        "." | CURRENT_FIX_BUTTON_8_PRESS != ".", list(CURRENT_FIX_BUTTON_0_PRESS = CURRENT_FIX_BUTTON_0_PRESS[1], 
        CURRENT_FIX_BUTTON_1_PRESS = CURRENT_FIX_BUTTON_1_PRESS[1], 
        CURRENT_FIX_BUTTON_2_PRESS = CURRENT_FIX_BUTTON_2_PRESS[1], 
        CURRENT_FIX_BUTTON_3_PRESS = CURRENT_FIX_BUTTON_3_PRESS[1], 
        CURRENT_FIX_BUTTON_4_PRESS = CURRENT_FIX_BUTTON_4_PRESS[1], 
        CURRENT_FIX_BUTTON_5_PRESS = CURRENT_FIX_BUTTON_5_PRESS[1], 
        CURRENT_FIX_BUTTON_6_PRESS = CURRENT_FIX_BUTTON_6_PRESS[1], 
        CURRENT_FIX_BUTTON_7_PRESS = CURRENT_FIX_BUTTON_7_PRESS[1], 
        CURRENT_FIX_BUTTON_8_PRESS = CURRENT_FIX_BUTTON_8_PRESS[1]), 
        eval(aggExprParsed)]
    resp_DT$BUTTON_NUMBER <- NA
    resp_DT$BUTTON_NUMBER[resp_DT$CURRENT_FIX_BUTTON_0_PRESS != 
        "."] <- 0
    resp_DT$BUTTON_NUMBER[resp_DT$CURRENT_FIX_BUTTON_1_PRESS != 
        "."] <- 1
    resp_DT$BUTTON_NUMBER[resp_DT$CURRENT_FIX_BUTTON_2_PRESS != 
        "."] <- 2
    resp_DT$BUTTON_NUMBER[resp_DT$CURRENT_FIX_BUTTON_3_PRESS != 
        "."] <- 3
    resp_DT$BUTTON_NUMBER[resp_DT$CURRENT_FIX_BUTTON_4_PRESS != 
        "."] <- 4
    resp_DT$BUTTON_NUMBER[resp_DT$CURRENT_FIX_BUTTON_5_PRESS != 
        "."] <- 5
    resp_DT$BUTTON_NUMBER[resp_DT$CURRENT_FIX_BUTTON_6_PRESS != 
        "."] <- 6
    resp_DT$BUTTON_NUMBER[resp_DT$CURRENT_FIX_BUTTON_7_PRESS != 
        "."] <- 7
    resp_DT$BUTTON_NUMBER[resp_DT$CURRENT_FIX_BUTTON_8_PRESS != 
        "."] <- 8
    resp_DT$OUTCOME <- NA
    outExpr <- paste("resp_DT$OUTCOME[resp_DT$BUTTON_NUMBER == resp_DT$", 
        correct_answer_column, "] <- 'CORRECT'", sep = "")
    outExprParsed <- parse(text = outExpr)
    eval(outExprParsed)
    outExpr2 <- paste("resp_DT$OUTCOME[resp_DT$BUTTON_NUMBER != resp_DT$", 
        correct_answer_column, "] <- 'INCORRECT'", sep = "")
    outExprParsed2 <- parse(text = outExpr2)
    eval(outExprParsed2)
    resp_DT$RESPONSE_TIME <- NA
    resp_DT$RESPONSE_TIME[resp_DT$CURRENT_FIX_BUTTON_0_PRESS != 
        "."] <- as.numeric(as.character(resp_DT$CURRENT_FIX_BUTTON_0_PRESS[resp_DT$CURRENT_FIX_BUTTON_0_PRESS != 
        "."]))
    resp_DT$RESPONSE_TIME[resp_DT$CURRENT_FIX_BUTTON_1_PRESS != 
        "."] <- as.numeric(as.character(resp_DT$CURRENT_FIX_BUTTON_1_PRESS[resp_DT$CURRENT_FIX_BUTTON_1_PRESS != 
        "."]))
    resp_DT$RESPONSE_TIME[resp_DT$CURRENT_FIX_BUTTON_2_PRESS != 
        "."] <- as.numeric(as.character(resp_DT$CURRENT_FIX_BUTTON_2_PRESS[resp_DT$CURRENT_FIX_BUTTON_2_PRESS != 
        "."]))
    resp_DT$RESPONSE_TIME[resp_DT$CURRENT_FIX_BUTTON_3_PRESS != 
        "."] <- as.numeric(as.character(resp_DT$CURRENT_FIX_BUTTON_3_PRESS[resp_DT$CURRENT_FIX_BUTTON_3_PRESS != 
        "."]))
    resp_DT$RESPONSE_TIME[resp_DT$CURRENT_FIX_BUTTON_4_PRESS != 
        "."] <- as.numeric(as.character(resp_DT$CURRENT_FIX_BUTTON_4_PRESS[resp_DT$CURRENT_FIX_BUTTON_4_PRESS != 
        "."]))
    resp_DT$RESPONSE_TIME[resp_DT$CURRENT_FIX_BUTTON_5_PRESS != 
        "."] <- as.numeric(as.character(resp_DT$CURRENT_FIX_BUTTON_5_PRESS[resp_DT$CURRENT_FIX_BUTTON_5_PRESS != 
        "."]))
    resp_DT$RESPONSE_TIME[resp_DT$CURRENT_FIX_BUTTON_6_PRESS != 
        "."] <- as.numeric(as.character(resp_DT$CURRENT_FIX_BUTTON_6_PRESS[resp_DT$CURRENT_FIX_BUTTON_6_PRESS != 
        "."]))
    resp_DT$RESPONSE_TIME[resp_DT$CURRENT_FIX_BUTTON_7_PRESS != 
        "."] <- as.numeric(as.character(resp_DT$CURRENT_FIX_BUTTON_7_PRESS[resp_DT$CURRENT_FIX_BUTTON_7_PRESS != 
        "."]))
    resp_DT$RESPONSE_TIME[resp_DT$CURRENT_FIX_BUTTON_8_PRESS != 
        "."] <- as.numeric(as.character(resp_DT$CURRENT_FIX_BUTTON_8_PRESS[resp_DT$CURRENT_FIX_BUTTON_8_PRESS != 
        "."]))
    resp_DT$CURRENT_FIX_BUTTON_0_PRESS <- NULL
    resp_DT$CURRENT_FIX_BUTTON_1_PRESS <- NULL
    resp_DT$CURRENT_FIX_BUTTON_2_PRESS <- NULL
    resp_DT$CURRENT_FIX_BUTTON_3_PRESS <- NULL
    resp_DT$CURRENT_FIX_BUTTON_4_PRESS <- NULL
    resp_DT$CURRENT_FIX_BUTTON_5_PRESS <- NULL
    resp_DT$CURRENT_FIX_BUTTON_6_PRESS <- NULL
    resp_DT$CURRENT_FIX_BUTTON_7_PRESS <- NULL
    resp_DT$CURRENT_FIX_BUTTON_8_PRESS <- NULL
    joined_df <- join(data.frame(fix_DT), data.frame(resp_DT))
    return(joined_df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
