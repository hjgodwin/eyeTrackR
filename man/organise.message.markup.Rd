\name{organise.message.markup}
\alias{organise.message.markup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Organise - Markup Specific Message
}
\description{
Marks up an event message in a fixation report.
}
\usage{
organise.message.markup(message_df, fixreport_df, message, show_working = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{message_df}{
Message report data frame.
}
  \item{fixreport_df}{
Fixation report data frame.
}
  \item{message}{
The message event text (e.g., "SYNCTIME", "DISPLAY_IMAGES", "TIMER", "RESPONSE") and so on. Only enter one message. If you want to use multiple messages, 
run this function once for each message.
}
  \item{show_working}{
Show debug info (TRUE/FALSE). Optional parameter, defaults to FALSE.
}
}
\details{
Will inform you if the output and input rows differ. If they do, then you likely have a problem with missing data.
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
Hayward Godwin
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- TBA
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
