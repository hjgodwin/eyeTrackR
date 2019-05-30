\name{organise.message.removals}
\alias{organise.message.removals}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Organise - Message Removals
}
\description{
Remove trials where a list of message events did not occur or was not detected.
Provides statistics on how many trials were removed for each participants in a text file "event_trials_missing.txt".
}
\usage{
organise.message.removals(fixreport_df, required_message_list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fixreport_df}{
Input fixation report.
}
  \item{required_message_list}{
List of messages that should occur in order for a trial to be included.
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
