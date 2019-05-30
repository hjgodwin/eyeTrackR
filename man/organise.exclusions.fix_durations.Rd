\name{organise.exclusions.fix_durations}
\alias{organise.exclusions.fix_durations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Organise - Fixation Duration Exclusions
}
\description{
Removes fixation duration outliers.
}
\usage{
organise.exclusions.fix_durations(fixreport_df, min = 60, max = 1200)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fixreport_df}{
Fixation report.
}
  \item{min}{
Minimum fixation duration.
}
  \item{max}{
Maximum fixation duration.
}
}
\details{
Removes fixation duration outliers and provides statistics on the removals in a text file in the current working directory ("fix_duration_removals.txt").
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
