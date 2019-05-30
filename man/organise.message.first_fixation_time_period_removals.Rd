\name{organise.message.first_fixation_time_period_removals}
\alias{organise.message.first_fixation_time_period_removals}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
First fixation in time period removals.
}
\description{
Removes trials where the first fixation in a time period was too far from the centre. This is used when participants move away from the centre of the display too soon in a trial.
Uses 2.5 degrees of visual angle as the default. Reports on how many trials were removed as a result of this.
}
\usage{
organise.message.first_fixation_time_period_removals(fixreport_df, time_period, distance_limit = 2.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fixreport_df}{
Input fixation report.
}
  \item{time_period}{
Time period to focus on.
}
  \item{distance_limit}{
The distance limit (in degrees of visual angle).
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
