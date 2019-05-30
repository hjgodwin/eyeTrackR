\name{analyse.fix.duration}
\alias{analyse.fix.duration}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Analyse - Calculate Mean Fixation Durations
}
\description{
Gives the mean fixation durations according to a set of grouping columns.
}
\usage{
analyse.fix.duration(fixreport_df, aggregation_column_list, spss = FALSE, prefixLabel = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fixreport_df}{
Fixation report to analyse.
}
  \item{aggregation_column_list}{
Columns to group / aggregate by. Can add as many as you want.
}
  \item{spss}{
Set to TRUE or FALSE. Default is FALSE. If set to TRUE, saves the means calculated to a textfile in wide format for analysis in SPSS.
}
  \item{prefixLabel}{
Default is blank. Only used when spss is set to TRUE. The textfile that is output for SPSS is given a prefix label set to prefixLabel.
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
