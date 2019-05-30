\name{analyse.calculate.means}
\alias{analyse.calculate.means}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calculate the means for a given set of columns.
}
\description{
This is primarily a helper function that the other analyse.X.Y functions pass to. Can be used if you want to extend eyeTrackR and write new functions.
}
\usage{
analyse.calculate.means(fixreport_df, aggregation_column_list, output_column_expression, final_output_column_expression, spss, dvColumnName, prefixLabel = "", debug = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fixreport_df}{
The incoming fixation report that needs to be summarised into means.
}
  \item{aggregation_column_list}{
The list of columns that will group the fixation report.
}
  \item{output_column_expression}{
The expression to be parsed when calculating the means.
}
  \item{final_output_column_expression}{
The expression to be parsed when calculating the final means.
}
  \item{spss}{
boolean for whether the output should be given in wide format ready for analysis in SPSS.
}
  \item{dvColumnName}{
The column name of the dependent variable to provide means for.
}
  \item{prefixLabel}{
When the output is saved to a textfile for SPSS, the prefixLabel is added to the beginning of the textfile name.
}
  \item{debug}{
Debug mode.
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
Quick note on how means are computed: If you want, for example, to calculate the mean fixation duration, eyeTrackR won't just do a raw computation of all fixation durations and provide you with the result.
Instead, it computes by-trial means first (i.e., one mean fixation duration per trial), then computes the mean across trials. The reason for doing this is to ensure that all trials contribute equally for any final means that are examined.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- No examples, it's a helper function!
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
