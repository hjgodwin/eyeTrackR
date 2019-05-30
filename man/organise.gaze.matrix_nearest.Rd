\name{organise.gaze.matrix_nearest}
\alias{organise.gaze.matrix_nearest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ~~ Organise Nearest Gaze Matrix ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
organise.gaze.matrix_nearest(fixreport_df, distance_max = 2.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fixreport_df}{
%%     ~~Describe \code{fixreport_df} here~~
}
  \item{distance_max}{
%%     ~~Describe \code{distance_max} here~~
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
function (fixreport_df, distance_max = 2.5) 
{
    fixreport_df$RECORDING_SESSION_LABEL <- as.character(fixreport_df$RECORDING_SESSION_LABEL)
    fixreport_df$TRIAL_INDEX <- as.numeric(as.character(fixreport_df$TRIAL_INDEX))
    fixreport_df$CURRENT_FIX_INTEREST_AREA_LABEL_BACKUP <- fixreport_df$CURRENT_FIX_INTEREST_AREA_LABEL
    fixreport_df$CURRENT_FIX_INTEREST_AREA_LABEL <- as.character(fixreport_df$CURRENT_FIX_NEAREST_INTEREST_AREA_LABEL)
    fixreport_df$CURRENT_FIX_NEAREST_INTEREST_AREA_DISTANCE <- as.numeric(as.character(fixreport_df$CURRENT_FIX_NEAREST_INTEREST_AREA_DISTANCE))
    fixreport_df$CURRENT_FIX_INTEREST_AREA_LABEL[fixreport_df$CURRENT_FIX_NEAREST_INTEREST_AREA_DISTANCE >= 
        2.5] <- "."
    fixreport_df <- data.table(fixreport_df)
    setkey(fixreport_df, RECORDING_SESSION_LABEL, TRIAL_INDEX)
    ppt_list <- unique(fixreport_df$RECORDING_SESSION_LABEL)
    ppt_df <- do.call("rbind", lapply(ppt_list, function(ppt) {
        print(ppt)
        trial_list <- unique(fixreport_df$TRIAL_INDEX[fixreport_df$RECORDING_SESSION_LABEL == 
            ppt])
        trial_df <- do.call("rbind", lapply(trial_list, function(trial) {
            temp_df <- fixreport_df[fixreport_df$RECORDING_SESSION_LABEL == 
                ppt & fixreport_df$TRIAL_INDEX == trial, ]
            temp_df$GAZE_INDEX <- NA
            RUNNING_GAZE_INDEX <- 0
            running_IA <- "nothing"
            visited_matrix <- data.frame(IA_NAME = unique(temp_df$CURRENT_FIX_INTEREST_AREA_LABEL), 
                RUNNING_GAZE_INDEX = rep(0, length(unique(temp_df$CURRENT_FIX_INTEREST_AREA_LABEL))))
            fix_index_list <- unique(temp_df$CURRENT_FIX_INDEX)
            for (i in seq(1:length(fix_index_list))) {
                if (temp_df$CURRENT_FIX_INTEREST_AREA_LABEL[temp_df$CURRENT_FIX_INDEX == 
                  fix_index_list[i]] != running_IA) {
                  running_IA <- temp_df$CURRENT_FIX_INTEREST_AREA_LABEL[temp_df$CURRENT_FIX_INDEX == 
                    fix_index_list[i]]
                  if (running_IA != ".") {
                    visited_matrix$RUNNING_GAZE_INDEX[visited_matrix$IA_NAME == 
                      running_IA] <- visited_matrix$RUNNING_GAZE_INDEX[visited_matrix$IA_NAME == 
                      running_IA] + 1
                    temp_df$GAZE_INDEX[temp_df$CURRENT_FIX_INDEX == 
                      fix_index_list[i]] <- visited_matrix$RUNNING_GAZE_INDEX[visited_matrix$IA_NAME == 
                      running_IA]
                    RUNNING_GAZE_INDEX = RUNNING_GAZE_INDEX + 
                      1
                    temp_df$RUNNING_GAZE_INDEX[temp_df$CURRENT_FIX_INDEX == 
                      fix_index_list[i]] = RUNNING_GAZE_INDEX
                  }
                  if (running_IA == ".") {
                    temp_df$RUNNING_GAZE_INDEX[temp_df$CURRENT_FIX_INDEX == 
                      fix_index_list[i]] = NA
                  }
                }
                if (temp_df$CURRENT_FIX_INTEREST_AREA_LABEL[temp_df$CURRENT_FIX_INDEX == 
                  fix_index_list[i]] == running_IA) {
                  if (running_IA != ".") {
                    temp_df$GAZE_INDEX[temp_df$CURRENT_FIX_INDEX == 
                      fix_index_list[i]] <- visited_matrix$RUNNING_GAZE_INDEX[visited_matrix$IA_NAME == 
                      running_IA]
                    temp_df$RUNNING_GAZE_INDEX[temp_df$CURRENT_FIX_INDEX == 
                      fix_index_list[i]] = RUNNING_GAZE_INDEX
                  }
                  if (running_IA == ".") {
                    temp_df$RUNNING_GAZE_INDEX[temp_df$CURRENT_FIX_INDEX == 
                      fix_index_list[i]] = NA
                  }
                }
            }
            return(temp_df)
        }))
        return(trial_df)
    }))
    return(data.frame(ppt_df))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
