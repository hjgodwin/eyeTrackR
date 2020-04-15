
# 
# # TESTING CODE IS HERE FOR LATER REMOVAL 
# 
# # LOAD IA REPORT FOR TESTING
# iareport <- fread(file.choose())
# 
# # # NOW THE FUNCTIONS ARE SET UP, RUN THIS ONE
# # # CHANGE TRIAL INDEX TO SOMETHING THAT YOU WANT LIKE IF YOU WANT THE FIRST TRIAL INDEX ONLY WRITE TRIAL_INDEX
# iaDT <- iareport
# iaDT <- organise.reading.calculateRemovals(iaDT, source_column_name = 'IA_FIRST_FIXATION_DURATION', output_column_name = 'FFD',
#                                            filterBy = ' IA_FIRST_FIX_PROGRESSIVE == 1', sdLimit = 3,
#                                            aggregation_column_list=list('Participant')) # THIS IS DOING BY-PARTIIPANT OUTLIERS, CHANGE TO
# 


globalVariables(c('IA_INDEX','IA_LABEL','image_blank','randomColor','image_annotate','image_write','iaDT'))

##############################################################################################################################################################
# CALCULATE REMOVALS - GENERIC FUNCTION
#' Generic function for calculating outliers in a reading study.
#'
#' @param ia_df Interest area data.frame
#' @param source_column_name The source column name for the outlier calculation.
#' @param output_column_name The output column name.
#' @param filterBy Which columns to filter by. This can be formatted as a 'WHERE' statement in data.table language.
#' @param sdLimit The SD limit - how many times the SD is the upper and lower limit? (Default is 2.5)
#' @param aggregation_column_list The columns to group by - can usualy be the factors in your study, or in some cases, the participants themselves.
#'
#' @return An updated interest area data.table with additional columns marked up. There will be a 'KEEP' column where 1 says it's safe to keep and 0 says to remove.
#'
#' @examples
organise.reading.calculateRemovals <- function(ia_df, source_column_name, output_column_name, filterBy = '', sdLimit = 2.5, 
                                               aggregation_column_list){
  
  # MAKE SURE ITS A DATA.TABLE
  ia_df <- data.table(ia_df)
  
  # MEANS FIRST ##############################################################################################################################
  
  # REMOVE FULL STOPS AND CONVERT THEM TO NAs
  eval(parse(text=paste("ia_df[,", eval(source_column_name), ":=as.numeric(as.character(",eval(source_column_name),")),]", sep='')))
  
  # CALCULATE MEANS GROUPED BY aggregation_column_list
  eval(parse(text=paste("ia_df[", eval(filterBy), ",", eval(output_column_name), ":=mean(", eval(source_column_name), " [is.na(", eval(source_column_name), ")==F]),", eval(aggregation_column_list), "]", sep='')))
  
  # SDS NEXT ##############################################################################################################################
  
  # CALCULATE SD GROUPED BY aggregation_column_list
  sd_column_name <- paste(output_column_name, '_SD', sep="")
  eval(parse(text=paste("ia_df[", eval(filterBy), ",", eval(sd_column_name), ":=sd(", eval(source_column_name), " [is.na(", eval(source_column_name), ")==F]),", eval(aggregation_column_list), "]", sep='')))
  
  # CALCULATE UPPER AND LOWER SD BOUNDS
  upper_column_name <- paste(sd_column_name, '_UPPER', sep="")
  lower_column_name <- paste(sd_column_name, '_LOWER', sep="")
  
  eval(parse(text=paste("ia_df[", eval(filterBy), ",", eval(upper_column_name), ":=", eval(output_column_name), ' + (',  eval(sdLimit)," * ", eval(sd_column_name), "),", eval(aggregation_column_list), "]", sep='')))
  eval(parse(text=paste("ia_df[", eval(filterBy), ",", eval(lower_column_name), ":=", eval(output_column_name), ' - (',  eval(sdLimit)," * ", eval(sd_column_name), "),", eval(aggregation_column_list), "]", sep='')))
  
  # SET WHICH ONES TO KEEP
  keeper_column_name <- paste(output_column_name, '_KEEP', sep='')
  
  eval(parse(text=paste("ia_df[,", eval(keeper_column_name), ":=0,]", sep="")))
  
  # EXPAND FILTER BY TO FIT IN WITH CODE
  if (nchar(filterBy)>0){filterBy <- paste(filterBy, ' & ', sep="")}
  
  eval(parse(text=paste("ia_df[", eval(filterBy), "is.na(", eval(source_column_name), ")==F & 
                                (", eval(source_column_name), " > ", eval(lower_column_name), " | 
                                  ", eval(source_column_name), " < ", eval(upper_column_name), "), ",
                        eval(keeper_column_name), ":=1,]", sep="")))
  
  
  return(ia_df)
}


##############################################################################################################################################################
# CALCULATE REMOVALS - SPECFIC FUNCTIONS



#' Reading study - determine total reading time outliers.
#'
#' @param ia_df Interest area data.frame
#' @param sdLimit The SD limit - how many times the SD is the upper and lower limit? (Default is 2.5)
#' @param aggregation_column_list The columns to group by - can usualy be the factors in your study, or in some cases, the participants themselves.
#'
#' @return An updated interest area data.table with additional columns marked up. There will be a 'KEEP' column where 1 says it's safe to keep and 0 says to remove.
#' 
#' @examples
organise.reading.totalReadingTime <- function(ia_df, sdLimit=2.5, aggregation_column_list){
  
  ia_df <- organise.reading.calculateRemovals(ia_df, source_column_name = 'IA_DWELL_TIME', output_column_name = 'TOTAL_READING_TIME',
                                     filterBy = '', sdLimit = sdLimit, aggregation_column_list = aggregation_column_list)
  
  return(ia_df)
}

#' Reading study - determine first fixation time outliers.
#'
#' @param ia_df Interest area data.frame
#' @param sdLimit The SD limit - how many times the SD is the upper and lower limit? (Default is 2.5)
#' @param aggregation_column_list The columns to group by - can usualy be the factors in your study, or in some cases, the participants themselves.
#'
#' @return An updated interest area data.table with additional columns marked up. There will be a 'KEEP' column where 1 says it's safe to keep and 0 says to remove.
#' 
#' @examples
organise.reading.firstFixationDuration <- function(ia_df, sdLimit=2.5, aggregation_column_list){
  
  ia_df <- organise.reading.calculateRemovals(ia_df, source_column_name = 'IA_FIRST_FIXATION_DURATION', output_column_name = 'FIRST_FIXATION_DURATION',
                                     filterBy = 'IA_FIRST_FIX_PROGRESSIVE == 1', sdLimit = sdLimit, aggregation_column_list = aggregation_column_list)
  
  return(ia_df)
}

#' Reading study - determine skipping outliers.
#'
#' @param ia_df Interest area data.frame
#' @param sdLimit The SD limit - how many times the SD is the upper and lower limit? (Default is 2.5)
#' @param aggregation_column_list The columns to group by - can usualy be the factors in your study, or in some cases, the participants themselves.
#'
#' @return An updated interest area data.table with additional columns marked up. There will be a 'KEEP' column where 1 says it's safe to keep and 0 says to remove.
#' 
#' @examples
organise.reading.Skipping <- function(ia_df, sdLimit=2.5, aggregation_column_list){
  
  ia_df <- organise.reading.calculateRemovals(ia_df, source_column_name = 'IA_SKIP', output_column_name = 'SKIP',
                                     filterBy = '', sdLimit = sdLimit, aggregation_column_list = aggregation_column_list)
  
  return(ia_df)
}

#' Reading study - determine outliers for the number of regressions into an Interest Area.
#'
#' @param ia_df Interest area data.frame
#' @param sdLimit The SD limit - how many times the SD is the upper and lower limit? (Default is 2.5)
#' @param aggregation_column_list The columns to group by - can usualy be the factors in your study, or in some cases, the participants themselves.
#'
#' @return An updated interest area data.table with additional columns marked up. There will be a 'KEEP' column where 1 says it's safe to keep and 0 says to remove.
#' 
#' @examples
organise.reading.RegressionsIn <- function(ia_df, sdLimit=2.5, aggregation_column_list){
  
  ia_df <- organise.reading.calculateRemovals(iaDT, source_column_name = 'IA_REGRESSION_IN_COUNT', output_column_name = 'REGRESSIONS_IN',
                                     filterBy = '', sdLimit = sdLimit, 
                                     aggregation_column_list = aggregation_column_list)
  
  return(ia_df)
}

#' Reading study - determine outliers for the number of regressions out of an Interest Area.
#'
#' @param ia_df Interest area data.frame
#' @param sdLimit The SD limit - how many times the SD is the upper and lower limit? (Default is 2.5)
#' @param aggregation_column_list The columns to group by - can usualy be the factors in your study, or in some cases, the participants themselves.
#'
#' @return An updated interest area data.table with additional columns marked up. There will be a 'KEEP' column where 1 says it's safe to keep and 0 says to remove.
#' 
#' @examples
organise.reading.RegressionsOut <- function(ia_df, sdLimit=2.5, aggregation_column_list){
  
  ia_df <- organise.reading.calculateRemovals(iaDT, source_column_name = 'IA_REGRESSION_OUT_COUNT', output_column_name = 'REGRESSIONS_OUT',
                                     filterBy = '', sdLimit = sdLimit, 
                                     aggregation_column_list = aggregation_column_list)
  
  return(ia_df)
}

#' Reading study - determine outliers for saccade amplitudes.
#'
#' @param ia_df Interest area data.frame
#' @param sdLimit The SD limit - how many times the SD is the upper and lower limit? (Default is 2.5)
#' @param aggregation_column_list The columns to group by - can usualy be the factors in your study, or in some cases, the participants themselves.
#'
#' @return An updated interest area data.table with additional columns marked up. There will be a 'KEEP' column where 1 says it's safe to keep and 0 says to remove.
#' 
#' @examples
organise.reading.SaccadeAmplitude <- function(ia_df, sdLimit=2.5, aggregation_column_list){
  
  ia_df <- organise.reading.calculateRemovals(iaDT, source_column_name = 'IA_FIRST_SACCADE_AMPLITUDE', output_column_name = 'FIRST_SACCADE_AMPLITUDE',
                                     filterBy = '', sdLimit = sdLimit, aggregation_column_list = aggregation_column_list)
  
  return(ia_df)
}

#' Reading study - determine outliers for gaze durations.
#'
#' @param ia_df Interest area data.frame
#' @param sdLimit The SD limit - how many times the SD is the upper and lower limit? (Default is 2.5)
#' @param aggregation_column_list The columns to group by - can usualy be the factors in your study, or in some cases, the participants themselves.
#'
#' @return An updated interest area data.table with additional columns marked up. There will be a 'KEEP' column where 1 says it's safe to keep and 0 says to remove.
#' 
#' @examples
organise.reading.GazeDuration <- function(ia_df, sdLimit=2.5, aggregation_column_list){
  
  ia_df <- organise.reading.calculateRemovals(iaDT, source_column_name = 'IA_FIRST_RUN_DWELL_TIME', output_column_name = 'GAZE_DURATION',
                                     filterBy = 'IA_FIRST_FIX_PROGRESSIVE == 1', sdLimit = sdLimit, 
                                     aggregation_column_list = aggregation_column_list)
  
  return(ia_df)
}

#' Reading study - determine outliers for go-past times.
#'
#' @param ia_df Interest area data.frame
#' @param sdLimit The SD limit - how many times the SD is the upper and lower limit? (Default is 2.5)
#' @param aggregation_column_list The columns to group by - can usualy be the factors in your study, or in some cases, the participants themselves.
#'
#' @return An updated interest area data.table with additional columns marked up. There will be a 'KEEP' column where 1 says it's safe to keep and 0 says to remove.
#' 
#' @examples
organise.reading.GoPast <- function(ia_df, sdLimit=2.5, aggregation_column_list){
  
  ia_df <- organise.reading.calculateRemovals(iaDT, source_column_name = 'IA_SELECTIVE_REGRESSION_PATH_DURATION', output_column_name = 'GO_PAST_TIME',
                                     filterBy = 'IA_FIRST_FIX_PROGRESSIVE == 1', sdLimit = sdLimit, 
                                     aggregation_column_list = aggregation_column_list)
  
  return(ia_df)
}

#' Reading study - determine outliers for single fixation durations.
#'
#' @param ia_df Interest area data.frame
#' @param sdLimit The SD limit - how many times the SD is the upper and lower limit? (Default is 2.5)
#' @param aggregation_column_list The columns to group by - can usualy be the factors in your study, or in some cases, the participants themselves.
#'
#' @return An updated interest area data.table with additional columns marked up. There will be a 'KEEP' column where 1 says it's safe to keep and 0 says to remove.
#' 
#' @examples
organise.reading.SingleFixationDuration <- function(ia_df, sdLimit=2.5, aggregation_column_list){
  
  ia_df <- organise.reading.calculateRemovals(iaDT, source_column_name = 'IA_FIRST_FIXATION_DURATION', output_column_name = 'SINGLE_FIXATION_DURATION',
                                     filterBy = 'IA_FIRST_RUN_FIXATION_COUNT == 1'& 'IA_FIRST_FIX_PROGRESSIVE == 1', 
                                     sdLimit = sdLimit, aggregation_column_list = aggregation_column_list)
  
  return(ia_df)
}

