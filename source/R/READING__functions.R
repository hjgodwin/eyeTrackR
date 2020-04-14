

library(eyeTrackR)
library(ggplot2)

# SET WORKING DIRECTORY TO THE SAME ONE THIS SCRIPT IS IN
# REPLACE iareport.txt with an interest area report
iaDT <- fread('IAsExp1FULL.csv') # fread is a data.table function that does super fast data loading

# SELECLT THE FUNCTIONS BELOW AND 'RUN' THEM

# THIS JUST DOES THE MEAN
organise.reading.calculateAverage <- function(ia_df, source_column_name, output_column_name, filterBy, aggregation_column_list){
  
  # REMOVE FULL STOPS AND CONVERT THEM TO NAs
  eval(parse(text=paste("ia_df[,", eval(source_column_name), ":=as.numeric(as.character(",eval(source_column_name),")),]", sep='')))
  
  # CALCULATE MEANS GROUPED BY aggregation_column_list
  eval(parse(text=paste("ia_df[", eval(filterBy), ",", eval(output_column_name), ":=mean(", eval(source_column_name), " [is.na(", eval(source_column_name), ")==F]),", eval(aggregation_column_list), "]", sep='')))
  
}

# THIS DOES THE UPPER AND LOWER SDs
organise.reading.calculateSDs <- function(ia_df, source_column_name, output_column_name, filterBy, sdLimit, aggregation_column_list){
  
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
                                (", eval(source_column_name), " < ", eval(lower_column_name), " | 
                                  ", eval(source_column_name), " > ", eval(upper_column_name), "), ",
                              eval(keeper_column_name), ":=1,]", sep="")))
  
}

# THIS COMBINES THE OTHERS TOGETHER
organise.reading.calculateRemovals <- function(ia_df, source_column_name, output_column_name, filterBy = '', sdLimit = 3, 
                                               aggregation_column_list=list('RECORDING_SESSION_LABEL')){
  
  suppressWarnings(organise.reading.calculateAverage(ia_df, source_column_name, output_column_name, filterBy, aggregation_column_list))
  suppressWarnings(organise.reading.calculateSDs(ia_df, source_column_name, output_column_name, filterBy, sdLimit, aggregation_column_list))
  
}

# NOW THE FUNCTIONS ARE SET UP, RUN THIS ONE
# CHANGE TRIAL INDEX TO SOMETHING THAT YOU WANT LIKE IF YOU WANT THE FIRST TRIAL INDEX ONLY WRITE TRIAL_INDEX
organise.reading.calculateRemovals(iaDT, source_column_name = 'IA_FIRST_FIXATION_DURATION', output_column_name = 'FFD', 
                                   filterBy = ' IA_FIRST_FIX_PROGRESSIVE == 1', sdLimit = 3, 
                                   aggregation_column_list=list('RECORDING_SESSION_LABEL')) # THIS IS DOING BY-PARTIIPANT OUTLIERS, CHANGE TO 
                                                                                              # A CONDITION COLUMN TO DO IT BY-CONDITION
          
# THEN TAKE A LOOK AT iaDT in the viewer and see what it's done

# YOUR NEW ONES GO HERE
organise.reading.totalReadingTime <- function(iaDT, sdLimit=3, aggregation_column_list=list('RECORDING_SESSION_LABEL')){
  
  organise.reading.calculateRemovals(iaDT, source_column_name = 'IA_DWELL_TIME', output_column_name = 'TOTAL_READING_TIME',
                                     filterBy = '', sdLimit = sdLimit, aggregation_column_list = aggregation_column_list)
}

organise.reading.firstFixationDuration <- function(iaDT, sdLimit=3, aggregation_column_list=list('RECORDING_SESSION_LABEL')){
  
  organise.reading.calculateRemovals(iaDT, source_column_name = 'IA_FIRST_FIXATION_DURATION', output_column_name = 'FIRST_FIXATION_DURATION',
                                     filterBy = 'IA_FIRST_FIX_PROGRESSIVE == 1', sdLimit = sdLimit, aggregation_column_list = aggregation_column_list)
}

organise.reading.Skipping <- function(iaDT, sdLimit=3, aggregation_column_list=list('RECORDING_SESSION_LABEL')){
  
  organise.reading.calculateRemovals(iaDT, source_column_name = 'IA_SKIP', output_column_name = 'SKIP',
                                     filterBy = '', sdLimit = sdLimit, aggregation_column_list = aggregation_column_list)
}

organise.reading.RegressionsIn <- function(iaDT, sdLimit=3, aggregation_column_list=list('RECORDING_SESSION_LABEL')){
  
  organise.reading.calculateRemovals(iaDT, source_column_name = 'IA_REGRESSION_IN_COUNT', output_column_name = 'REGRESSIONS_IN',
                                     filterBy = '', sdLimit = sdLimit, aggregation_column_list = aggregation_column_list)
}

organise.reading.RegressionsOut <- function(iaDT, sdLimit=3, aggregation_column_list=list('RECORDING_SESSION_LABEL')){
  
  organise.reading.calculateRemovals(iaDT, source_column_name = 'IA_REGRESSION_OUT_COUNT', output_column_name = 'REGRESSIONS_OUT',
                                     filterBy = '', sdLimit = sdLimit, aggregation_column_list = aggregation_column_list)
}

organise.reading.SaccadeAmplitude <- function(iaDT, sdLimit=3, aggregation_column_list=list('RECORDING_SESSION_LABEL')){
  
  organise.reading.calculateRemovals(iaDT, source_column_name = 'IA_FIRST_SACCADE_AMPLITUDE', output_column_name = 'FIRST_SACCADE_AMPLITUDE',
                                     filterBy = '', sdLimit = sdLimit, aggregation_column_list = aggregation_column_list)
}


organise.reading.GazeDuration <- function(iaDT, sdLimit=3, aggregation_column_list=list('RECORDING_SESSION_LABEL')){
  
  organise.reading.calculateRemovals(iaDT, source_column_name = 'IA_FIRST_RUN_DWELL_TIME', output_column_name = 'GAZE_DURATION',
                                     filterBy = 'IA_FIRST_FIX_PROGRESSIVE == 1', sdLimit = sdLimit, aggregation_column_list = aggregation_column_list)
}

organise.reading.GoPast <- function(iaDT, sdLimit=3, aggregation_column_list=list('RECORDING_SESSION_LABEL')){
  
  organise.reading.calculateRemovals(iaDT, source_column_name = 'IA_SELECTIVE_REGRESSION_PATH_DURATION', output_column_name = 'GO_PAST_TIME',
                                     filterBy = 'IA_FIRST_FIX_PROGRESSIVE == 1', sdLimit = sdLimit, aggregation_column_list = aggregation_column_list)
}

organise.reading.SingleFixationDuration <- function(iaDT, sdLimit=3, aggregation_column_list=list('RECORDING_SESSION_LABEL')){
  
  organise.reading.calculateRemovals(iaDT, source_column_name = 'IA_FIRST_FIXATION_DURATION', output_column_name = 'SINGLE_FIXATION_DURATION',
                                     filterBy = 'IA_FIRST_RUN_FIXATION_COUNT == 1'& 'IA_FIRST_FIX_PROGRESSIVE == 1', sdLimit = sdLimit, aggregation_column_list = aggregation_column_list)
}


# IGNORE THE STUFF BELOW

##############################################################################################################################################################
# MARKS UP EACH TRIAL WHERE A MESSAGE OCCURRED WITH THE MESSAGE VALUE
#' Markup trial messages.
#'
#' @param message_df Message report
#' @param fixreport_df Fixation report
#' @param message The message or event you want to mark up
#' @param show_working Should eyeTrackR show more detail when calculating the output?
#'
#' @return An updated fixation report with the message marked up into each trial.
#' If there is a difference between the number of input and output rows, there was a problem
#' with the joining of your data. You'll have a repeated session name or trial index.
#' @export
#'
#' @examples
#' data(fixationreport)
#' data(messagereport)
#' 
#' # REPLACE SPACES IN MESSAGES
#' messagereport <- organise.message.replace_spaces(messagereport)
#' 
#' # TAKE A LOOK
#' print(organise.message.descriptives(messagereport))
#' 
#' # MARKUP
#' fixationreport <- organise.message.markup(message_df=messagereport, 
#'     fixreport_df = fixationreport, message="DISPLAY_START")
#' 
#' fixationreport <- organise.message.markup(message_df=messagereport, 
#'     fixreport_df = fixationreport, message="DISPLAY_CHANGE")
organise.message.markup <- function(message_df, fixreport_df, message, show_working=FALSE){
  
  # SORT OUT THE MESSAGE REPORT
  mDT <- data.table(message_df)
  setkey(mDT, RECORDING_SESSION_LABEL, TRIAL_INDEX)
  
  # ORGANISE THE FIX REPORT
  fix_DT <- data.table(fixreport_df)
  setkey(fix_DT, RECORDING_SESSION_LABEL, TRIAL_INDEX)
  
  message(message)
  
  selectExpr <- parse(text=paste("list('", message, "'", "=", "CURRENT_MSG_TIME)", sep=""))                    
  
  selected_mDT <- mDT[CURRENT_MSG_TEXT==message,
                      eval(selectExpr),
                      list(RECORDING_SESSION_LABEL, TRIAL_INDEX)]
  
  setkey(selected_mDT, RECORDING_SESSION_LABEL, TRIAL_INDEX)
  
  # PRINTS THE MESSAGES
  if (show_working == TRUE){
    #print(mDT)
    #print(fix_DT)
    message(selected_mDT)
  }  
  
  # JOIN - A WORK IN PROGRESS
  joined_mDT <- join(data.frame(fix_DT), data.frame(selected_mDT)) #fix_DT[J(selected_mDT)]
  #joined_mDT <- selected_mDT[fix_DT] # THIS WORKS BUT HAS THE COLUMNS IN AN ANNOYING ORDER
  
  inputrow <- nrow(fixreport_df)
  outputrow <- nrow(data.frame(joined_mDT))
  
  message(paste("Difference between input and output rows:", inputrow-outputrow, sep=' '))
  if(inputrow-outputrow!=0){warning('There was a difference between input and output rows. Check your data.')}
  
  return(data.frame(joined_mDT))  
}
