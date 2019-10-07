##############################################################################################################################################################

globalVariables('RECORDING_SESSION_LABEL')
globalVariables(c(
  'CURRENT_FIX_ADJUSTED','CURRENT_FIX_BLINK_AROUND',
  'CURRENT_FIX_BUTTON_0_PRESS','CURRENT_FIX_BUTTON_1_PRESS',
  'CURRENT_FIX_LABEL','CURRENT_FIX_PUPIL', 'spss_df',
                  'CURRENT_FIX_BUTTON_2_PRESS','CURRENT_FIX_BUTTON_3_PRESS',
                  'CURRENT_FIX_BUTTON_4_PRESS','CURRENT_FIX_BUTTON_5_PRESS',
                  'CURRENT_FIX_BUTTON_6_PRESS','CURRENT_FIX_BUTTON_7_PRESS',
                  'CURRENT_FIX_BUTTON_8_PRESS','CURRENT_FIX_END_OTHER',
                  'CURRENT_FIX_INTEREST_AREAS','CURRENT_FIX_INTEREST_AREA_DWELL_TIME',
                  'CURRENT_FIX_INTEREST_AREA_FIX_COUNT','CURRENT_FIX_INTEREST_AREA_GROUP',
                  'CURRENT_FIX_INTEREST_AREA_INDEX','CURRENT_FIX_INTEREST_AREA_X_OFFSET',
                  'CURRENT_FIX_INTEREST_AREA_Y_OFFSET','CURRENT_FIX_IS_RT_END',
                  'CURRENT_FIX_LABEL CURRENT_FIX_PUPIL','CURRENT_FIX_REFIX_INTEREST_AREA',
                  'CURRENT_FIX_REFIX_PREV_INTEREST_AREA','CURRENT_FIX_RUN_DWELL_TIME',
                  'CURRENT_FIX_START_OTHER','CURRENT_FIX_TRIAL_SPAN','CURRENT_FIX_X_OTHER',
                  'CURRENT_FIX_X_RESOLUTION','CURRENT_FIX_Y_OTHER','CURRENT_FIX_Y_RESOLUTION',
                  'CURRENT_MSG_TEXT','CURRENT_MSG_TIME','DATA_FILE','EYE_USED',
                  'FIXATION_CONTINGENCY','IP_END_TIME','IP_LABEL','IP_START_TIME',
                  'LAST_BUTTON_PRESSED','LAST_BUTTON_PRESSED_TIME','LAST_BUTTON_RELEASED',
                  'LAST_BUTTON_RELEASED_TIME','LAST_BUTTON_TIME','NEXT_FIX_BLINK_AROUND',
                  'NEXT_FIX_END_OTHER','NEXT_FIX_INTEREST_AREAS','NEXT_FIX_IS_RT_END',
                  'NEXT_FIX_LABEL','NEXT_FIX_NEAREST_INTEREST_AREA','NEXT_FIX_PUPIL',
                  'NEXT_FIX_RUN_INDEX','NEXT_FIX_RUN_SIZE','NEXT_FIX_START_OTHER',
                  'NEXT_FIX_TRIAL_SPAN','NEXT_FIX_X','NEXT_FIX_X_RESOLUTION','NEXT_FIX_Y',
                  'NEXT_FIX_Y_OTHER','NEXT_SAC_LABEL','OUTCOME',
                  'NEXT_FIX_INTEREST_AREA_DWELL_TIME','NEXT_FIX_INTEREST_AREA_FIX_COUNT',
                  'NEXT_FIX_INTEREST_AREA_GROUP','NEXT_FIX_INTEREST_AREA_INDEX',
                  'NEXT_FIX_INTEREST_AREA_RUN_ID','NEXT_FIX_IS_RT_END NEXT_FIX_LABEL',
                  'NEXT_FIX_MSG_COUNT','NEXT_FIX_NEAREST_INTEREST_AREA NEXT_FIX_PUPIL',
                  'NEXT_FIX_RUN_DWELL_TIME','NEXT_FIX_RUN_INDEX NEXT_FIX_RUN_SIZE',
                  'NEXT_FIX_START','NEXT_FIX_START_OTHER NEXT_FIX_TRIAL_SPAN NEXT_FIX_X',
                  'NEXT_FIX_X_OTHER','NEXT_FIX_X_RESOLUTION NEXT_FIX_Y NEXT_FIX_Y_OTHER',
                  'NEXT_FIX_Y_RESOLUTION','NEXT_SAC_END_INTEREST_AREA_INDEX',
                  'NEXT_SAC_END_X_RESOLUTION','NEXT_SAC_IS_RT_END','NEXT_SAC_LABEL OUTCOME',
                  'PREVIOUS_FIX_ANGLE','PREVIOUS_FIX_BLINK_AROUND','PREVIOUS_FIX_DIRECTION',
                  'PREVIOUS_FIX_RUN_SIZE','PREVIOUS_FIX_START',
                  'PREVIOUS_FIX_DISTANCE','PREVIOUS_FIX_END','PREVIOUS_FIX_END_OTHER',
                  'PREVIOUS_FIX_INTEREST_AREAS','PREVIOUS_FIX_INTEREST_AREA_DWELL_TIME',
                  'PREVIOUS_FIX_INTEREST_AREA_FIX_COUNT','PREVIOUS_FIX_INTEREST_AREA_GROUP',
                  'PREVIOUS_FIX_INTEREST_AREA_INDEX','PREVIOUS_FIX_INTEREST_AREA_RUN_ID',
                  'PREVIOUS_FIX_IS_RT_END','PREVIOUS_FIX_LABEL','PREVIOUS_FIX_MSG_COUNT',
                  'PREVIOUS_FIX_NEAREST_INTEREST_AREA','PREVIOUS_FIX_PUPIL',
                  'PREVIOUS_FIX_RUN_DWELL_TIME','PREVIOUS_FIX_RUN_INDEX',
                  'PREVIOUS_FIX_RUN_SIZE PREVIOUS_FIX_START','PREVIOUS_FIX_START_OTHER',
                  'PREVIOUS_FIX_TRIAL_SPAN','PREVIOUS_FIX_X','PREVIOUS_FIX_X_OTHER',
                  'PREVIOUS_FIX_X_RESOLUTION','PREVIOUS_FIX_Y','RESPONSE_TIME','TRIAL_INDEX','TRUE_RT',
                  'inputrow','CURRENT_FIX_INDEX'))

# GETS RID OF SPACES IN THE MESSAGES
#' Replace spaces in message report message with underscores.
#'
#' @param message_df A message report.
#'
#' @return An updated message report with spaces between words replaced with underscores.
#' @export
#'
#' @examples 
#' data(messagereport)
#' messagereport <- organise.message.replace_spaces(messagereport)
#' @import data.table
#' @import stringr
#' @import utils
#' @import stats
#' @import plyr
organise.message.replace_spaces <- function(message_df){
  
  message_df$CURRENT_MSG_TEXT<-str_replace_all(message_df$CURRENT_MSG_TEXT, " ", "_")
  
  return(message_df)
}

##############################################################################################################################################################
# PICKS AND DISPLAYS A RANDOM TRIAL FOR DETAILED CHECKS
#' Display a randomly selected trial for detailed checks.
#'
#' @param fixreport_df object Input fixation report.
#'
#' @return Single trial as a data.table, plus the same trial printed to the console.
#' @export
#'
#' @examples
#' data(fixationreport)
#' organise.checks.random_trial(fixationreport)
organise.checks.random_trial <- function(fixreport_df){
  
  ppt_list <- unique(fixreport_df$RECORDING_SESSION_LABEL)
  
  ppt <- sample(ppt_list, 1)
  
  trial_list <- unique(fixreport_df$TRIAL_INDEX[fixreport_df$RECORDING_SESSION_LABEL==ppt])
  
  trial <- sample(trial_list, 1)
  
  selected <- fixreport_df[fixreport_df$RECORDING_SESSION_LABEL==ppt & fixreport_df$TRIAL_INDEX==trial,]
  
  selected$FIX_START <- selected$CURRENT_FIX_START
  selected$FIX_END <- selected$CURRENT_FIX_END
  selected$CURRENT_IA <- selected$CURRENT_FIX_INTEREST_AREA_LABEL
  
  print(selected)
  
  return(selected)
}

##############################################################################################################################################################
# BASIC STATS ON WHEN THINGS HAPPENED
#' Descriptive statistics for messages in message report.
#'
#' @param message_df Message report.
#'
#' @return Descriptive information relating to messages in the trials sent to the console for easy viewing.
#' @export
#'
#' @examples
#' data(messagereport)
#' organise.message.descriptives(messagereport)
organise.message.descriptives <- function(message_df){
  
  ## WE USE THIS TO FILTER OUT MESSAGES WHICH ARE NOT USEFUL
  message_df$include_message <- 1
  message_df$include_message[grep('TRIALID', message_df$CURRENT_MSG_TEXT)] <- 0
  message_df$include_message[grep('RECCFG', message_df$CURRENT_MSG_TEXT)] <- 0
  message_df$include_message[grep('ELCL', message_df$CURRENT_MSG_TEXT)] <- 0
  message_df$include_message[grep('GAZE_COORDS', message_df$CURRENT_MSG_TEXT)] <- 0
  message_df$include_message[grep('THRESHOLDS', message_df$CURRENT_MSG_TEXT)] <- 0
  message_df$include_message[grep('!MODE', message_df$CURRENT_MSG_TEXT)] <- 0
  
  # SORT OUT THE MESSAGE REPORT
  mDT <- data.table(message_df[message_df$include_message==1,])
  setkey(mDT, RECORDING_SESSION_LABEL, TRIAL_INDEX)
  
  # GIVE DESCRIPTIVES FOR MAIN MESSAGES
  summary_DT <-
    mDT[,
        list("EVENT_COUNT" = length(TRIAL_INDEX),
             "TIME_MEAN" = mean(CURRENT_MSG_TIME),
             "TIME_MIN" = min(CURRENT_MSG_TIME),
             "TIME_MAX" = max(CURRENT_MSG_TIME)),
        list(CURRENT_MSG_TEXT)
        ]
  
  #print(mDT)
  
  return(summary_DT)
}

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
#' # TAKE A LOOK AT THE MESSAGES WE HAVE
#' organise.message.descriptives(messagereport)
#' 
#' # MARKUP - SYNCTIME IS THE START OF THE TRIAL AND LEADIN IS A FIXATION CROSS BEFOREHAND
#' # THERE IS A TRIAL END BVUT WE DON'T WORRY ABOUT THAT SINCE WE USE THE RESPONSE MESSAGE INSTEAD
#' foo <- organise.message.markup(message_df=messagereport, fixreport_df = fixationreport, 
#'   message="LEADIN")
#' foo <- organise.message.markup(message_df=messagereport, fixreport_df = foo, 
#'   message="SYNCTIME")
organise.message.markup <- function(message_df, fixreport_df, message, show_working=FALSE){
  
  # SORT OUT THE MESSAGE REPORT
  mDT <- data.table(message_df)
  setkey(mDT, RECORDING_SESSION_LABEL, TRIAL_INDEX)
  
  # ORGANISE THE FIX REPORT
  fix_DT <- data.table(fixreport_df)
  setkey(fix_DT, RECORDING_SESSION_LABEL, TRIAL_INDEX)
  
  print(message)
  
  selectExpr <- parse(text=paste("list('", message, "'", "=", "CURRENT_MSG_TIME)", sep=""))                    
  
  selected_mDT <- mDT[CURRENT_MSG_TEXT==message,
                      eval(selectExpr),
                      list(RECORDING_SESSION_LABEL, TRIAL_INDEX)]
  
  setkey(selected_mDT, RECORDING_SESSION_LABEL, TRIAL_INDEX)
  
  # PRINTS THE MESSAGES
  if (show_working == TRUE){
	#print(mDT)
	#print(fix_DT)
    print(selected_mDT)
  }  
  
  # JOIN - A WORK IN PROGRESS
  joined_mDT <- join(data.frame(fix_DT), data.frame(selected_mDT)) #fix_DT[J(selected_mDT)]
  #joined_mDT <- selected_mDT[fix_DT] # THIS WORKS BUT HAS THE COLUMNS IN AN ANNOYING ORDER
  
  inputrow <- nrow(fixreport_df)
  outputrow <- nrow(data.frame(joined_mDT))
  
  print(paste("Difference between input and output rows:", inputrow-outputrow, sep=' '))
  if(inputrow-outputrow!=0){warning('There was a difference between input and output rows. Check your data.')}
  
  return(data.frame(joined_mDT))  
}

##############################################################################################################################################################

##############################################################################################################################################################
# RETURNS TRIALS WHERE A SPECIFIC MESSAGE IS FOUND

#' Return trials where a specific message is found.
#'
#' @param message_df Message report.
#' @param fixreport_df Fixation report.
#' @param message The message you want to search for.
#' @param show_working Should eyeTrackR show more detail when calculating the output?
#'
#' @return Data.table of marked up fixation report.
#' @export
#'
#' @examples
#' # HERE, 'SYNCTIME' STARTS A TRIAL
#' data(messagereport)
#' data(fixationreport)
#' organise.message.return_specific(messagereport, fixationreport, 'SYNCTIME')
organise.message.return_specific <- function(message_df, fixreport_df, message, show_working=FALSE){
  
  # SORT OUT THE MESSAGE REPORT
  mDT <- data.table(message_df)
  setkey(mDT, RECORDING_SESSION_LABEL, TRIAL_INDEX)
  
  # ORGANISE THE FIX REPORT
  fix_DT <- data.table(fixreport_df)
  setkey(fix_DT, RECORDING_SESSION_LABEL, TRIAL_INDEX)
  
  print(message)
  
  selectExpr <- parse(text=paste("list('", message, "'", "=", "CURRENT_MSG_TIME)", sep=""))                    
  
  selected_mDT <- mDT[CURRENT_MSG_TEXT==message,
                      eval(selectExpr),
                      list(RECORDING_SESSION_LABEL, TRIAL_INDEX)]
  
  # PRINTS THE MESSAGES
  if (show_working == TRUE){
    print(selected_mDT)
  }  
  
  joined_mDT <- fix_DT[selected_mDT]
  
  return(data.table(joined_mDT))  
}

##############################################################################################################################################################
# ORGANISE AND MARKUP FIXATION CONTINGENCIES
#' Oganise and markup fixation contingencies.
#'
#' @param fixreport_df Fixation report.
#' @param ordered_message_list List of messages to markup, in temporal order at which they occurred.
#'
#' @return Marked-up fixation report data.table.
#' @export
#'
#' @examples
#' 
#' data(fixationreport)
#' data(messagereport)
#' 
#' # REPLACE SPACES IN MESSAGES
#' messagereport <- organise.message.replace_spaces(messagereport)
#' 
#' # TAKE A LOOK AT THE MESSAGES WE HAVE
#' organise.message.descriptives(messagereport)
#' 
#' # MARKUP - SYNCTIME IS THE START OF THE TRIAL AND LEADIN IS A FIXATION CROSS BEFOREHAND
#' # THERE IS A TRIAL END BVUT WE DON'T WORRY ABOUT THAT SINCE WE USE THE RESPONSE MESSAGE INSTEAD
#' foo <- organise.message.markup(message_df=messagereport, fixreport_df = fixationreport, 
#'    message="LEADIN")
#' foo <- organise.message.markup(message_df=messagereport, fixreport_df = foo, 
#'    message="SYNCTIME")
#' 
#' # NOW DO ACCURACY AND RT MARKUP
#' foo <- organise.responses.markup(foo, "CORRECT_RESPONSE")
#'
#'# NOW MARK UP FIXATION CONTINGENCIES
#' foo<-organise.message.fix_contingencies(foo, list("LEADIN", "SYNCTIME", "RESPONSE_TIME"))
organise.message.fix_contingencies <- function(fixreport_df, ordered_message_list){
  
  # DO THE MAIN THING
  for (i in 1:length(ordered_message_list)){
    
    # IF WE ARE AT THE START OF OUR LIST, HANDLE PRE-FIRST EVENT FIXATIONS
    if (i==1){
      eventBasePreExpr <- paste("fixreport_df$WITHIN_PRE_", ordered_message_list[i], "<-'FALSE'", sep="")
      eventBasePreParsed <- parse(text=eventBasePreExpr)
      eval(eventBasePreParsed)
      
      preExpr <- paste("fixreport_df$WITHIN_PRE_", ordered_message_list[i], 
                       "[(fixreport_df$CURRENT_FIX_START <= ", "fixreport_df$", ordered_message_list[i], ") |",
                       "(fixreport_df$CURRENT_FIX_END <= ", "fixreport_df$", ordered_message_list[i], ")",                      
                       "]<- TRUE", sep="")
      
      preExprParsed <- parse(text=preExpr)
      eval(preExprParsed)
      
    }
    
    # SETUP EVENT COLUMN
    eventBaseExpr <- paste("fixreport_df$WITHIN_", ordered_message_list[i], "<-'FALSE'", sep="")
    eventBaseParsed <- parse(text=eventBaseExpr)
    eval(eventBaseParsed)
    
    # CHECK IF FIXATION WITHIN EVENT LIMITS
    
    print(ordered_message_list[i])
    
    # IF WE ARE NOT AT THE END OF OUR LIST, DO THIS
    if (i<length(ordered_message_list)){
      
      colExpr <- paste("fixreport_df$WITHIN_", ordered_message_list[i], 
                       "[(fixreport_df$CURRENT_FIX_START >= ", "fixreport_df$", ordered_message_list[i], 
                       " & fixreport_df$CURRENT_FIX_START <= ", "fixreport_df$", ordered_message_list[i+1], 
                       ") | ",
                       "(fixreport_df$CURRENT_FIX_END >= ", "fixreport_df$", ordered_message_list[i], 
                       " & fixreport_df$CURRENT_FIX_END <= ", "fixreport_df$", ordered_message_list[i+1], ")",
                       " | ",       
                       "(fixreport_df$CURRENT_FIX_START < ", "fixreport_df$", ordered_message_list[i], " & ",
                       "fixreport_df$CURRENT_FIX_END > ", "fixreport_df$", ordered_message_list[i+1], ")", 
                       "]<- TRUE", sep="")
    }
    
    # IF WE ARE AT THE END OF OUR LIST, DO THIS
    if (i==length(ordered_message_list)){
      colExpr <- paste("fixreport_df$WITHIN_", ordered_message_list[i], 
                       "[(fixreport_df$CURRENT_FIX_START >= ", "fixreport_df$", ordered_message_list[i], ") |",
                       "(fixreport_df$CURRENT_FIX_END >= ", "fixreport_df$", ordered_message_list[i], ")",                      
                       "]<- TRUE", sep="")
    }
    
    # EVALUATE AND RUN
    colExprParsed <- parse(text=colExpr)
    eval(colExprParsed)
  }
  
  fixreport_df$FIXATION_CONTINGENCY <- ""
  
  ordered_message_list <- c(paste("PRE_", ordered_message_list[1], sep=""), ordered_message_list)
  
  for (i in seq(1:length(ordered_message_list))){
    
    comparisonExpr <- paste("fixreport_df$FIXATION_CONTINGENCY[fixreport_df$WITHIN_", ordered_message_list[i]," == TRUE & fixreport_df$FIXATION_CONTINGENCY!=''] ",
                            " <- ", 
                            "paste(","fixreport_df$FIXATION_CONTINGENCY[fixreport_df$WITHIN_", ordered_message_list[i], " == TRUE & fixreport_df$FIXATION_CONTINGENCY!=''], ",
                            "'__", ordered_message_list[i], "', ",
                            "sep='')",
                            sep="")
    
    #print(comparisonExpr)
    comaprisonExprParsed <- parse(text=comparisonExpr)
    eval(comaprisonExprParsed)
    
    comparisonExpr <- paste("fixreport_df$FIXATION_CONTINGENCY[fixreport_df$WITHIN_", ordered_message_list[i]," == TRUE & fixreport_df$FIXATION_CONTINGENCY==''] ",
                            " <- ", 
                            "paste(","fixreport_df$FIXATION_CONTINGENCY[fixreport_df$WITHIN_", ordered_message_list[i], " == TRUE & fixreport_df$FIXATION_CONTINGENCY==''], ",
                            "'", ordered_message_list[i], "', ",
                            "sep='')",
                            sep="")
    
    #print(comparisonExpr)
    comaprisonExprParsed <- parse(text=comparisonExpr)
    eval(comaprisonExprParsed)
    
  }
  
  fixreport_df$FIXATION_CONTINGENCY[fixreport_df$FIXATION_CONTINGENCY==""]<- "UNCLASSIFIED"
  
  return(data.table(fixreport_df))
}

##############################################################################################################################################################
# ORGANISE AND MARKUP RESPONSE DETAILS
#' Mark up responses into a fixation report.
#'
#' @param fixreport_df Fixation report
#' @param correct_answer_column The column in the fixation report containing the correct button response number (1-7).
#'
#' @return Updated fixation report as a data.table.
#' @export
#'
#' @examples
#' 
#' data(fixationreport)
#' data(messagereport)
#' 
#' # REPLACE SPACES IN MESSAGES
#' messagereport <- organise.message.replace_spaces(messagereport)
#' 
#' # TAKE A LOOK AT THE MESSAGES WE HAVE
#' organise.message.descriptives(messagereport)
#' 
#' # MARKUP - SYNCTIME IS THE START OF THE TRIAL AND LEADIN IS A FIXATION CROSS BEFOREHAND
#' # THERE IS A TRIAL END BVUT WE DON'T WORRY ABOUT THAT SINCE WE USE THE RESPONSE MESSAGE INSTEAD
#' foo <- organise.message.markup(message_df=messagereport, fixreport_df = fixationreport, 
#'    message="LEADIN")
#' foo <- organise.message.markup(message_df=messagereport, fixreport_df = foo, 
#'    message="SYNCTIME")
#' 
#' # NOW DO ACCURACY AND RT MARKUP
#' foo <- organise.responses.markup(foo, "CORRECT_RESPONSE")
organise.responses.markup <- function(fixreport_df, correct_answer_column){
  
  # ORGANISE THE FIX REPORT
  fix_DT <- data.table(fixreport_df)
  setkey(fix_DT, RECORDING_SESSION_LABEL, TRIAL_INDEX)
  
  # GROUPING COL EVALUATION
  aggExpr <- paste("list(RECORDING_SESSION_LABEL, TRIAL_INDEX, ", correct_answer_column, ")", sep="")
  aggExprParsed <- parse(text = aggExpr)
    
  # GET RESPONSES
  resp_DT <- fix_DT[CURRENT_FIX_BUTTON_0_PRESS!="." | CURRENT_FIX_BUTTON_1_PRESS!="." | CURRENT_FIX_BUTTON_2_PRESS!="." | CURRENT_FIX_BUTTON_3_PRESS!="." |
      CURRENT_FIX_BUTTON_4_PRESS!="." | CURRENT_FIX_BUTTON_5_PRESS!="." | CURRENT_FIX_BUTTON_6_PRESS!="." | CURRENT_FIX_BUTTON_7_PRESS!="." | CURRENT_FIX_BUTTON_8_PRESS!=".",
                      list("CURRENT_FIX_BUTTON_0_PRESS" = CURRENT_FIX_BUTTON_0_PRESS[1],
                           "CURRENT_FIX_BUTTON_1_PRESS" = CURRENT_FIX_BUTTON_1_PRESS[1],
                           "CURRENT_FIX_BUTTON_2_PRESS" = CURRENT_FIX_BUTTON_2_PRESS[1],
                           "CURRENT_FIX_BUTTON_3_PRESS" = CURRENT_FIX_BUTTON_3_PRESS[1],
                           "CURRENT_FIX_BUTTON_4_PRESS" = CURRENT_FIX_BUTTON_4_PRESS[1],
                           "CURRENT_FIX_BUTTON_5_PRESS" = CURRENT_FIX_BUTTON_5_PRESS[1],
                           "CURRENT_FIX_BUTTON_6_PRESS" = CURRENT_FIX_BUTTON_6_PRESS[1],
                           "CURRENT_FIX_BUTTON_7_PRESS" = CURRENT_FIX_BUTTON_7_PRESS[1],
                           "CURRENT_FIX_BUTTON_8_PRESS" = CURRENT_FIX_BUTTON_8_PRESS[1]),
      eval(aggExprParsed)]
  
    resp_DT$BUTTON_NUMBER <- NA
    resp_DT$BUTTON_NUMBER[resp_DT$CURRENT_FIX_BUTTON_0_PRESS!='.'] <- 0
    resp_DT$BUTTON_NUMBER[resp_DT$CURRENT_FIX_BUTTON_1_PRESS!='.'] <- 1
    resp_DT$BUTTON_NUMBER[resp_DT$CURRENT_FIX_BUTTON_2_PRESS!='.'] <- 2
    resp_DT$BUTTON_NUMBER[resp_DT$CURRENT_FIX_BUTTON_3_PRESS!='.'] <- 3
    resp_DT$BUTTON_NUMBER[resp_DT$CURRENT_FIX_BUTTON_4_PRESS!='.'] <- 4
    resp_DT$BUTTON_NUMBER[resp_DT$CURRENT_FIX_BUTTON_5_PRESS!='.'] <- 5
    resp_DT$BUTTON_NUMBER[resp_DT$CURRENT_FIX_BUTTON_6_PRESS!='.'] <- 6
    resp_DT$BUTTON_NUMBER[resp_DT$CURRENT_FIX_BUTTON_7_PRESS!='.'] <- 7
    resp_DT$BUTTON_NUMBER[resp_DT$CURRENT_FIX_BUTTON_8_PRESS!='.'] <- 8
  
    # SORT OUT OUTCOME
    resp_DT$OUTCOME <- NA
    outExpr <- paste("resp_DT$OUTCOME[resp_DT$BUTTON_NUMBER == resp_DT$", correct_answer_column, "] <- 'CORRECT'", sep="")
    outExprParsed <- parse(text=outExpr)
    eval(outExprParsed)
  
    outExpr2 <- paste("resp_DT$OUTCOME[resp_DT$BUTTON_NUMBER != resp_DT$", correct_answer_column, "] <- 'INCORRECT'", sep="") 
    outExprParsed2 <- parse(text=outExpr2)
    eval(outExprParsed2)
  
    resp_DT$RESPONSE_TIME <- NA
    resp_DT$RESPONSE_TIME[resp_DT$CURRENT_FIX_BUTTON_0_PRESS!='.'] <- as.numeric(as.character(resp_DT$CURRENT_FIX_BUTTON_0_PRESS[resp_DT$CURRENT_FIX_BUTTON_0_PRESS!='.']))
    resp_DT$RESPONSE_TIME[resp_DT$CURRENT_FIX_BUTTON_1_PRESS!='.'] <- as.numeric(as.character(resp_DT$CURRENT_FIX_BUTTON_1_PRESS[resp_DT$CURRENT_FIX_BUTTON_1_PRESS!='.']))
    resp_DT$RESPONSE_TIME[resp_DT$CURRENT_FIX_BUTTON_2_PRESS!='.'] <- as.numeric(as.character(resp_DT$CURRENT_FIX_BUTTON_2_PRESS[resp_DT$CURRENT_FIX_BUTTON_2_PRESS!='.']))
    resp_DT$RESPONSE_TIME[resp_DT$CURRENT_FIX_BUTTON_3_PRESS!='.'] <- as.numeric(as.character(resp_DT$CURRENT_FIX_BUTTON_3_PRESS[resp_DT$CURRENT_FIX_BUTTON_3_PRESS!='.']))
    resp_DT$RESPONSE_TIME[resp_DT$CURRENT_FIX_BUTTON_4_PRESS!='.'] <- as.numeric(as.character(resp_DT$CURRENT_FIX_BUTTON_4_PRESS[resp_DT$CURRENT_FIX_BUTTON_4_PRESS!='.']))
    resp_DT$RESPONSE_TIME[resp_DT$CURRENT_FIX_BUTTON_5_PRESS!='.'] <- as.numeric(as.character(resp_DT$CURRENT_FIX_BUTTON_5_PRESS[resp_DT$CURRENT_FIX_BUTTON_5_PRESS!='.']))
    resp_DT$RESPONSE_TIME[resp_DT$CURRENT_FIX_BUTTON_6_PRESS!='.'] <- as.numeric(as.character(resp_DT$CURRENT_FIX_BUTTON_6_PRESS[resp_DT$CURRENT_FIX_BUTTON_6_PRESS!='.']))
    resp_DT$RESPONSE_TIME[resp_DT$CURRENT_FIX_BUTTON_7_PRESS!='.'] <- as.numeric(as.character(resp_DT$CURRENT_FIX_BUTTON_7_PRESS[resp_DT$CURRENT_FIX_BUTTON_7_PRESS!='.']))
    resp_DT$RESPONSE_TIME[resp_DT$CURRENT_FIX_BUTTON_8_PRESS!='.'] <- as.numeric(as.character(resp_DT$CURRENT_FIX_BUTTON_8_PRESS[resp_DT$CURRENT_FIX_BUTTON_8_PRESS!='.']))
  
    # FINAL STUFF FOR JOINING
    resp_DT$CURRENT_FIX_BUTTON_0_PRESS <- NULL
    resp_DT$CURRENT_FIX_BUTTON_1_PRESS <- NULL
    resp_DT$CURRENT_FIX_BUTTON_2_PRESS <- NULL
    resp_DT$CURRENT_FIX_BUTTON_3_PRESS <- NULL
    resp_DT$CURRENT_FIX_BUTTON_4_PRESS <- NULL
    resp_DT$CURRENT_FIX_BUTTON_5_PRESS <- NULL
    resp_DT$CURRENT_FIX_BUTTON_6_PRESS <- NULL
    resp_DT$CURRENT_FIX_BUTTON_7_PRESS <- NULL
    resp_DT$CURRENT_FIX_BUTTON_8_PRESS <- NULL
      
    # JOIN UP WITH MAIN DF BEFORE RETURNING
    joined_df <- join(data.frame(fix_DT), data.frame(resp_DT))
  
    return(data.table(joined_df))
}

##############################################################################################################################################################
# BASIC STATS ON FIXATION CONTINGENCIES
#' Descriptive statistics of fixation contingencies. 
#'
#' @param fixreport_df Fixation report.
#'
#' @return Output to console.
#' @export
#'
#' @examples
#' data(fixationreport)
#' data(messagereport)
#' 
#' # REPLACE SPACES IN MESSAGES
#' messagereport <- organise.message.replace_spaces(messagereport)
#' 
#' # TAKE A LOOK AT THE MESSAGES WE HAVE
#' organise.message.descriptives(messagereport)
#' 
#' # MARKUP - SYNCTIME IS THE START OF THE TRIAL AND LEADIN IS A FIXATION CROSS BEFOREHAND
#' # THERE IS A TRIAL END BUT WE DON'T WORRY ABOUT THAT SINCE WE USE THE RESPONSE MESSAGE INSTEAD
#' foo <- organise.message.markup(message_df=messagereport, fixreport_df = fixationreport, 
#'                                message="LEADIN")
#' foo <- organise.message.markup(message_df=messagereport, fixreport_df = foo, 
#'                                message="SYNCTIME")
#' 
#' # NOW DO ACCURACY AND RT MARKUP
#' foo <- organise.responses.markup(foo, "CORRECT_RESPONSE")
#' 
#' # NOW MARK UP FIXATION CONTINGENCIES
#' foo<-organise.message.fix_contingencies(foo, list("LEADIN", "SYNCTIME", "RESPONSE_TIME"))
#'
#' # RANDOM TRIAL TO CHECK THINGS OUT
#' organise.checks.random_trial(foo)
#'
#' # FIX CONTINGENCIES
#' organise.contingencies.descriptives(foo)
organise.contingencies.descriptives <- function(fixreport_df){
  
  # SORT OUT THE MESSAGE REPORT
  fDT <- data.table(fixreport_df)
  setkey(fDT, RECORDING_SESSION_LABEL, TRIAL_INDEX)
  
  # GIVE DESCRIPTIVES FOR MAIN MESSAGES
  summary_DT <-
    fDT[,list("FIXATION_COUNT" = length(TRIAL_INDEX)),
        list(FIXATION_CONTINGENCY)]
  
  return(data.frame(summary_DT))
}

##############################################################################################################################################################
# REMOVES TRIALS WHICH FAILED TO HAVE ALL OF THE MESSAGES LISTED. RETURNS THE DF WITH THEM REMOVED. SAVES A REPORT OF HOW MANY WERE REMOVED AND WHY.
#' Remove trials which fail to have all of the listed messages. 
#'
#' @param fixreport_df Fixation report.
#' @param required_message_list List of messages required for each trial.
#'
#' @return Updated fixation report with trials removed as a data.table. 
#' @export
#'
#' @examples
#' data(fixationreport)
#' data(messagereport)
#' 
#' # REPLACE SPACES IN MESSAGES
#' messagereport <- organise.message.replace_spaces(messagereport)
#' 
#' # TAKE A LOOK AT THE MESSAGES WE HAVE
#' organise.message.descriptives(messagereport)
#' 
#' # MARKUP - SYNCTIME IS THE START OF THE TRIAL AND LEADIN IS A FIXATION CROSS BEFOREHAND
#' # THERE IS A TRIAL END BVUT WE DON'T WORRY ABOUT THAT SINCE WE USE THE RESPONSE MESSAGE INSTEAD
#' foo <- organise.message.markup(message_df=messagereport, fixreport_df = fixationreport, 
#'                                message="LEADIN")
#' foo <- organise.message.markup(message_df=messagereport, fixreport_df = foo, 
#'                                message="SYNCTIME")
#' 
#' # NOW DO ACCURACY AND RT MARKUP
#' foo <- organise.responses.markup(foo, "CORRECT_RESPONSE")
#' 
#' # NOW MARK UP FIXATION CONTINGENCIES
#' foo<-organise.message.fix_contingencies(foo, list("LEADIN", "SYNCTIME", "RESPONSE_TIME"))
#' 
#' # RANDOM TRIAL TO CHECK THINGS OUT
#' organise.checks.random_trial(foo)
#' 
#' # FIX CONTINGENCIES
#' organise.contingencies.descriptives(foo)
#' 
#' # REMOVE MISSING EVENTS - HERE, TRIALS WHICH LACKED A RESPOSNE
#' foo <- organise.message.removals(fixreport_df=foo, 
#'                                  required_message_list=list("LEADIN", "SYNCTIME", "RESPONSE_TIME"))
#' # REMOVES NO TRIALS = EXCELLENT!
#' 
#' organise.contingencies.descriptives(foo) 
#' # THIS LISTS NO UNCLASSIFIED FIXATIONS WHICH IS GOOD BECAUSE IT MEANS WE ACCOUNT FOR 
#' # EVERYTHING THAT TOOK PLACE IN OUR TRIALS.
organise.message.removals <- function(fixreport_df, required_message_list){
  
  # SET UP KEYS
  fix_DT <- data.table(fixreport_df)
  setkey(fix_DT, RECORDING_SESSION_LABEL, TRIAL_INDEX)  
  
  full_DT <- fix_DT[, list("TRIAL_TOTAL" = length(unique(TRIAL_INDEX))),
                    list(RECORDING_SESSION_LABEL)]
  
  # STORAGE FOR A BIG 'WHERE' EXPRESSION CONSISTING OF ANDs FOR ALL EVENTS
  bigWhereExpr <- ''
  
  for (i in seq(1:length(required_message_list))){
    
    # SET UP WHERE EXPRESSION
    whereExpr <- paste("is.na(", required_message_list[i], ")==FALSE", sep="")
    whereExprParsed <- parse(text = whereExpr)
    
    # ADD TO BIG WHERE EXPRESSION
    if (i ==1){
      bigWhereExpr <- paste(bigWhereExpr, whereExpr, sep="")
    }
    
    if (i > 1){
      bigWhereExpr <- paste(bigWhereExpr, " & ", whereExpr, sep="")      
    }
    
    # SET UP SELECT EXPRESSION
    selectExpr <- paste("list('TRIAL_COUNT_", required_message_list[i], "' = length(unique(TRIAL_INDEX)))", sep="")
    selectExprParsed <- parse(text = selectExpr)
    
    selected_DT <- fix_DT[eval(whereExprParsed),
                          eval(selectExprParsed),
                          list(RECORDING_SESSION_LABEL)]
    
    full_DT <- full_DT[selected_DT]
    
  }
  
  # NOW DO THE FINAL COLUMN WHICH HAS ALL INFO
  bigWhereExprParsed <- parse(text=bigWhereExpr)
  
  selected_DT <- fix_DT[eval(bigWhereExprParsed),
                        list("FINAL_TRIAL_COUNT" = length(unique(TRIAL_INDEX))),
                        list(RECORDING_SESSION_LABEL)]
  
  full_DT <- full_DT[selected_DT]
  
  full_DT$LOST_TRIALS <- full_DT$TRIAL_TOTAL - full_DT$FINAL_TRIAL_COUNT
  full_DT$LOST_TRIALS_PERC <- full_DT$FINAL_TRIAL_COUNT / full_DT$TRIAL_TOTAL
  
  #write.table(full_DT, "event_trials_missing.txt", row.names=FALSE)
  
  print("EVENT INCLUSIONS AND EXCLUSIONS")
  print(full_DT)
  
  # NOW RUN THE MEGA BEAST ON THE MAIN FIXATION REPORT AND RETURN IT
  final_DT <- fix_DT[eval(bigWhereExprParsed),]
  
  return(data.frame(final_DT))
}

# SAVES RT AND ACCURACY SPLIT BY SPECIFIED COLUMNS #############################################################################################################################################################
#' Save RT and Accuracy split by specified columns.
#'
#' @param fixreport_df Fixation report.
#' @param grouping_column_list List of columns to split by.
#' @param response_period_start Message that starts the RT timer.
#' @param prefix_label Prefix output with a label.
#'
#' @return Summarised behavioural information as a data.table. This is also saved to disk.
#' @export
#'
#' @examples
#' data(fixationreport)
#' data(messagereport)
#' 
#' # REPLACE SPACES IN MESSAGES
#' messagereport <- organise.message.replace_spaces(messagereport)
#' 
#' # TAKE A LOOK AT THE MESSAGES WE HAVE
#' organise.message.descriptives(messagereport)
#' 
#' # MARKUP - SYNCTIME IS THE START OF THE TRIAL AND LEADIN IS A FIXATION CROSS BEFOREHAND
#' # THERE IS A TRIAL END BVUT WE DON'T WORRY ABOUT THAT SINCE WE USE THE RESPONSE MESSAGE INSTEAD
#' foo <- organise.message.markup(message_df=messagereport, fixreport_df = fixationreport, 
#'                                message="LEADIN")
#' foo <- organise.message.markup(message_df=messagereport, fixreport_df = foo, 
#'                                message="SYNCTIME")
#' 
#' # NOW DO ACCURACY AND RT MARKUP
#' foo <- organise.responses.markup(foo, "CORRECT_RESPONSE")
#' 
#' # NOW MARK UP FIXATION CONTINGENCIES
#' foo<-organise.message.fix_contingencies(foo, list("LEADIN", "SYNCTIME", "RESPONSE_TIME"))
#' 
#' # RANDOM TRIAL TO CHECK THINGS OUT
#' organise.checks.random_trial(foo)
#' 
#' # FIX CONTINGENCIES
#' organise.contingencies.descriptives(foo)
#' 
#' # REMOVE MISSING EVENTS - HERE, TRIALS WHICH LACKED A RESPOSNE
#' foo <- organise.message.removals(fixreport_df=foo, 
#'                                  required_message_list=list("LEADIN", "SYNCTIME", "RESPONSE_TIME"))
#' # REMOVES NO TRIALS = EXCELLENT!
#' 
#' organise.contingencies.descriptives(foo) # THIS SHOWS WE HAVE NO UNCLASSIFIED FIXATIONS, GOOD!
#' 
#' # BY-TRIAL BEHAVIOURAL DATA
#' organise.behavioural.base(fixreport_df = foo, list( 'TRIALTYPE_TEXT'), 
#'                           response_period_start="SYNCTIME", 
#'                           prefix_label="BYTRIAL")
organise.behavioural.base <- function(fixreport_df, grouping_column_list, response_period_start="", prefix_label="") {
  
  # ORGANISE THE FIX REPORT
  fix_DT <- data.table(fixreport_df)
  setkey(fix_DT, RECORDING_SESSION_LABEL, TRIAL_INDEX)
  
  # GENERATE EXPRESSION FOR OUTPUT COLUMNS
  aggExpr <- "list(RECORDING_SESSION_LABEL, TRIAL_INDEX "
  
  # IF ITS NOT A BLANK COLUMN LIST
  if (length(grouping_column_list) > 0){
    
    for (i in seq(1:length(grouping_column_list))){
      aggExpr <- paste(aggExpr, ", ", grouping_column_list[i], sep="")
    }
  }
  
  # CLOSE UP THE AGG LISTS
  aggExpr <- paste(aggExpr, ")", sep="")
  
  # PARSE THE EXPRESSIONS
  aggExprParsed <- parse(text = aggExpr)
  
  # GET BEHAVIOURAL RAW DATA AND THEN SAVE IT
  if (response_period_start==""){
    b_DT <- fix_DT[,
                   list("RESPONSE_TIME" = RESPONSE_TIME[1],
                        "TRUE_RT" = RESPONSE_TIME[1],
                        "OUTCOME" = OUTCOME[1]),
                   eval(aggExprParsed)]
  }
  
  if (response_period_start!=""){
        
    rtExpr <- "list('RESPONSE_TIME' = RESPONSE_TIME[1],"
    rtExpr <- paste(rtExpr, "'TRUE_RT'= RESPONSE_TIME[1] - ", response_period_start, "[1],", sep="")
    rtExpr <- paste(rtExpr, "'OUTCOME' = OUTCOME[1])", sep="")
    rtExprParsed <- parse(text=rtExpr)
    
    b_DT <- fix_DT[,
                   eval(rtExprParsed),
                   eval(aggExprParsed)]
  }
  
  print(b_DT)
  
  #write.table(data.frame(b_DT), paste(prefix_label, "behavioural_data.txt", sep="_"), row.names=FALSE)

  return(b_DT)
}

############################################################################################################################################################################################################################################################################################################################


# REMOVES FIXATIONS THAT WERE TOO LONG OR TOO SHORT #############################################################################################################################################################
#' Exclude very brief and very long fixations.
#'
#' @param fixreport_df Fixation report.
#' @param min Minimum duration of fixations.
#' @param max Maximum duration of fixations.
#'
#' @return Fixation report with outliers removed. Also saves information on how many fixations were removed.
#' @export
#'
#' @examples
#' data(fixationreport)
#' data(messagereport)
#' 
#' # REPLACE SPACES IN MESSAGES
#' messagereport <- organise.message.replace_spaces(messagereport)
#' 
#' # TAKE A LOOK AT THE MESSAGES WE HAVE
#' organise.message.descriptives(messagereport)
#' 
#' # MARKUP - SYNCTIME IS THE START OF THE TRIAL AND LEADIN IS A FIXATION CROSS BEFOREHAND
#' # THERE IS A TRIAL END BVUT WE DON'T WORRY ABOUT THAT SINCE WE USE THE RESPONSE MESSAGE INSTEAD
#' foo <- organise.message.markup(message_df=messagereport, fixreport_df = fixationreport, 
#'                                message="LEADIN")
#' foo <- organise.message.markup(message_df=messagereport, fixreport_df = foo, 
#'                                message="SYNCTIME")
#' 
#' # NOW DO ACCURACY AND RT MARKUP
#' foo <- organise.responses.markup(foo, "CORRECT_RESPONSE")
#' 
#' # NOW MARK UP FIXATION CONTINGENCIES
#' foo<-organise.message.fix_contingencies(foo, list("LEADIN", "SYNCTIME", "RESPONSE_TIME"))
#' 
#' # RANDOM TRIAL TO CHECK THINGS OUT
#' organise.checks.random_trial(foo)
#' 
#' # FIX CONTINGENCIES
#' organise.contingencies.descriptives(foo)
#' 
#' # REMOVE MISSING EVENTS - HERE, TRIALS WHICH LACKED A RESPOSNE
#' foo <- organise.message.removals(fixreport_df=foo, 
#'                                  required_message_list=list("LEADIN", "SYNCTIME", "RESPONSE_TIME"))
#' # REMOVES NO TRIALS = EXCELLENT!
#' 
#' organise.contingencies.descriptives(foo) # THIS SHOWS WE HAVE NO UNCLASSIFIED FIXATIONS, GOOD!
#' 
#' # BY-TRIAL BEHAVIOURAL DATA
#' organise.behavioural.base(fixreport_df = foo, list('TRIALTYPE_TEXT'), 
#'                           response_period_start="SYNCTIME", 
#'                           prefix_label="BYTRIAL")
#'                           
#' # NEXT REMOVE PRE-SYNCTIME FIXATIONS
#' fooy <- foo[foo$FIXATION_CONTINGENCY!='PRE_LEADIN__LEADIN',]
#' fooy <- fooy[fooy$FIXATION_CONTINGENCY!='LEADIN',]
#' fooy <- organise.exclusions.fix_durations(fixreport_df=fooy)
organise.exclusions.fix_durations <- function(fixreport_df, min=60, max=1200){
  
  # SET UP KEYS
  fix_DT <- data.table(fixreport_df)
  setkey(fix_DT, RECORDING_SESSION_LABEL, TRIAL_INDEX)  
  
  full_DT <- fix_DT[, list("FIX_TOTAL" = length(CURRENT_FIX_INDEX)),
                    list(RECORDING_SESSION_LABEL)]
  
  # SET UP WHERE EXPRESSION
  whereExpr <- paste("CURRENT_FIX_DURATION", "<", max, " & ", 
                     "CURRENT_FIX_DURATION", ">", min, "",
                     sep="")
  whereExprParsed <- parse(text = whereExpr)
  
  print(whereExpr)
  
  # SET UP SELECT EXPRESSION
  selectExpr <- paste("list('FINAL_FIX_COUNT' = length(CURRENT_FIX_INDEX))", sep="")
  selectExprParsed <- parse(text = selectExpr)
  
  selected_DT <- fix_DT[eval(whereExprParsed),
                        eval(selectExprParsed),
                        list(RECORDING_SESSION_LABEL)]
  
  full_DT <- full_DT[selected_DT]
  
  full_DT$LOST_FIXES <- full_DT$FIX_TOTAL - full_DT$FINAL_FIX_COUNT
  full_DT$LOST_FIXES_PERC <- full_DT$FINAL_FIX_COUNT / full_DT$FIX_TOTAL
  
  #write.table(full_DT, "fix_duration_removals.txt", row.names=FALSE)
  
  print("FIX DURATION EXCLUSIONS")
  print(full_DT)
  
  # NOW RUN THE MEGA BEAST ON THE MAIN FIXATION REPORT AND RETURN IT
  final_DT <- fix_DT[eval(whereExprParsed),]
  
  return(data.frame(final_DT))
  
}
