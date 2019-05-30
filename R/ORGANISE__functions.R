##############################################################################################################################################################
# GETS RID OF PESKY SPACES IN THE MESSAGES
organise.message.replace_spaces <- function(message_df){
  
  message_df$CURRENT_MSG_TEXT<-str_replace(message_df$CURRENT_MSG_TEXT, " ", "_")
  
  return(message_df)
}

##############################################################################################################################################################
# GETS RID OF OFTEN UNUSED COLUMNS IN FIXREPORT
organise.fixreport.remove_rare <- function(fixreport_df){
    
  fixreport_df<-subset(fixreport_df, select=-c(CURRENT_FIX_ADJUSTED,CURRENT_FIX_BLINK_AROUND, CURRENT_FIX_END_OTHER, CURRENT_FIX_INTEREST_AREAS, CURRENT_FIX_INTEREST_AREA_DWELL_TIME,
                                               CURRENT_FIX_INTEREST_AREA_FIX_COUNT, CURRENT_FIX_INTEREST_AREA_GROUP, CURRENT_FIX_INTEREST_AREA_INDEX, CURRENT_FIX_INTEREST_AREA_X_OFFSET,
                                               CURRENT_FIX_INTEREST_AREA_Y_OFFSET, CURRENT_FIX_IS_RT_END, CURRENT_FIX_LABEL, CURRENT_FIX_PUPIL, CURRENT_FIX_REFIX_INTEREST_AREA,
                                               CURRENT_FIX_REFIX_PREV_INTEREST_AREA, CURRENT_FIX_RUN_DWELL_TIME, CURRENT_FIX_START_OTHER, CURRENT_FIX_TRIAL_SPAN, CURRENT_FIX_X_OTHER,
                                               CURRENT_FIX_X_RESOLUTION, CURRENT_FIX_Y_OTHER, CURRENT_FIX_Y_RESOLUTION, DATA_FILE, EYE_USED, IP_END_TIME, IP_LABEL, 
                                               IP_START_TIME, LAST_BUTTON_PRESSED, LAST_BUTTON_PRESSED_TIME, LAST_BUTTON_RELEASED, LAST_BUTTON_RELEASED_TIME, LAST_BUTTON_TIME,
                                               NEXT_FIX_BLINK_AROUND, NEXT_FIX_END_OTHER, NEXT_FIX_INTEREST_AREAS, NEXT_FIX_INTEREST_AREA_DWELL_TIME, NEXT_FIX_INTEREST_AREA_FIX_COUNT,
                                               NEXT_FIX_INTEREST_AREA_GROUP, NEXT_FIX_INTEREST_AREA_INDEX, NEXT_FIX_INTEREST_AREA_RUN_ID, NEXT_FIX_IS_RT_END,
                                               NEXT_FIX_LABEL, NEXT_FIX_MSG_COUNT, NEXT_FIX_NEAREST_INTEREST_AREA,
                                               NEXT_FIX_PUPIL, NEXT_FIX_RUN_DWELL_TIME, NEXT_FIX_RUN_INDEX, NEXT_FIX_RUN_SIZE,
                                               NEXT_FIX_START, NEXT_FIX_START_OTHER, NEXT_FIX_TRIAL_SPAN, NEXT_FIX_X, NEXT_FIX_X_OTHER, NEXT_FIX_X_RESOLUTION, NEXT_FIX_Y, NEXT_FIX_Y_OTHER, NEXT_FIX_Y_RESOLUTION,
                                               NEXT_SAC_END_INTEREST_AREA_INDEX, NEXT_SAC_END_X_RESOLUTION, NEXT_SAC_IS_RT_END,  NEXT_SAC_LABEL,
                                               PREVIOUS_FIX_ANGLE, PREVIOUS_FIX_BLINK_AROUND, PREVIOUS_FIX_DIRECTION, PREVIOUS_FIX_DISTANCE,
                                               PREVIOUS_FIX_END, PREVIOUS_FIX_END_OTHER, PREVIOUS_FIX_INTEREST_AREAS, PREVIOUS_FIX_INTEREST_AREA_DWELL_TIME, PREVIOUS_FIX_INTEREST_AREA_FIX_COUNT,
                                               PREVIOUS_FIX_INTEREST_AREA_GROUP, PREVIOUS_FIX_INTEREST_AREA_INDEX, PREVIOUS_FIX_INTEREST_AREA_RUN_ID, PREVIOUS_FIX_IS_RT_END,
                                               PREVIOUS_FIX_LABEL, PREVIOUS_FIX_MSG_COUNT, PREVIOUS_FIX_NEAREST_INTEREST_AREA,
                                               PREVIOUS_FIX_PUPIL, PREVIOUS_FIX_RUN_DWELL_TIME, PREVIOUS_FIX_RUN_INDEX,
                                               PREVIOUS_FIX_RUN_SIZE, PREVIOUS_FIX_START, PREVIOUS_FIX_START_OTHER, PREVIOUS_FIX_TRIAL_SPAN, PREVIOUS_FIX_X, PREVIOUS_FIX_X_OTHER, PREVIOUS_FIX_X_RESOLUTION, PREVIOUS_FIX_Y
                                               ))
  
  return(fixreport_df)
}

##############################################################################################################################################################
# PICKS AND DISPLAYS A RANDOM TRIAL FOR DETAILED CHECKS
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
  
  print(mDT)
  
  return(data.frame(summary_DT))
}

##############################################################################################################################################################
# MARKS UP EACH TRIAL WHERE A MESSAGE OCCURRED WITH THE MESSAGE VALUE
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
  
  return(data.frame(joined_mDT))  
}

##############################################################################################################################################################

##############################################################################################################################################################
# RETURNS TRIALS WHERE A SPECIFIC MESSAGE IS FOUND

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
  
  return(data.frame(joined_mDT))  
}

##############################################################################################################################################################
# ORGANISE AND MARKUP FIXATION CONTINGENCIES
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
  
  return(fixreport_df)
}

##############################################################################################################################################################
# ORGANISE AND MARKUP RESPONSE DETAILS
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
  
    return(joined_df)
}

##############################################################################################################################################################
# BASIC STATS ON FIXATION CONTINGENCIES
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
  
  write.table(full_DT, "event_trials_missing.txt", row.names=FALSE)
  
  print("EVENT INCLUSIONS AND EXCLUSIONS")
  print(full_DT)
  
  # NOW RUN THE MEGA BEAST ON THE MAIN FIXATION REPORT AND RETURN IT
  final_DT <- fix_DT[eval(bigWhereExprParsed),]
  
  return(data.frame(final_DT))
}

##############################################################################################################################################################
# RETURNS THE FIRST FIXATION INDEX AND LOCATION AND DISTANCE TO CENTRE OF THE REQUESTED TIME PERIOD
organise.message.first_fixation_time_period_info <- function(fixreport_df, time_period){
  
  # SET UP KEYS
  fix_DT <- data.table(fixreport_df)
  setkey(fix_DT, RECORDING_SESSION_LABEL, TRIAL_INDEX)
  
  # SET UP WHERE EXPRESSION
  whereExpr <- paste("WITHIN_", time_period, "==TRUE", sep="")
  whereExprParsed <- parse(text = whereExpr)
  
  # SET UP COLUMN EXPRESSION
  colExpr <- paste('list("FIRST_FIX_INDEX_', time_period, '"=min(CURRENT_FIX_INDEX),',
                   '"FIRST_FIX_X_', time_period, '"=CURRENT_FIX_X[CURRENT_FIX_INDEX==min(CURRENT_FIX_INDEX)],',
                   '"FIRST_FIX_Y_', time_period, '"=CURRENT_FIX_Y[CURRENT_FIX_INDEX==min(CURRENT_FIX_INDEX)]',
                   ')', sep="")
  colExprParsed <- parse(text = colExpr)
  
  # DO THE SELECTION
  index_DT <- fix_DT[eval(whereExprParsed),
                     eval(colExprParsed),
                     list(RECORDING_SESSION_LABEL, TRIAL_INDEX)]
  
  # CENTRE DISTANCE CALCULATION
  mid_x = 1024 / 2
  mid_y = 768 / 2
  
  distExpr <- paste("index_DT$FIRST_FIX_DISTANCE_TO_CENTRE_", time_period, "<- (",
                    "sqrt((index_DT$FIRST_FIX_X_", time_period,  "-", mid_x, ")^2",
                    "+", "(index_DT$FIRST_FIX_Y_", time_period, "-", mid_y, ")^2)) / 35",
                    sep="")
  distExprParsed <- parse(text = distExpr)
  eval(distExprParsed)
  
  # JOIN UP WITH MAIN DF BEFORE RETURNING
  joined_df <- join(data.frame(fix_DT), data.frame(index_DT))    
  
  # PRINT OUT SUMMARIES
  # BIN COLUMN WONT BE ADDED TO OUTPUT
  summaryExpr <- paste("index_DT$bin <- ", "cut(index_DT$FIRST_FIX_DISTANCE_TO_CENTRE_", time_period,
                       ", breaks = c(0,1,2,2.5,3,4,5,10,200), labels = c('0-1', '1-2', '2-2.5', '2.5-3', '3-4', '4-5', '5-10', '10+'))",
                       sep="")
  summaryExprParsed <- parse(text = summaryExpr)
  eval(summaryExprParsed)
  
  print("### OVERALL SUMMARY ###")
  print(index_DT[order(bin), list("TRIAL_COUNT" = length(TRIAL_INDEX)), bin])
  
  return(joined_df)
}

##############################################################################################################################################################
# REMOVE FIXATIONS THAT WERE TOO FAR FROM THE CENTRE AT THE START OF A GIVEN TIME PERIOD
organise.message.first_fixation_time_period_removals <- function(fixreport_df, time_period, distance_limit=2.5){
  
  # SET UP KEYS
  fix_DT <- data.table(fixreport_df)
  setkey(fix_DT, RECORDING_SESSION_LABEL, TRIAL_INDEX)  
  
  full_DT <- fix_DT[, list("TRIAL_TOTAL" = length(unique(TRIAL_INDEX))),
                    list(RECORDING_SESSION_LABEL)]
  
  # SET UP WHERE EXPRESSION
  whereExpr <- paste("FIRST_FIX_DISTANCE_TO_CENTRE_", time_period, "<", distance_limit, "", sep="")
  whereExprParsed <- parse(text = whereExpr)
  
  print(whereExpr)
  
  # SET UP SELECT EXPRESSION
  selectExpr <- paste("list('FINAL_TRIAL_COUNT' = length(unique(TRIAL_INDEX)))", sep="")
  selectExprParsed <- parse(text = selectExpr)
  
  selected_DT <- fix_DT[eval(whereExprParsed),
                        eval(selectExprParsed),
                        list(RECORDING_SESSION_LABEL)]
  
  full_DT <- full_DT[selected_DT]
  
  full_DT$LOST_TRIALS <- full_DT$TRIAL_TOTAL - full_DT$FINAL_TRIAL_COUNT
  full_DT$LOST_TRIALS_PERC <- full_DT$FINAL_TRIAL_COUNT / full_DT$TRIAL_TOTAL
  
  write.table(full_DT, "first_fix_distance_limits_removals.txt", row.names=FALSE)
  
  print("FIRST FIX DISTANCE INCLUSIONS AND EXCLUSIONS")
  print(full_DT)
  
  # NOW RUN THE MEGA BEAST ON THE MAIN FIXATION REPORT AND RETURN IT
  final_DT <- fix_DT[eval(whereExprParsed),]
  
  return(data.frame(final_DT))
  
}

##############################################################################################################################################################
# REMOVE FIXATIONS THAT WERE TOO FAR FROM THE CENTRE AT THE START OF A GIVEN TIME PERIOD
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
  
  write.table(full_DT, "fix_duration_removals.txt", row.names=FALSE)
  
  print("FIX DURATION EXCLUSIONS")
  print(full_DT)
  
  # NOW RUN THE MEGA BEAST ON THE MAIN FIXATION REPORT AND RETURN IT
  final_DT <- fix_DT[eval(whereExprParsed),]
  
  return(data.frame(final_DT))
  
}

##############################################################################################################################################################
# MODIFIES THE GAZE INDEX TO A NEW VALUE
organise.gaze.matrix <- function(fixreport_df){
  
  fixreport_df$RECORDING_SESSION_LABEL <- as.character(fixreport_df$RECORDING_SESSION_LABEL)
  fixreport_df$TRIAL_INDEX <- as.numeric(as.character(fixreport_df$TRIAL_INDEX))
  fixreport_df$CURRENT_FIX_INTEREST_AREA_LABEL <- as.character(fixreport_df$CURRENT_FIX_INTEREST_AREA_LABEL)
  
  fixreport_df <- data.table(fixreport_df)
  setkey(fixreport_df, RECORDING_SESSION_LABEL, TRIAL_INDEX)
  
  ppt_list <- unique(fixreport_df$RECORDING_SESSION_LABEL)
  
  #ppt_df <- do.call("rbind", parLapply(c1, ppt_list, function(ppt){
  ppt_df <- do.call("rbind", lapply(ppt_list, function(ppt){
    
    print(ppt)
    
    trial_list <- unique(fixreport_df$TRIAL_INDEX[fixreport_df$RECORDING_SESSION_LABEL==ppt])
    
    trial_df <- do.call("rbind", lapply(trial_list, function(trial){
      
      # CREATE A TEMPORARY DF TO HOLD EVERYTHING
      temp_df <- fixreport_df[fixreport_df$RECORDING_SESSION_LABEL==ppt & fixreport_df$TRIAL_INDEX==trial,]
      temp_df$GAZE_INDEX <- NA
      RUNNING_GAZE_INDEX <- 0
      
      # SETUP THE MATRIX
      running_IA <- "nothing"
      visited_matrix <- data.frame("IA_NAME"= unique(temp_df$CURRENT_FIX_INTEREST_AREA_LABEL),
                                   "RUNNING_GAZE_INDEX" = rep(0, length(unique(temp_df$CURRENT_FIX_INTEREST_AREA_LABEL))))
      
      fix_index_list <- unique(temp_df$CURRENT_FIX_INDEX)
      
      for (i in seq(1:length(fix_index_list))){
        
        # IF THIS IS A NEW INTEREST AREA
        if (temp_df$CURRENT_FIX_INTEREST_AREA_LABEL[temp_df$CURRENT_FIX_INDEX==fix_index_list[i]] != running_IA){
          
          # UPDATE CURRENT IA
          running_IA <- temp_df$CURRENT_FIX_INTEREST_AREA_LABEL[temp_df$CURRENT_FIX_INDEX==fix_index_list[i]]
          
          # IF THIS IS NOT A NON-IA FIX
          if (running_IA !='.'){
            
            # UPDATE MATRIX
            visited_matrix$RUNNING_GAZE_INDEX[visited_matrix$IA_NAME==running_IA] <- visited_matrix$RUNNING_GAZE_INDEX[visited_matrix$IA_NAME==running_IA] + 1
            
            # RETURN THIS TO THE DATA FRAME
            temp_df$GAZE_INDEX[temp_df$CURRENT_FIX_INDEX==fix_index_list[i]] <- visited_matrix$RUNNING_GAZE_INDEX[visited_matrix$IA_NAME==running_IA]
            
            # INCREMENT THE RUNNING GAZE INDEX
            RUNNING_GAZE_INDEX = RUNNING_GAZE_INDEX + 1
            temp_df$RUNNING_GAZE_INDEX[temp_df$CURRENT_FIX_INDEX==fix_index_list[i]] = RUNNING_GAZE_INDEX
            
          }
          
          if (running_IA=='.'){
            temp_df$RUNNING_GAZE_INDEX[temp_df$CURRENT_FIX_INDEX==fix_index_list[i]] = NA            
          }
        }
        
        # IF THIS IS THE SAME IA
        if (temp_df$CURRENT_FIX_INTEREST_AREA_LABEL[temp_df$CURRENT_FIX_INDEX==fix_index_list[i]] == running_IA){
          
          # IF THIS IS NOT A NON-IA FIX
          if (running_IA !='.'){
          
            # RETURN THIS TO THE DATA FRAME
            temp_df$GAZE_INDEX[temp_df$CURRENT_FIX_INDEX==fix_index_list[i]] <- visited_matrix$RUNNING_GAZE_INDEX[visited_matrix$IA_NAME==running_IA]
            
            # SET RUNNING GAZE INDEX
            temp_df$RUNNING_GAZE_INDEX[temp_df$CURRENT_FIX_INDEX==fix_index_list[i]] = RUNNING_GAZE_INDEX
          
          }
          
          if (running_IA=='.'){
            temp_df$RUNNING_GAZE_INDEX[temp_df$CURRENT_FIX_INDEX==fix_index_list[i]] = NA            
          }
        }
        
      }
      
      return(temp_df)
    })) #lapply for trial ends here
    
    return(trial_df)
    
    })) #lapply for ppt ends here
  
  
  return(data.frame(ppt_df))
  }

############################################################################################################################################################################################################################################################################################################################


##############################################################################################################################################################
# MODIFIES THE GAZE INDEX TO A NEW VALUE
organise.gaze.matrix_nearest <- function(fixreport_df, distance_max =2.5){
  
  fixreport_df$RECORDING_SESSION_LABEL <- as.character(fixreport_df$RECORDING_SESSION_LABEL)
  fixreport_df$TRIAL_INDEX <- as.numeric(as.character(fixreport_df$TRIAL_INDEX))
  fixreport_df$CURRENT_FIX_INTEREST_AREA_LABEL_BACKUP <- fixreport_df$CURRENT_FIX_INTEREST_AREA_LABEL
  fixreport_df$CURRENT_FIX_INTEREST_AREA_LABEL <- as.character(fixreport_df$CURRENT_FIX_NEAREST_INTEREST_AREA_LABEL)
  fixreport_df$CURRENT_FIX_NEAREST_INTEREST_AREA_DISTANCE <- as.numeric(as.character(fixreport_df$CURRENT_FIX_NEAREST_INTEREST_AREA_DISTANCE))
  fixreport_df$CURRENT_FIX_INTEREST_AREA_LABEL[fixreport_df$CURRENT_FIX_NEAREST_INTEREST_AREA_DISTANCE>=2.5] <- "."
  
  fixreport_df <- data.table(fixreport_df)
  setkey(fixreport_df, RECORDING_SESSION_LABEL, TRIAL_INDEX)
  
  ppt_list <- unique(fixreport_df$RECORDING_SESSION_LABEL)
  
  #ppt_df <- do.call("rbind", parLapply(c1, ppt_list, function(ppt){
  ppt_df <- do.call("rbind", lapply(ppt_list, function(ppt){
    
    print(ppt)
    
    trial_list <- unique(fixreport_df$TRIAL_INDEX[fixreport_df$RECORDING_SESSION_LABEL==ppt])
    
    trial_df <- do.call("rbind", lapply(trial_list, function(trial){
      
      # CREATE A TEMPORARY DF TO HOLD EVERYTHING
      temp_df <- fixreport_df[fixreport_df$RECORDING_SESSION_LABEL==ppt & fixreport_df$TRIAL_INDEX==trial,]
      temp_df$GAZE_INDEX <- NA
      RUNNING_GAZE_INDEX <- 0
      
      # SETUP THE MATRIX
      running_IA <- "nothing"
      visited_matrix <- data.frame("IA_NAME"= unique(temp_df$CURRENT_FIX_INTEREST_AREA_LABEL),
                                   "RUNNING_GAZE_INDEX" = rep(0, length(unique(temp_df$CURRENT_FIX_INTEREST_AREA_LABEL))))
      
      fix_index_list <- unique(temp_df$CURRENT_FIX_INDEX)
      
      for (i in seq(1:length(fix_index_list))){
        
        # IF THIS IS A NEW INTEREST AREA
        if (temp_df$CURRENT_FIX_INTEREST_AREA_LABEL[temp_df$CURRENT_FIX_INDEX==fix_index_list[i]] != running_IA){
          
          # UPDATE CURRENT IA
          running_IA <- temp_df$CURRENT_FIX_INTEREST_AREA_LABEL[temp_df$CURRENT_FIX_INDEX==fix_index_list[i]]
          
          # IF THIS IS NOT A NON-IA FIX
          if (running_IA !='.'){
            
            # UPDATE MATRIX
            visited_matrix$RUNNING_GAZE_INDEX[visited_matrix$IA_NAME==running_IA] <- visited_matrix$RUNNING_GAZE_INDEX[visited_matrix$IA_NAME==running_IA] + 1
            
            # RETURN THIS TO THE DATA FRAME
            temp_df$GAZE_INDEX[temp_df$CURRENT_FIX_INDEX==fix_index_list[i]] <- visited_matrix$RUNNING_GAZE_INDEX[visited_matrix$IA_NAME==running_IA]
            
            # INCREMENT THE RUNNING GAZE INDEX
            RUNNING_GAZE_INDEX = RUNNING_GAZE_INDEX + 1
            temp_df$RUNNING_GAZE_INDEX[temp_df$CURRENT_FIX_INDEX==fix_index_list[i]] = RUNNING_GAZE_INDEX
            
          }
          
          if (running_IA=='.'){
            temp_df$RUNNING_GAZE_INDEX[temp_df$CURRENT_FIX_INDEX==fix_index_list[i]] = NA            
          }
        }
        
        # IF THIS IS THE SAME IA
        if (temp_df$CURRENT_FIX_INTEREST_AREA_LABEL[temp_df$CURRENT_FIX_INDEX==fix_index_list[i]] == running_IA){
          
          # IF THIS IS NOT A NON-IA FIX
          if (running_IA !='.'){
            
            # RETURN THIS TO THE DATA FRAME
            temp_df$GAZE_INDEX[temp_df$CURRENT_FIX_INDEX==fix_index_list[i]] <- visited_matrix$RUNNING_GAZE_INDEX[visited_matrix$IA_NAME==running_IA]
            
            # SET RUNNING GAZE INDEX
            temp_df$RUNNING_GAZE_INDEX[temp_df$CURRENT_FIX_INDEX==fix_index_list[i]] = RUNNING_GAZE_INDEX
            
          }
          
          if (running_IA=='.'){
            temp_df$RUNNING_GAZE_INDEX[temp_df$CURRENT_FIX_INDEX==fix_index_list[i]] = NA            
          }
        }
        
      }
      
      return(temp_df)
    })) #lapply for trial ends here
    
    return(trial_df)
    
  })) #lapply for ppt ends here
  
  
  return(data.frame(ppt_df))
  }

############################################################################################################################################################################################################################################################################################################################


# SAVES RT AND ACCURACY SPLIT BY SPECIFIED COLUMNS #############################################################################################################################################################
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
  
  write.table(data.frame(b_DT), paste(prefix_label, "behavioural_data.txt", sep="_"), row.names=FALSE)

  return(b_DT)
}

############################################################################################################################################################################################################################################################################################################################
















# 
# 
# 
# 
# ############################################################################################################################################
# ## APPENDIX
# 
# 
# 
# # WHERE CLAUSE USAGE
# 
# mew <- data.table(fooX)
# setkey(mew, RECORDING_SESSION_LABEL, TRIAL_INDEX)
# 
# mew[TRIAL_INDEX==1, 
#     list("MEAN_FIX_DUR" = mean(CURRENT_FIX_DURATION), "FOO" = WITHIN_SYNCTIME[1]), 
#     list(RECORDING_SESSION_LABEL, TRIAL_INDEX)]
# 
# mew[WITHIN_SYNCTIME==TRUE, 
#     list("MEAN_FIX_DUR" = mean(CURRENT_FIX_DURATION), "FOO" = WITHIN_SYNCTIME[1]), 
#     list(RECORDING_SESSION_LABEL, TRIAL_INDEX)]
# 
# # GETTING EVALUATIONS TOW ORK
# argh <- function(df){
#   
#   t <- parse(text = "df$BLAH = 1")
#   
#   eval(t)
#   
#   return(df)
# }
# 
# blah <- argh(fixreport)
# 
# moo <- data.frame("x"=c(1,2,3,4), "p"=c(0,55,99,109), "blah"=c("A", "B", "C", "D"))
# yew <- data.frame("x"=c(1,2), "p"=c(0,55), "flax" = c("X","V"))
# 
# mDT <- data.table(moo)
# yDT <- data.table(yew)
# 
# setkey(mDT, x, p)
# setkey(yDT, x, p)
# 
# mDT[yDT, mult='all', nomatch=NA]
# 
# yDT[mDT]
# 
# merge(mDT, yDT, all.x=TRUE)
# 
# 
# 
# 
# 
# ############
# foo <- data.table(foo)
# setkey(foo, RECORDING_SESSION_LABEL, TRIAL_INDEX)
# 
# foo[,
#     list("FIX_CROSS_TIME" = CURRENT_MSG_TIME[CURRENT_MSG_TEXT="FIX_CROSS_TIME"]),
#     list(RECORDING_SESSION_LABEL, TRIAL_INDEX)]
# 
# 
# 
# # ##############################################################################################################################################################
# # # CREATE EVENT MATRIX LATIN SQUARE
# # organise.message.event_matrix <- function(ordered_message_list){
# #   
# #   # CREATE LATIN SQUARE MATRIX OF ALL POSSIBLE EVENT CONTINGENCIES
# #   event_matrix <- data.frame()
# #   event_matrix[1, "START"] <- "NONE"
# #   event_matrix[1, "END"] <- ordered_message_list[1]
# #   
# #   for (i in seq(1:(length(ordered_message_list)-1))){
# #     print("NEW")
# #     print(ordered_message_list[i])
# #     
# #     print("REMAINING")
# #     
# #     remaining <- ordered_message_list[(i+1):length(ordered_message_list)]
# #     
# #     print(remaining)
# #     
# #     for (j in seq(1:length(remaining))){
# #       newrow_id <- nrow(event_matrix) + 1
# #       event_matrix[newrow_id, "START"] <- ordered_message_list[i]
# #       event_matrix[newrow_id, "END"] <- remaining[j]
# #     }
# #   }
# #   
# #   # ADD A FINAL ROW FOR THE FINAL EVENT AND BEYOND
# #   finalrow_id <- nrow(event_matrix) + 1
# #   event_matrix[finalrow_id, "START"] <- ordered_message_list[length(ordered_message_list)]
# #   event_matrix[finalrow_id, "END"] <- "INFINITY"
# #   
# #   print(event_matrix)
# #   
# #   return(event_matrix)
# # }
# 
# #organise.message.event_matrix(list("FIX_CROSS", "FIX_TIMER", "SYNCTIME", "TRIAL_RESULT_0"))