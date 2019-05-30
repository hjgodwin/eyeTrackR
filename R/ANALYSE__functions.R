# GENERIC FUNCTION FOR CALCULATING MEANS OF ALL SORTS #############################################################################################################################################################
analyse.calculate.means <- function(fixreport_df, aggregation_column_list, output_column_expression, final_output_column_expression, 
                                    spss, dvColumnName, prefixLabel = "", debug=FALSE){
    
  # CREATE DATA TABLE
  fix_DT <- data.table(fixreport_df)
  setkey(fix_DT, RECORDING_SESSION_LABEL, TRIAL_INDEX)
  
  # CREATE BY-TRIAL MEANS
  aggExpr <- "list(RECORDING_SESSION_LABEL, TRIAL_INDEX "
  finalAggExpr <- "list(RECORDING_SESSION_LABEL "
  graphAggExpr <- "list("
  
  # IF ITS NOT A BLANK COLUMN LIST
  if (length(aggregation_column_list) > 0){
    
    for (i in seq(1:length(aggregation_column_list))){    
      aggExpr <- paste(aggExpr, ", ", aggregation_column_list[i], sep="")
      finalAggExpr <- paste(finalAggExpr, ", ", aggregation_column_list[i], sep="")
      
      # IF THIS IS AFTER THE FIRST ONE, ADD A COMMA FOR THE GRAPHS WHICH HAVE NO RECORDING SESSION LABEL
      if (i>1){
        graphAggExpr <- paste(graphAggExpr, ", ", sep="")
      }
      
      graphAggExpr <- paste(graphAggExpr, aggregation_column_list[i], sep="")
    }
  }
  
  # CLOSE UP THE AGG LISTS
  aggExpr <- paste(aggExpr, ")", sep="")
  finalAggExpr <- paste(finalAggExpr, ")", sep="")
  graphAggExpr <- paste(graphAggExpr, ")", sep="")
  
  # PARSE THE EXPRESSIONS
  aggExprParsed <- parse(text = aggExpr)
  ocExprParsed <- parse(text = output_column_expression)
    
  out_DT <- fix_DT[,
                   eval(ocExprParsed),
                   eval(aggExprParsed)]
  
  if (debug==TRUE){
    print(out_DT)
  }
  
  # PARSE THE MEAN OF THE MEANS
  finalAggExprParsed <- parse(text = finalAggExpr)
  focParsed <- parse(text = final_output_column_expression)
  
  final_DT <- out_DT[,
                     eval(focParsed),
                     eval(finalAggExprParsed)]
    
  # CALCULATE MEANS FOR A GRAPH
  grExprParsed <- parse(text = graphAggExpr)
  graphmeansParsed <- parse(text = paste("list('AVERAGE' = .Internal(mean(", dvColumnName, ")),",
                                         "'SE' = sqrt(var(", dvColumnName, ")/length(", dvColumnName, ")))", sep=""))
                            
  graphs_DT <- final_DT[,
                        eval(graphmeansParsed),
                        eval(grExprParsed)]  
    
  if (spss==FALSE){  
    
    final = list("bytrial" = out_DT,
                 "byppt" = final_DT,
                 "graphs" = graphs_DT)
    
    return(final)
  }
  
  if (spss==TRUE){
        
    df <- data.frame(final_DT)
    df$RECORDING_SESSION_LABEL <- as.factor(df$RECORDING_SESSION_LABEL)
    
    wideExpr <- paste("spss_df <- data.frame(cast(df, RECORDING_SESSION_LABEL", sep="")
    
    for (i in seq(1:length(aggregation_column_list))){
      
      if(i < 2){
        wideExpr <- paste(wideExpr, "~", aggregation_column_list[i])
      }
      
      if(i > 1){
        wideExpr <- paste(wideExpr, "*", aggregation_column_list[i])
      }
      
    }
    
    wideExpr <- paste(wideExpr, ", value=c('", dvColumnName, "')))", sep="")
        
    wideExprParsed <- parse(text = wideExpr)
    
    eval(wideExprParsed)
    
    write.table(spss_df, paste(prefixLabel, "_", dvColumnName, ".txt", sep=""), row.names=FALSE)
    
    return(spss_df)
    
  }
}

#############################################################################################################################################################
# MEAN FIX DURATION - WORKS WITH GLOBAL AND LOCAL #############################################################################################################################################################
analyse.fix.duration <- function(fixreport_df, aggregation_column_list, spss=FALSE, prefixLabel=""){
  
  # SET UP OUTPUT COLUMN EXPRESSION
  ocExpr <- "list('MEAN_FIX_DURATION' = .Internal(mean(CURRENT_FIX_DURATION)))"
  
  # SET UP FINAL OUTPUT COLUMN EXPRESSION
  focExpr <- "list('MEAN_FIX_DURATION' = .Internal(mean(MEAN_FIX_DURATION)))"
  
  # DV COLUMN NAME
  dvColumnName = "MEAN_FIX_DURATION"
  
  # SEND ON TO CALCULATE
  out_df <- analyse.calculate.means(fixreport_df=fixreport_df, aggregation_column_list=aggregation_column_list, 
                                      output_column_expression=ocExpr, final_output_column_expression=focExpr, 
                                      spss, dvColumnName, prefixLabel)
  
  return(out_df)
}
#############################################################################################################################################################

#############################################################################################################################################################
# MEAN FIX COUNT - WORKS WITH GLOBAL AND LOCAL #############################################################################################################################################################
analyse.fix.count <- function(fixreport_df, aggregation_column_list, spss=FALSE, prefixLabel=""){
  
  # SET UP OUTPUT COLUMN EXPRESSION
  ocExpr <- "list('MEAN_FIX_COUNT' = length(CURRENT_FIX_INDEX))"
  
  # SET UP FINAL OUTPUT COLUMN EXPRESSION
  focExpr <- "list('MEAN_FIX_COUNT' = .Internal(mean(MEAN_FIX_COUNT)))"
  
  # DV COLUMN NAME
  dvColumnName = "MEAN_FIX_COUNT"
  
  # SEND ON TO CALCULATE
  out_df <- analyse.calculate.means(fixreport_df=fixreport_df, aggregation_column_list=aggregation_column_list, 
                                    output_column_expression=ocExpr, final_output_column_expression=focExpr, 
                                    spss, dvColumnName, prefixLabel)
  
  return(out_df)
}
#############################################################################################################################################################

#############################################################################################################################################################
# TOTAL FIX TIME (DURATION) - WORKS WITH GLOBAL AND LOCAL #############################################################################################################################################################
analyse.fix.totaltime <- function(fixreport_df, aggregation_column_list, spss=FALSE, prefixLabel=""){
  
  # SET UP OUTPUT COLUMN EXPRESSION
  ocExpr <- "list('MEAN_FIX_TOTAL_TIME' = sum(CURRENT_FIX_DURATION))"
  
  # SET UP FINAL OUTPUT COLUMN EXPRESSION
  focExpr <- "list('MEAN_FIX_TOTAL_TIME' = .Internal(mean(MEAN_FIX_TOTAL_TIME)))"
  
  # DV COLUMN NAME
  dvColumnName = "MEAN_FIX_TOTAL_TIME"
  
  # SEND ON TO CALCULATE
  out_df <- analyse.calculate.means(fixreport_df=fixreport_df, aggregation_column_list=aggregation_column_list, 
                                    output_column_expression=ocExpr, final_output_column_expression=focExpr, 
                                    spss, dvColumnName, prefixLabel)
  
  return(out_df)
}
#############################################################################################################################################################

#############################################################################################################################################################
# VISIT COUNT - WORKS WITH GLOBAL AND LOCAL #############################################################################################################################################################
analyse.visit.count <- function(fixreport_df, aggregation_column_list, spss=FALSE, prefixLabel=""){
  
  # SET UP OUTPUT COLUMN EXPRESSION
  ocExpr <- "list('VISIT_COUNT' = max(CURRENT_FIX_INTEREST_AREA_RUN_ID))"
  
  # SET UP FINAL OUTPUT COLUMN EXPRESSION
  focExpr <- "list('MEAN_VISIT_COUNT' = .Internal(mean(VISIT_COUNT)))"
  
  # DV COLUMN NAME
  dvColumnName = "MEAN_VISIT_COUNT"
  
  # SEND ON TO CALCULATE
  out_df <- analyse.calculate.means(fixreport_df=fixreport_df, aggregation_column_list=aggregation_column_list, 
                                    output_column_expression=ocExpr, final_output_column_expression=focExpr, 
                                    spss, dvColumnName, prefixLabel)
  
  return(out_df)
}
#############################################################################################################################################################

#############################################################################################################################################################
# TOTAL FIX TIME (DURATION) - WORKS WITH GLOBAL AND LOCAL #############################################################################################################################################################
analyse.gaze.firstpass_duration <- function(fixreport_df, aggregation_column_list, spss=FALSE, prefixLabel=""){
  
  # SET UP OUTPUT COLUMN EXPRESSION
  ocExpr <- "list('FIRSTPASS_DURATION' = sum(CURRENT_FIX_DURATION[CURRENT_FIX_INTEREST_AREA_RUN_ID==1]))"
  
  # SET UP FINAL OUTPUT COLUMN EXPRESSION
  focExpr <- "list('MEAN_FIRSTPASS_DURATION' = .Internal(mean(FIRSTPASS_DURATION)))"
  
  # DV COLUMN NAME
  dvColumnName = "MEAN_FIRSTPASS_DURATION"
  
  # SEND ON TO CALCULATE
  out_df <- analyse.calculate.means(fixreport_df=fixreport_df, aggregation_column_list=aggregation_column_list, 
                                    output_column_expression=ocExpr, final_output_column_expression=focExpr, 
                                    spss, dvColumnName, prefixLabel)
  
  return(out_df)
}
#############################################################################################################################################################

#############################################################################################################################################################
# TOTAL FIX TIME (DURATION) - WORKS WITH GLOBAL AND LOCAL #############################################################################################################################################################
analyse.gaze.firstpass_nfixes <- function(fixreport_df, aggregation_column_list, spss=FALSE, prefixLabel=""){
  
  # SET UP OUTPUT COLUMN EXPRESSION
  ocExpr <- "list('FIRSTPASS_NFIXES' = length(CURRENT_FIX_INDEX[CURRENT_FIX_INTEREST_AREA_RUN_ID==1]))"
  
  # SET UP FINAL OUTPUT COLUMN EXPRESSION
  focExpr <- "list('MEAN_FIRSTPASS_NFIXES' = .Internal(mean(FIRSTPASS_NFIXES)))"
  
  # DV COLUMN NAME
  dvColumnName = "MEAN_FIRSTPASS_NFIXES"
  
  # SEND ON TO CALCULATE
  out_df <- analyse.calculate.means(fixreport_df=fixreport_df, aggregation_column_list=aggregation_column_list, 
                                    output_column_expression=ocExpr, final_output_column_expression=focExpr, 
                                    spss, dvColumnName, prefixLabel)
  
  return(out_df)
}
#############################################################################################################################################################


#############################################################################################################################################################
# TOTAL FIX TIME (DURATION) - WORKS WITH GLOBAL AND LOCAL #############################################################################################################################################################
analyse.sac.amplitude<- function(fixreport_df, aggregation_column_list, spss=FALSE, prefixLabel=""){
  
  # REMOVE BLANK SAC AMPLITUDE VALUES
  fixreport_df <- fixreport_df[fixreport_df$NEXT_SAC_AMPLITUDE!='.',]
  fixreport_df$NEXT_SAC_AMPLITUDE <- as.numeric(as.character(fixreport_df$NEXT_SAC_AMPLITUDE))
  
  # SET UP OUTPUT COLUMN EXPRESSION
  ocExpr <- "list('SAC_AMPLITUDE' = .Internal(mean(NEXT_SAC_AMPLITUDE)))"
  
  # SET UP FINAL OUTPUT COLUMN EXPRESSION
  focExpr <- "list('MEAN_SAC_AMPLITUDE' = .Internal(mean(SAC_AMPLITUDE)))"
  
  # DV COLUMN NAME
  dvColumnName = "MEAN_SAC_AMPLITUDE"
  
  # SEND ON TO CALCULATE
  out_df <- analyse.calculate.means(fixreport_df=fixreport_df, aggregation_column_list=aggregation_column_list, 
                                    output_column_expression=ocExpr, final_output_column_expression=focExpr, 
                                    spss, dvColumnName, prefixLabel)
  
  return(out_df)
}
#############################################################################################################################################################

# FIRST FIX DURATION #############################################################################################################################################################
analyse.fix.first_duration <- function(fixreport_df, aggregation_column_list, spss=FALSE, prefixLabel=""){
  
  # SET UP OUTPUT COLUMN EXPRESSION
  ocExpr <- "list('FIRST_FIX_DURATION' = CURRENT_FIX_DURATION[1])"
  
  # SET UP FINAL OUTPUT COLUMN EXPRESSION
  focExpr <- "list('FIRST_FIX_DURATION' = .Internal(mean(FIRST_FIX_DURATION)))"
  
  # DV COLUMN NAME
  dvColumnName = "FIRST_FIX_DURATION"
  
  # SEND ON TO CALCULATE
  out_df <- analyse.calculate.means(fixreport_df=fixreport_df, aggregation_column_list=aggregation_column_list, 
                                    output_column_expression=ocExpr, final_output_column_expression=focExpr, 
                                    spss, dvColumnName, prefixLabel)
  
  return(out_df)
}

# BEHAVIOURAL DATA #############################################################################################################################################################
analyse.behavioural.data<- function(bd_df, aggregation_column_list){
    
  # CREATE DATA TABLE
  b_DT <- data.table(bd_df)
  setkey(b_DT, RECORDING_SESSION_LABEL, TRIAL_INDEX)
  
  # CREATE BY-TRIAL MEANS
  finalAggExpr <- "list(RECORDING_SESSION_LABEL "
  
  # IF ITS NOT A BLANK COLUMN LIST
  if (length(aggregation_column_list) > 0){   
    for (i in seq(1:length(aggregation_column_list))){     
      finalAggExpr <- paste(finalAggExpr, ", ", aggregation_column_list[i], sep="")
    }
  }
  
  # CLOSE UP THE AGG LISTS AND PARSE
  finalAggExpr <- paste(finalAggExpr, ")", sep="")
  finalAggExprParsed <- parse(text = finalAggExpr)
  
  # RUN IT
  final_DT <- b_DT[,
                     list("MEDIAN_RT" = as.numeric(as.character(median(TRUE_RT[OUTCOME=="CORRECT"]))),
                          "MEAN_RT" = mean(TRUE_RT[OUTCOME=="CORRECT"]),
                          "CORRECT_COUNT" = length(TRIAL_INDEX[OUTCOME=="CORRECT"]),
                          "TOTAL_TRIALS" = length(TRIAL_INDEX)
                          ),
                     eval(finalAggExprParsed)]
  
  final_DT$ACCURACY <- final_DT$CORRECT_COUNT / final_DT$TOTAL_TRIALS
  
  out_df <- data.frame(final_DT)
  
  return(out_df)
}
#############################################################################################################################################################

