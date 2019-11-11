library(eyeTrackR)
library(data.table)

data(fixationreport)
data(messagereport)

#### ORGANISE WALKTHROUGH ####################################################################################################

# REPLACE SPACES IN MESSAGES
messagereport <- organise.message.replace_spaces(messagereport)

# TAKE A LOOK
print(organise.message.descriptives(messagereport))

# MARKUP FIXATION REPORT
fixationreport <- organise.message.markup(message_df= messagereport,
                                          fixreport_df = fixationreport,
                                          message="DISPLAY_START")

fixationreport <- organise.message.markup(message_df= messagereport,
                                          fixreport_df = fixationreport,
                                          message="DISPLAY_CHANGE")

# NOW DO ACCURACY AND RT MARKUP
fixationreport <- organise.responses.markup(fixationreport, "CORRECT_RESPONSE")

#ALTERNATIVE MARKUP FOR KEYBOARD RESPONSE
fixationreport <- organise.message.markup(message_df=messagereport,
                                          fixreport_df = fixationreport, message="KEYBOARD_PRESENT")
fixationreport <- organise.message.markup(message_df=messagereport,
                                          fixreport_df = fixationreport, message="KEYBOARD_ABSENT")

fixationreport<-as.data.table(fixationreport)

fixationreport[is.na(KEYBOARD_PRESENT)==F,RESPONSE:='PRESENT',]
fixationreport[is.na(KEYBOARD_ABSENT)==F,RESPONSE:='ABSENT',]

fixationreport[PRESENCE==RESPONSE,OUTCOME:='CORRECT',]
fixationreport[PRESENCE!=RESPONSE,OUTCOME:='INCORRECT',]

fixationreport[is.na(KEYBOARD_ABSENT)==F,RESPONSE_TIME:=KEYBOARD_ABSENT,]
fixationreport[is.na(KEYBOARD_PRESENT)==F,RESPONSE_TIME:=KEYBOARD_PRESENT,]

# NOW MARK UP FIXATION CONTINGENCIES
fixationreport<-organise.message.fix_contingencies(fixationreport,
                                                   list("DISPLAY_START",
                                                        "DISPLAY_CHANGE",
                                                        "RESPONSE_TIME"))

# SET UP TRUE RT
fixationreport[,TRUE_RT := RESPONSE_TIME-DISPLAY_START,]

behaviouralData <- analyse.behavioural.data(fixationreport,
                                            aggregation_column_list = list('TRIALTYPE_TEXT'))

# RANDOM TRIAL TO CHECK THINGS OUT
print(organise.checks.random_trial(fixationreport))

# FIX CONTINGENCIES
print(organise.contingencies.descriptives(fixationreport))

# REMOVE TRIALS THAT LACKED A DISPLAY CHANGE AND/OR A RESPONSE TIME
messageRemovals <- organise.message.removals(fixreport_df=fixationreport,
                                             required_message_list=list("DISPLAY_CHANGE", "RESPONSE_TIME"))

# LOOK AT MESSAGE REMOVALS
print(messageRemovals[[1]])

# GRAB THE FIXATION REPORT WITH TRIALS REMOVED
fixMessagesRemoved <- messageRemovals[[2]]

# THIS SHOWS WE HAVE NO UNCLASSIFIED FIXATIONS, GOOD!
print(organise.contingencies.descriptives(fixMessagesRemoved))

# REMOVALS BASED ON FIXATION DURATIONS
durationRemovals <- organise.exclusions.fix_durations(fixreport_df=fixMessagesRemoved)

# SUMMARY STATS OF HOW MANY FIXATIONS REMOVED PER PARTICIPANT
durationsRemoved <- durationRemovals[[1]]

# FINAL DATASET WHICH CAN BE ANALYSED
finalDT <- durationRemovals [[2]]

# THAT'S IT! LET'S NOW SAVE THE FINAL THING ##############################################################################
write.table(finalDT, "df_final_for_analysis.txt", row.names=FALSE)


#### ANALYSE WALKTHROUGH ####################################################################################################

library(ggplot2)
library(cowplot)

fixCounts <- analyse.fix.count(fixationreport,
                               aggregation_column_list = list('TRIALTYPE_TEXT'))

ggplot(fixCounts$graphs)+
  aes(x=TRIALTYPE_TEXT, y=AVERAGE)+
  geom_point(size=2)+
  scale_y_continuous('Mean Fixation Count')+
  scale_x_discrete('Trialtype Text')+
  geom_errorbar(aes(ymin=AVERAGE-SE, ymax=AVERAGE+SE), width=0.1)
