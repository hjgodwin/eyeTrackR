library(yaml)
library(eyeTrackR)

# FUNCTIONS ##############################################################################################################################

setwd("~/Dropbox/eyeTrackR Development/Github/eyeTrackR/Projects Testing/singleProject")


# data("fixationreport")
# fwrite(fixationreport, 'data/fixationreport.txt')
# data("messagereport")
# fwrite(messagereport, 'data/messagereport.txt')

source(file = '../../source/R/PROJECT__functions.R')
source(file = '../../source/R/PIPELINE__functions.R')

fixationreport <- fread('data/fixationreport.txt')
messagereport <- fread('data/messagereport.txt')

project.pipeline.createOrganise(authors = c(''),
                                message.markupMultiple = "list('DISPLAY_START','DISPLAY_CHANGE')",
                                responses.markup = "CORRECT_RESPONSE",
                                message.fix_contingencies = "list('DISPLAY_START','DISPLAY_CHANGE','RESPONSE_TIME')",
                                message.removals = "list('DISPLAY_CHANGE', 'RESPONSE_TIME')",
                                exclusions.fix_durations = "list(60,1200)",
                                homeProject = '', 
                                website = '')

project.pipeline.run()


  