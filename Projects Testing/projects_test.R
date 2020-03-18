library(yaml)
library(eyeTrackR)

# PROJECT FUNCTIONS ##############################################################################################################################

source(file = '../../source/R/PROJECT__functions.R')

setwd("~/Dropbox/eyeTrackR Development/Github/eyeTrackR/Projects Testing/singleProject")

project.create.singleDataset(title='moo', description = 'I am a testX', directoryTitle = 'singleProject')

setwd("~/Dropbox/eyeTrackR Development/Github/eyeTrackR/Projects Testing/multiProject")

project.create.multiDataset(title='eyeTrackR Project', 
                                             description = ' MULTI', 
                                             authors = '', 
                                             website = '')

project.import.singleDataset('../singleProject/')

setwd("~/Dropbox/eyeTrackR Development/Github/eyeTrackR/Projects Testing/multiMultiProject")

project.create.multiDataset(title='eyeTrackR Project', 
                            description = ' MULTIMULTI', 
                            authors = '', 
                            website = '')

project.import.multiDataset('../multiProject/')
