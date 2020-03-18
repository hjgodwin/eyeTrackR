
# MARKUP MUTLIPLE MESSAGES - NEW FUNCTION FOR ORGANISE__

organise.message.markupMultiple <- function(message_df, fixreport_df, messages, show_working=FALSE){
  
  outputDT <- data.table(fixreport_df)
  
  for (i in seq(1:length(messages))){
    outputDT <- organise.message.markup(message_df, fixreport_df, messages[[i]], show_working=show_working) 
  }
  
  return(outputDT)
  
}


# CREATE DATA PROCESSING PIPELINE - GENERIC #############################################################################################################################################################
#' Function for creating a generic data processing pipeline.
#'
#' @param label Pipeline label.
#' @param description Description of the pipeline and what it does or is for.
#' @param authors Authors of the pipeline, in a list.
#' @param functions Functions to feed into the pipeline, in order.
#' @param functionParamaters Parameters to feed each function in the pipeline, in order.
#' @param homeProject Name of the project where this pipeline was first created.
#' @param website Web address for the project that gave rise to this pipeline.
#'
#' @examples
project.pipeline.create <- function(label = 'pipeline', description = '',
                        authors = c(''), functions = c(''), functionInputs = c(''),
                        functionOutputs =c(''),
                        homeProject = '', website = ''){
  
  if (dir.exists('pipelines/')==T){
  
    pipelineFilename <- paste('pipelines/', label, '.yaml', sep='')
    
    if(file.exists(pipelineFilename)==T){
      message('Unable to create new data processing pipeline: that pipeline already exists.')
    }
    
    if(file.exists(pipelineFilename)==F){
    
    pipelineYaml <- file(pipelineFilename, 'w')
    
    baseList <- c(label=label,
                  description = description,
                  authors = authors)
    
    for (i in seq(1:length(functions))){
      baseList <- c(baseList, paste('FUNCTION__', functions[[i]], ':', functionInputs[[i]], sep=''))
    }

    for (i in seq(1:length(functions))){
      baseList <- c(baseList, paste('OUTPUT:',functionOutputs[[i]], sep=''))
    }
      
    baseList <- c(baseList, website = website,
                  eyeTrackR = as.character(packageVersion("eyeTrackR")[[1]]))
    
    write_yaml(baseList, pipelineYaml)
    
    close(pipelineYaml)
    }
  }
  
  if (dir.exists('pipelines/')==F){
    message('Unable to find a pipelines directory.', call.=F) 
  }

}

#  CREATE DATA PROCESSING PIPELINE - ORGANISE FUNCTIONS #############################################################################################################################################################
#' Function for creating a data processing pipeline that focuses on ORGANISE functions.
#'
#' @param authors Authors of the pipeline, in a list.
#' @param message.markup What messages to mark up, in order.
#' @param responses.markup The column in the dataset that houses the correct response for each trial.
#' @param message.fix_contingencies What messages to mark up as part of fixation contingencies, in order.
#' @param message.removals A list of the messages that must appear in a trial for it to be kept within the dataset.
#' @param exclusions.fix_durations The lower and upper limits of fixation durations, in order.
#' @param homeProject Name of the project where this pipeline was first created.
#' @param website Web address for the project that gave rise to this pipeline.
#'
#' @examples
project.pipeline.createOrganise <- function(
                                    authors = c(''),
                                    message.markupMultiple = c(''),
                                    responses.markup = '',
                                    message.fix_contingencies = c(''),
                                    message.removals = c(''),
                                    exclusions.fix_durations = c(60,1200),
                                    homeProject = '', 
                                    website = ''){
  
  project.pipeline.create(label = 'markup_pipeline', 
                          description = 'Organise Functions for eyeTrackR',
                          authors = authors, 
                          functions = c('organise.message.replace_spaces', 
                                        'organise.message.markupMultiple', 
                                        'organise.responses.markup', 
                                        'organise.message.fix_contingencies', 
                                        'organise.message.removals', 
                                        'organise.exclusions.fix_durations'),
                          functionInputs = c('messagereport',  
                                             paste('messagereport', 'fixationreport', message.markupMultiple, sep=', '), 
                                             paste('fixationreport', paste("'",responses.markup, "'", sep=''), sep=', '), 
                                             paste('fixationreport', message.fix_contingencies, sep=', '), 
                                             paste('fixationreport', message.removals, sep=', '), 
                                             paste('messageRemovals[[2]]', exclusions.fix_durations, sep=', ')),
                          functionOutputs = c('messagereport', 
                                              'fixationreport', 
                                              'fixationreport',
                                              'fixationreport',
                                              'messageRemovals',
                                              'durationRemovals'),
                          homeProject = homeProject, 
                          website = website)
}


#  RUN DATA PROCESSING PIPELINE - ORGANISE FUNCTIONS #############################################################################################################################################################
#' Function for creating a data processing pipeline that focuses on ORGANISE functions.
#'
#' @param authors Authors of the pipeline, in a list.
#' @param message.markup What messages to mark up, in order.
#' @param responses.markup The column in the dataset that houses the correct response for each trial.
#' @param message.fix_contingencies What messages to mark up as part of fixation contingencies, in order.
#' @param message.removals A list of the messages that must appear in a trial for it to be kept within the dataset.
#' @param exclusions.fix_durations The lower and upper limits of fixation durations, in order.
#' @param homeProject Name of the project where this pipeline was first created.
#' @param website Web address for the project that gave rise to this pipeline.
#'
#' @examples
project.pipeline.run <- function(label = 'markup_pipeline'){
  
  #label <- 'markup_pipeline'
  
  # LOAD IN MARKUP
  pipeline <- data.table(TEXT=read_yaml(paste('pipelines/', label, '.yaml', sep='')))
  
  pipelineFunctions <- pipeline[substring(TEXT, 1, 8)=='FUNCTION',,]
  pipelineFunctions[,TEXT:=str_replace_all(TEXT, 'FUNCTION__', ''),]
  pipelineFunctions[,FUNCTION:=str_split(TEXT, ':')[[1]][[1]],list(TEXT)]
  pipelineFunctions[,PARAMETERS:=str_split(TEXT, ':')[[1]][[2]],list(TEXT)]
  
  pipelineOutputs <- pipeline[substring(TEXT, 1, 6)=='OUTPUT',,]
  pipelineOutputs[,OUTPUT:=str_split(TEXT, ':')[[1]][[2]],list(TEXT)]
  
  if(nrow(pipelineFunctions)==0){stop(call. = F, 'Pipeline is lacking any functions.')}
  if(nrow(pipelineFunctions)!=nrow(pipelineOutputs)){stop(call. = F, 'Pipeline has a different number of outputs to functions.')}
  
  for (i in seq(1:nrow(pipelineFunctions))){
    currentFunction <- paste(pipelineFunctions$FUNCTION[[i]], '(', pipelineFunctions$PARAMETERS[[i]], ')', sep='')
    currentOutput <- pipelineOutputs$OUTPUT[[i]]
    
    stringToAssign <- paste("assign('", currentOutput, "',", currentFunction, ", .GlobalEnv)", sep='')
    print(stringToAssign)
    
    eval(parse(text=stringToAssign))
    
  }
  
  
}




