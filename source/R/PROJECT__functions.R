
# CREATE PROJECT FILES AND STRUCTURE FOR A SINGLE-DATASET PROJECT #############################################################################################################################################################
#' Function for creating a single-dataset project
#'
#' @param title Name of new project
#' @param directoryTitle Name of the folder the dataset is housed in. This is needed for when you want to 
#' import your dataset into another project, so needs to follow standard folder naming rules.
#' @param description Description of new project
#' @param authors Authors of the project, in a list
#' @param website Web address for the project
#'
#' @examples
project.create.singleDataset <- function(title='eyeTrackR Project', 
                                         directoryTitle = 'eyeTrackRProject',
                                         description = '', 
                                         authors = '', 
                                         website = ''){
  
  # CHECK DIRECTORY TITLE IS A VALID FOLDER - SEEK OUT SPECIAL CHARACTERS AND FLAG IF A PROBLEM
  pattern <- "/|:|\\?|<|>|\\|\\\\|\\*"
  dirCheck <- grepl(pattern, directoryTitle)
  try(if(dirCheck == TRUE) {
    stop(call. = F, "Your directoryTitle contains one or more special characters. Please remove these before re-running this function.")})
  
  # CHECK IF WE ALREADY HAVE A PROJECT IN THE CURRENT WORKING DIRECTORY
  if((dir.exists('data/')==T | dir.exists('pipelines/')==T | 
     dir.exists('logs/')==T | dir.exists('scripts/')==T | file.exists('project.yaml')==T) & dirCheck==T){
    
    message('Unable to create the new project that you requested. You seem to have some existing project folders/files in your current working directory. Please check or remove these before running this function.')
  }
  
  # CREATE PROJECT MARKUP FILE
  if (file.exists('project.yaml')==F & dir.exists('data/')==F & 
      dir.exists('pipelines/')==F & dir.exists('logs/')==F & 
      dir.exists('logs/')==F & dir.exists('scripts/')==F & dirCheck ==F){
    
    # CREATE DIRECTORIES
    dir.create('data/')
    dir.create('pipelines/')
    dir.create('logs/')
    dir.create('scripts/')
  
    projectYaml <- file('project.yaml', 'w')
    
    write_yaml(data.table(
      title = title,
      directoryTitle = directoryTitle,
      description = description,
      projectType = 'single-dataset',
      authors = authors,
      website = website,
      eyeTrackR = as.character(packageVersion("eyeTrackR")[[1]])
    ), projectYaml)
    close(projectYaml)
    
  }
  
}

# CREATE PROJECT FILES AND STRUCTURE FOR A MULTI-DATASET PROJECT #############################################################################################################################################################
#' Function for creating a multi-dataset project
#'
#' @param title Name of new project
#' @param description Description of new project
#' @param authors Authors of the project, in a list
#' @param website Web address for the project
#'
#' @examples
project.create.multiDataset <- function(title='eyeTrackR Project', 
                                         description = '', 
                                         authors = '', 
                                         website = ''){
  
  
  # CHECK IF WE ALREADY HAVE A PROJECT IN THE CURRENT WORKING DIRECTORY
  if(dir.exists('projects/')==T | dir.exists('scriptsMulti/')==T | file.exists('project.yaml')==T){
    
    message('Unable to create the new project that you requested. You seem to have some existing project files in your current working directory. Please check or remove these before running this function.')
  }
  
  # CREATE PROJECT MARKUP FILE
  if (file.exists('project.yaml')==F & dir.exists('projects/')==F & dir.exists('scriptsMulti/')==F){
    
    dir.create('projects/')
    dir.create('scriptsMulti/')
    
    projectYaml <- file('project.yaml', "w")
    write_yaml(data.table(
      title = title,
      briefTitle = title,
      description = description,
      projectType = 'multi-dataset',
      authors = authors,
      website = website,
      eyeTrackR = as.character(packageVersion("eyeTrackR")[[1]])
    ), projectYaml)
    close(projectYaml)
  }
  
}

# LOAD INFO FOR A PROJECT BASED ON THE PATH #######################################################################
#' Helper function for loading project info from yaml file
#'
#' @param path Path to dataset that you want to import
#'
#' @examples
project.load.info <- function(path=''){
  
  currentProject <- ''
  
  # ASSUME WE JUST WANT TO LOAD THE INFO FROM THE CURRENT PROJECT
  if(path==''){ currentProject <- read_yaml('project.yaml')}
  
  # IF WE ARE POINTING TO A DIFFERENT PATH
  if(path!=''){ currentProject <- read_yaml(paste(path, 'project.yaml', sep='/'))}
  
  return(currentProject)
}

# IMPORT SINGLE-DATASET PROJECT #############################################################################################################################################################
#' Function for importing a dataset from a single-dataset project into a multi-dataset project
#'
#' @param path Path to dataset that you want to import
#'
#' @examples
project.import.singleDataset <- function(path=''){
  
  if (file.exists(paste(path, '/project.yaml', sep=''))){
    newProjectInfo <- project.load.info(path)
    
    if(dir.exists(paste('projects/', newProjectInfo$directoryTitle, sep=''))==T){
      warning(call. = F,'Unable to import - dataset already imported!')
    }
    
    if(dir.exists(paste('projects/', newProjectInfo$directoryTitle, sep=''))==F){
      file.copy(path, 'projects/', recursive=TRUE)
    }
  }
  
  if(file.exists(paste(path, '/project.yaml', sep=''))==F){
    warning(call. = F, 'No project exists in the path you specified.')
  }
}

# IMPORT MULTI-DATASET PROJECT #############################################################################################################################################################
#' Function for importing datasets from a multi-dataset project into a multi-dataset project
#'
#' @param path Path to dataset that you want to import
#'
#' @examples
project.import.multiDataset <- function(path=''){
  
  # THIS FUNCTION JUST LOOPS THROUGH THE FOLDERS IN PROJECTS AND COPIES THEM OVER FOR YOU
  dirsToImport <- list.dirs(paste(path, 'projects/', sep=''), recursive = F)
  
  for(i in seq(1:length(dirsToImport))){
    project.import.singleDataset(dirsToImport[[i]])
  }
    
}



