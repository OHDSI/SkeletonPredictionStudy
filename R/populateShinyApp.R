# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of SkeletonPredictionStudy
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Adds the results of the study into a shiny app
#'
#' @details
#' This function adds the results of the study into a shiny app ready for sharing 
#'
#' @param outputDirectory     Name of folder containing the shiny app
#' @param shinyDirectory      Location of a template shiny app (with no data)
#' @param resultDirectory     The location of the results from the study
#' @param minCellCount        The minimum number of subjects contributing to a count before it can be included in the results.
#' @param databaseName        A friendly name for the database
#'
#' @export
populateShinyApp <- function(outputDirectory = './ShinyApp',
                             shinyDirectory,
                             resultDirectory,
                             minCellCount = 10,
                             databaseName = 'sharable name of development data'){
  
  #check inputs
  if(missing(shinyDirectory)){
    shinyDirectory <- system.file("shiny", "PLPViewer", package = "PatientLevelPrediction")
  }
  if(missing(resultDirectory)){
    stop('Need to enter the resultDirectory')
  }
  if(!dir.exists(resultDirectory)){
    stop('resultDirectory does not exist')
  }
  
  # create the shiny data folder
  if(!dir.exists(outputDirectory)){
    dir.create(outputDirectory, recursive = T)
  }
  
  # copy shiny folder to outputDirectory 
  R.utils::copyDirectory(from = shinyDirectory, 
                       to= outputDirectory,
                       recursive=TRUE)
  
  outputDirectory <- file.path(outputDirectory,'data')
  #outputDirectory <- file.path(shinyDirectory,'data')
  
  # copy the settings csv
  file <- utils::read.csv(file.path(resultDirectory,'settings.csv'))
  utils::write.csv(file, file.path(outputDirectory,'settings.csv'), row.names = F)
  
  # copy each analysis as a rds file and copy the log
  files <- dir(resultDirectory, full.names = F)
  files <- files[grep('Analysis', files)]
  for(file in files){
    
    if(!dir.exists(file.path(outputDirectory,file))){
      dir.create(file.path(outputDirectory,file))
    }
    
    if(dir.exists(file.path(resultDirectory,file, 'plpResult'))){
      res <- PatientLevelPrediction::loadPlpResult(file.path(resultDirectory,file, 'plpResult'))
      res <- PatientLevelPrediction::transportPlp(res, n= minCellCount, 
                                                  save = F, dataName = databaseName)
      saveRDS(res, file.path(outputDirectory,file, 'plpResult.rds'))
    }
    if(file.exists(file.path(resultDirectory,file, 'plpLog.txt'))){
      file.copy(from = file.path(resultDirectory,file, 'plpLog.txt'), 
                to = file.path(outputDirectory,file, 'plpLog.txt'))
    }
  }
  
  # copy any validation results
  if(dir.exists(file.path(resultDirectory,'Validation'))){
    valFolders <-  dir(file.path(resultDirectory,'Validation'), full.names = F)
    
    if(length(valFolders)>0){
      # move each of the validation rds
      for(valFolder in valFolders){
        
        # get the analysisIds
        valSubfolders <- dir(file.path(resultDirectory,'Validation',valFolder), full.names = F)
        if(length(valSubfolders)!=0){
          for(valSubfolder in valSubfolders ){
            valOut <- file.path(valFolder,valSubfolder)
            if(!dir.exists(file.path(outputDirectory,'Validation',valOut))){
              dir.create(file.path(outputDirectory,'Validation',valOut), recursive = T)
            }
            
            
            if(file.exists(file.path(resultDirectory,'Validation',valOut, 'validationResult.rds'))){
              res <- readRDS(file.path(resultDirectory,'Validation',valOut, 'validationResult.rds'))
              res <- PatientLevelPrediction::transportPlp(res, n= minCellCount, 
                                                          save = F, dataName = databaseName)
              saveRDS(res, file.path(outputDirectory,'Validation',valOut, 'validationResult.rds'))
            }
          }
        }
        
      }
      
    }
    
  }
  
  ParallelLogger::logInfo(paste0('Shiny App created at: ',  outputDirectory))
  ParallelLogger::logInfo(paste0('Upload the folder ',  outputDirectory, ' to the shinyDeploy OHDSI github to share the results with others.'))
  
  return(outputDirectory)
  
}
