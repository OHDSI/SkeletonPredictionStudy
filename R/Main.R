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

#' Execute the Study
#'
#' @details
#' This function executes the SkeletonPredictionStudy Study.
#' 
#' @param databaseDetails      Database details created using \code{PatientLevelPrediction::createDatabaseDetails()} 
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/). Do not use a folder on a network drive since this greatly impacts
#'                             performance.
#' @param createProtocol       Creates a protocol based on the analyses specification                             
#' @param createCohorts        Create the cohortTable table with the target population and outcome cohorts?
#' @param runDiagnostic        Runs a diagnostic of the T, O and tar settings for the cdmDatabaseSchema - can be used to check whether to change 
#'                             settings or whether the prediction may not do well.  
#' @param viewDiagnostic       Opens a shiny app with the diagnostic results (run after runDiagnostic completes)                              
#' @param runAnalyses          Run the model development
#' @param createValidationPackage  Create a package for sharing the models 
#' @param skeletonVersion      The version of the validation skeleton to use
#' @param analysesToValidate   A vector of analysis ids (e.g., c(1,3,10)) specifying which analysese to export into validation package. Default is NULL and all are exported.
#' @param packageResults       Should results be packaged for later sharing?     
#' @param minCellCount         The minimum number of subjects contributing to a count before it can be included 
#'                             in packaged results.
#' @param createShiny          Create a shiny app with the results
#' @param onlyFetchData        Only fetch data for the analyses without fitting models. Setting this flag will overwrite your input provided to the runAnalyses and createCohorts parameters.
#' @param sampleSize           The number of patients in the target cohort to sample (if NULL uses all patients)
#' @param logSettings           The log setting \code{PatientLevelPrediction::createLogSettings()}                            
#'
#' @examples
#' \dontrun{
#' connectionDetails <- createConnectionDetails(dbms = "postgresql",
#'                                              user = "joe",
#'                                              password = "secret",
#'                                              server = "myserver")
#'                                              
#'  databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
#'  connectionDetails = connectionDetails,
#'  cdmDatabaseSchema = cdmDatabaseSchema,
#'  cdmDatabaseName = cdmDatabaseName,
#'  tempEmulationSchema = tempEmulationSchema,
#'  cohortDatabaseSchema = cohortDatabaseSchema,
#'  cohortTable = cohortTable,
#'  outcomeDatabaseSchema = cohortDatabaseSchema,
#'  outcomeTable = cohortTable,
#'  cdmVersion = cdmVersion
#'  )  
#'  
#'  logSettings <- PatientLevelPrediction::createLogSettings(
#'  verbosity = "INFO",
#'  timeStamp = T,
#'  logName = 'skeletonPlp'
#'  )                                          
#'
#' execute(databaseDetails = databaseDetails,
#'         outputFolder = "c:/temp/study_results", 
#'         createProtocol = T,
#'         createCohorts = T,
#'         runDiagnostic = F,
#'         viewDiagnostic = F,
#'         runAnalyses = T,
#'         createValidationPackage = T,
#'         skeletonVersion = 'v1.0.1',
#'         packageResults = F,
#'         minCellCount = 5,
#'         createShiny = F,
#'         sampleSize = 10000,
#'         logSettings = logSettings
#'         )
#' }
#'
#' @export
execute <- function(databaseDetails,
                    outputFolder,
                    createProtocol = F,
                    createCohorts = F,
                    runDiagnostic = F,
                    viewDiagnostic = F,
                    runAnalyses = F,
                    createValidationPackage = F,
                    skeletonVersion = 'v0.0.1',
                    analysesToValidate = NULL,
                    packageResults = F,
                    minCellCount= 5,
                    createShiny = F,
                    onlyFetchData = F,
                    sampleSize = NULL,
                    logSettings
  ) {
  
  if (!file.exists(outputFolder))
    dir.create(outputFolder, recursive = TRUE)
  
  ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))
  
  if(createProtocol){
    ensure_installed('officer')
    createPlpProtocol(outputFolder)
  }
  
  if (createCohorts || onlyFetchData) {
    ParallelLogger::logInfo("Creating cohorts")
    createCohorts(
      databaseDetails = databaseDetails,
      outputFolder = outputFolder
    )
  }
  
  if(runDiagnostic){
    ParallelLogger::logInfo(paste0("Creating diagnostic results for ",cdmDatabaseName))
    predictionAnalysisListFile <- system.file("settings",
                                              "predictionAnalysisList.json",
                                              package = "SkeletonPredictionStudy")
    
    predictionAnalysisList <- tryCatch(
      {PatientLevelPrediction::loadPlpAnalysesJson(file.path(predictionAnalysisListFile))},
      error= function(cond) {
        ParallelLogger::logInfo('Issue when loading json file...');
        ParallelLogger::logError(cond)
      })

    # add sample settings
    if(!is.null(sampleSize)){
      ParallelLogger::logInfo('Adding sample settings')
      for(i in 1:length(predictionAnalysisList$analyses)){
        predictionAnalysisList$analyses[[i]]$restrictPlpDataSettings$sampleSize <- sampleSize
      }
    }
    
    cohortIds <- lapply(predictionAnalysisList$analyses, function(x) x$targetId)
    outcomeIds <- lapply(predictionAnalysisList$analyses, function(x) x$outcomeId)
    
    outcomeId <- list()
    length(outcomeId) <- length(unique(cohortIds))
    for(cohortId in unique(cohortIds)){
      outcomeId[[i]] <- outcomeIds[cohortIds == cohortId]
    }
    
    # run diagnostic
    for(i in 1:length(unique(cohortIds))){
      
      cohortId <- unique(cohortIds)[i]
      outcomeIds <- outcomeId[[i]]
      
      cohortName <- getNames(
        cohortDefinitions = predictionAnalysisList$cohortDefinitions, 
        ids = cohortId
        )
      
      outcomeNames <- getNames(
        cohortDefinitions = predictionAnalysisList$cohortDefinitions, 
        ids = outcomeIds
      )
      
      databaseDetails$cohortId <- cohortId
      databaseDetails$outcomeIds <- outcomeIds
      
      
      ParallelLogger::logInfo(paste0("Target Cohort: ", cohortName, ' generating'))
      
      diag <- tryCatch({PatientLevelPrediction::diagnostic(
        cdmDatabaseName = cdmDatabaseName,
        cohortName = cohortName, 
        outcomeNames = outcomeNames, 
        databaseDetails = databaseDetails,
        restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(sampleSize = sampleSize),
        populationSettings = PatientLevelPrediction::createStudyPopulationSettings(
          includeAllOutcomes = F, 
          firstExposureOnly = F, 
          washoutPeriod = 0, 
          requireTimeAtRisk = F, 
          removeSubjectsWithPriorOutcome = F, 
          riskWindowStart = 0, 
          riskWindowEnd = 9999
          ),
        outputFolder = file.path(outputFolder, 'diagnostics'), 
        minCellCount = minCellCount
      )},
        error = function(err) {
          # error handler picks up where error was generated
          ParallelLogger::logError(paste("Diagnostic error:  ",err))
          return(NULL)
          
        })
    }
    
    
  }
  
  if(viewDiagnostic){
    ParallelLogger::logInfo(paste0("Loading diagnostic shiny app"))
    
    checkDiagnosticResults <- dir.exists(file.path(outputFolder, 'diagnostics'))
    checkShinyViewer <- dir.exists(system.file("shiny", "DiagnosticsExplorer", package = "PatientLevelPrediction"))
    if(!checkDiagnosticResults){
      warning('No diagnosstic results found, please execute with runDiagnostic first')
    } else if(!checkShinyViewer){
      warning('No DiagnosticsExplorer shiny app found in your PatientLevelPrediction library - try updating PatientLevelPrediction')
    } else{
      ensure_installed("shiny")
      ensure_installed("shinydashboard")
      ensure_installed("DT")
      ensure_installed("VennDiagram")
      ensure_installed("htmltools")
      ensure_installed("shinyWidgets")
      shinyDirectory <- system.file("shiny", "DiagnosticsExplorer", package = "PatientLevelPrediction")
      shinySettings <- list(dataFolder = file.path(outputFolder, 'diagnostics'))
      .GlobalEnv$shinySettings <- shinySettings
      on.exit(rm(shinySettings, envir = .GlobalEnv))
      shiny::runApp(shinyDirectory)
    }
    
  }
  
  if(runAnalyses || onlyFetchData){
    if(onlyFetchData) {
      ParallelLogger::logInfo("Only fetching data and not running predictions")
    } else {
      ParallelLogger::logInfo("Running predictions")
    }
  
    predictionAnalysisListFile <- system.file("settings",
                                              "predictionAnalysisList.json",
                                              package = "SkeletonPredictionStudy")
    
    predictionAnalysisList <- tryCatch(
      {PatientLevelPrediction::loadPlpAnalysesJson(file.path(predictionAnalysisListFile))},
      error= function(cond) {
        ParallelLogger::logInfo('Issue when loading json file...');
        ParallelLogger::logError(cond)
      })
    
    # add sample settings
    if(!is.null(sampleSize)){
      ParallelLogger::logInfo('Adding sample settings')
      for(i in 1:length(predictionAnalysisList$analyses)){
        predictionAnalysisList$analyses[[i]]$restrictPlpDataSettings$sampleSize <- sampleSize
      }
    }
  
    # add code to add database settings for covariates...
    #[TODO]
    
    result <- do.call(
      PatientLevelPrediction::runMultiplePlp, 
      
      list(
        databaseDetails = databaseDetails,
        modelDesignList = predictionAnalysisList$analyses,
        onlyFetchData = onlyFetchData,
        splitSettings = predictionAnalysisList$splitSettings,
        logSettings = logSettings,
        saveDirectory = outputFolder
        )
    )
  }
  
  if (packageResults) {
    ensure_installed("OhdsiSharing")
    ParallelLogger::logInfo("Packaging results")
    packageResults(outputFolder = outputFolder,
                   minCellCount = minCellCount)
  }
  
  if(createValidationPackage){
    ensure_installed("Hydra")
    if(!is_installed("Hydra", version = '0.0.8')){
      warning('Hydra need to be updated to use custom cohort covariates')
    }

      ParallelLogger::logInfo('Creating validation using models saved to JSON files')
      tryCatch({
        
        createValidationPackageJson(devPackageName = 'SkeletonPredictionStudy',
                                    devDatabaseName = cdmDatabaseName,
                                    analysisLocation = outputFolder,
                                    analysisIds = analysesToValidate,
                                    outputFolder = outputFolder,
                                    packageName = 'SkeletonPredictionStudyValidation',
                                    description = 'validating models in SkeletonPredictionStudy',
                                    skeletonVersion = skeletonVersion,
                                    createdBy = 'anonymous',
                                    organizationName = 'none')
        
        
      }, error = function(e){ParallelLogger::logError(e)})
  }
  
  if (createShiny) {
    ensure_installed("plotly")
    ensure_installed("ggplot2")
    ensure_installed("reshape2")
    ensure_installed("DT")
    ensure_installed("htmltools")
    ensure_installed("shinydashboard")
    ensure_installed("shinyWidgets")
    ensure_installed("shinycssloaders")
    ensure_installed("shiny")
    
    populateShinyApp(outputDirectory = file.path(outputFolder, 'ShinyApp'),
                     resultDirectory = outputFolder,
                     minCellCount = minCellCount,
                     databaseName = cdmDatabaseName)
  }
  
  invisible(NULL)
}




getNames <- function(
  cohortDefinitions, 
  ids
){
  
  idNames <- lapply(cohortDefinitions, function(x) c(x$id, x$name))
  idNames <- do.call(rbind, idNames)
  colnames(idNames) <- c('id', 'name')
  idNames <- as.data.frame(idNames)
  
  nams <- c()
  for(id in ids){
    nams <- c(nams, idNames$name[idNames$id == id])
  }
  
  return(nams)
  
}

