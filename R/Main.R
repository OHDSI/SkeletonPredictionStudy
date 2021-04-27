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
#' @param connectionDetails    An object of type \code{connectionDetails} as created using the
#'                             \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                             DatabaseConnector package.
#' @param cdmDatabaseSchema    Schema name where your patient-level data in OMOP CDM format resides.
#'                             Note that for SQL Server, this should include both the database and
#'                             schema name, for example 'cdm_data.dbo'.
#' @param cdmDatabaseName      Shareable name of the database 
#' @param cohortDatabaseSchema Schema name where intermediate data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_data.dbo'.
#' @param cohortTable          The name of the table that will be created in the work database schema.
#'                             This table will hold the target population cohorts used in this
#'                             study.
#' @param oracleTempSchema     Should be used in Oracle to specify a schema where the user has write
#'                             priviliges for storing temporary tables.
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/). Do not use a folder on a network drive since this greatly impacts
#'                             performance.
#' @param createProtocol       Creates a protocol based on the analyses specification                             
#' @param createCohorts        Create the cohortTable table with the target population and outcome cohorts?
#' @param runDiagnostic        Runs a diagnostic of the T, O and tar settings for the cdmDatabaseSchema - can be used to check whether to change 
#'                             settings or whether the prediction may not do well.  
#' @param viewDiagnostic       Opens a shiny app with the diagnostic results (run after runDiagnostic completes)                              
#' @param runAnalyses          Run the model development
#' @param createResultsDoc     Create a document containing the results of each prediction
#' @param createValidationPackage  Create a package for sharing the models 
#' @param skeletonVersion      The version of the validation skeleton to use
#' @param analysesToValidate   A vector of analysis ids (e.g., c(1,3,10)) specifying which analysese to export into validation package. Default is NULL and all are exported.
#' @param packageResults       Should results be packaged for later sharing?     
#' @param minCellCount         The minimum number of subjects contributing to a count before it can be included 
#'                             in packaged results.
#' @param createShiny          Create a shiny app with the results
#' @param createJournalDocument Do you want to create a template journal document populated with results?
#' @param analysisIdDocument   Which Analysis_id do you want to create the document for?
#' @param onlyFetchData        Only fetch data for the analyses without fitting models. Setting this flag will overwrite your input provided to the runAnalyses and createCohorts parameters.
#' @param sampleSize           The number of patients in the target cohort to sample (if NULL uses all patients)
#' @param verbosity            Sets the level of the verbosity. If the log level is at or higher in priority than the logger threshold, a message will print. The levels are:
#'                                         \itemize{
#'                                         \item{DEBUG}{Highest verbosity showing all debug statements}
#'                                         \item{TRACE}{Showing information about start and end of steps}
#'                                         \item{INFO}{Show informative information (Default)}
#'                                         \item{WARN}{Show warning messages}
#'                                         \item{ERROR}{Show error messages}
#'                                         \item{FATAL}{Be silent except for fatal errors}
#'                                         }                              
#' @param cdmVersion           The version of the common data model                       
#'
#' @examples
#' \dontrun{
#' connectionDetails <- createConnectionDetails(dbms = "postgresql",
#'                                              user = "joe",
#'                                              password = "secret",
#'                                              server = "myserver")
#'
#' execute(connectionDetails,
#'         cdmDatabaseSchema = "cdm_data",
#'         cdmDatabaseName = 'shareable name of the database'
#'         cohortDatabaseSchema = "study_results",
#'         cohortTable = "cohort",
#'         oracleTempSchema = NULL,
#'         outputFolder = "c:/temp/study_results", 
#'         createProtocol = T,
#'         createCohorts = T,
#'         runDiagnostic = F,
#'         viewDiagnostic = F,
#'         runAnalyses = T,
#'         createResultsDoc = T,
#'         createValidationPackage = T,
#'         skeletonVersion = 'v1.0.1',
#'         packageResults = F,
#'         minCellCount = 5,
#'         createShiny = F,
#'         sampleSize = 10000,
#'         verbosity = "INFO",
#'         cdmVersion = 5)
#' }
#'
#' @export
execute <- function(connectionDetails,
                    cdmDatabaseSchema,
                    cdmDatabaseName = 'friendly database name',
                    cohortDatabaseSchema = cdmDatabaseSchema,
                    cohortTable = "cohort",
                    oracleTempSchema = cohortDatabaseSchema,
                    outputFolder,
                    createProtocol = F,
                    createCohorts = F,
                    runDiagnostic = F,
                    viewDiagnostic = F,
                    runAnalyses = F,
                    createResultsDoc = F,
                    createValidationPackage = F,
                    skeletonVersion = 'v0.0.1',
                    analysesToValidate = NULL,
                    packageResults = F,
                    minCellCount= 5,
                    createShiny = F,
                    createJournalDocument = F,
                    analysisIdDocument = 1,
                    onlyFetchData = F,
                    sampleSize = NULL,
                    verbosity = "INFO",
                    cdmVersion = 5) {
  
  if (!file.exists(outputFolder))
    dir.create(outputFolder, recursive = TRUE)
  
  ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))
  
  if(createProtocol){
    ensure_installed('officer')
    createPlpProtocol(outputFolder)
  }
  
  if (createCohorts || onlyFetchData) {
    ParallelLogger::logInfo("Creating cohorts")
    createCohorts(connectionDetails = connectionDetails,
                  cdmDatabaseSchema = cdmDatabaseSchema,
                  cohortDatabaseSchema = cohortDatabaseSchema,
                  cohortTable = cohortTable,
                  oracleTempSchema = oracleTempSchema,
                  outputFolder = outputFolder)
  }
  
  if(runDiagnostic){
    ParallelLogger::logInfo(paste0("Creating diagnostic results for ",cdmDatabaseName))
    predictionAnalysisListFile <- system.file("settings",
                                              "predictionAnalysisList.json",
                                              package = "SkeletonPredictionStudy")
    predictionAnalysisList <- PatientLevelPrediction::loadPredictionAnalysisList(predictionAnalysisListFile)
    
    
    # extract settings
    #sampleSize = predictionAnalysisList$maxSampleSize
    if(!is.null(predictionAnalysisList$maxSampleSize)){
      warning('sampleSize is now specified in execute() - ignoring json settings')
    }
    cohortIds= predictionAnalysisList$cohortIds
    cohortNames = predictionAnalysisList$cohortNames
    outcomeIds = predictionAnalysisList$outcomeIds
    outcomeNames = predictionAnalysisList$outcomeNames
    
    tars <- do.call(rbind, lapply(predictionAnalysisList$modelAnalysisList$populationSettings, function(x){
      c(x$riskWindowStart, x$startAnchor, 
        x$riskWindowEnd, x$endAnchor)}))
    riskWindowStart = tars[,1]
    startAnchor = tars[,2]
    riskWindowEnd = tars[,3]
    endAnchor = tars[,4]
    
    # run diagnostic
    for(i in 1:length(cohortIds)){
      cohortId <- cohortIds[i]
      cohortName <- cohortNames[i]
      
      ParallelLogger::logInfo(paste0("Target Cohort: ", cohortName, ' generating'))
      
      diag <- tryCatch({PatientLevelPrediction::diagnostic(cdmDatabaseName = cdmDatabaseName, 
                                                 connectionDetails = connectionDetails, 
                                                 cdmDatabaseSchema = cdmDatabaseSchema, 
                                                 oracleTempSchema = oracleTempSchema, 
                                                 cohortId = cohortId, 
                                                 cohortName = cohortName, 
                                                 outcomeIds = outcomeIds, 
                                                 outcomeNames = outcomeNames, 
                                                 cohortDatabaseSchema = cohortDatabaseSchema, 
                                                 cohortTable = cohortTable, 
                                                 outcomeDatabaseSchema = cohortDatabaseSchema, 
                                                 outcomeTable = cohortTable, 
                                                 cdmVersion = cdmVersion, 
                                                 outputFolder = file.path(outputFolder, 'diagnostics'), 
                                                 sampleSize = sampleSize, 
                                                 minCellCount = minCellCount, 
                                                 riskWindowStart = as.double(riskWindowStart), 
                                                 startAnchor = startAnchor, 
                                                 riskWindowEnd = as.double(riskWindowEnd), 
                                                 endAnchor = endAnchor)},
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
    predictionAnalysisList <- PatientLevelPrediction::loadPredictionAnalysisList(predictionAnalysisListFile)
    predictionAnalysisList$connectionDetails = connectionDetails
    predictionAnalysisList$cdmDatabaseSchema = cdmDatabaseSchema
    predictionAnalysisList$cdmDatabaseName = cdmDatabaseName
    predictionAnalysisList$oracleTempSchema = oracleTempSchema
    predictionAnalysisList$cohortDatabaseSchema = cohortDatabaseSchema
    predictionAnalysisList$cohortTable = cohortTable
    predictionAnalysisList$outcomeDatabaseSchema = cohortDatabaseSchema
    predictionAnalysisList$outcomeTable = cohortTable
    predictionAnalysisList$cdmVersion = cdmVersion
    predictionAnalysisList$outputFolder = outputFolder
    predictionAnalysisList$verbosity = verbosity
    predictionAnalysisList$onlyFetchData = onlyFetchData
    
    if(!is.null(predictionAnalysisList$maxSampleSize)){
      warning('sampleSize is now specified in execute() - ignoring json settings')
    }
    predictionAnalysisList$maxSampleSize <- sampleSize
    
    
    # make backwards compatible:
    if(class(predictionAnalysisList$modelAnalysisList$covariateSettings[[1]])=='covariateSettings'){
      predictionAnalysisList$modelAnalysisList$covariateSettings <- predictionAnalysisList$modelAnalysisList$covariateSettings
    }else{
      predictionAnalysisList$modelAnalysisList$covariateSettings <- evaluateCovariateSettings(covariateSettings = predictionAnalysisList$modelAnalysisList$covariateSettings,
                                                                                              cohortDatabaseSchema = cohortDatabaseSchema,
                                                                                              cohortTable = cohortTable)
    }
    
    result <- do.call(PatientLevelPrediction::runPlpAnalyses, predictionAnalysisList)
  }
  
  if (packageResults) {
    ensure_installed("OhdsiSharing")
    ParallelLogger::logInfo("Packaging results")
    packageResults(outputFolder = outputFolder,
                   minCellCount = minCellCount)
  }
  
  if(createResultsDoc){
    ensure_installed("officer")
    ensure_installed("gridExtra")
    ensure_installed("grDevices")
    createMultiPlpReport(analysisLocation=outputFolder,
                         protocolLocation = file.path(outputFolder,'protocol.docx'),
                         includeModels = F)
  }
  
  if(createValidationPackage){
    ensure_installed("Hydra")
    if(!is_installed("Hydra", version = '0.0.8')){
      warning('Hydra need to be updated to use custom cohort covariates')
    }

    # TODO update to move cohorts over and edit cohort covariate to update cohort setting detail
    createValidationPackage(modelFolder = outputFolder, 
                            outputFolder = outputFolder,
                            minCellCount = minCellCount,
                            databaseName = cdmDatabaseName,
                            analysisIds = analysesToValidate,
                            skeletonVersion = skeletonVersion)  
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
    ensure_installed("R.utils")
    
    populateShinyApp(outputDirectory = file.path(outputFolder, 'ShinyApp'),
                     resultDirectory = outputFolder,
                     minCellCount = minCellCount,
                     databaseName = cdmDatabaseName)
  }
  
  if(createJournalDocument){
    ensure_installed("Hydra")
    predictionAnalysisListFile <- system.file("settings",
                                              "predictionAnalysisList.json",
                                              package = "SkeletonPredictionStudy")
    jsonSettings <-  tryCatch({Hydra::loadSpecifications(file=predictionAnalysisListFile)},
                              error=function(cond) {
                                stop('Issue with json file...')
                              })
    pn <- jsonlite::fromJSON(jsonSettings)
    createJournalDocument(resultDirectory = outputFolder,
                                      analysisId = analysisIdDocument, 
                                      includeValidation = T,
                                      cohortIds = pn$cohortDefinitions$id,
                                      cohortNames = pn$cohortDefinitions$name)
  }
  
  
  invisible(NULL)
}




