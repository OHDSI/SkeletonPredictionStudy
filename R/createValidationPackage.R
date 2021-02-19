createValidationPackage <- function(modelFolder, 
                                    outputFolder,
                                    minCellCount = 5,
                                    databaseName = 'sharable name of development data',
                                    jsonSettings,
                                    analysisIds = NULL, 
                                    cohortVariableSetting = NULL){
  
  # json needs to contain the cohort details and packagename
  Hydra::hydrate(specifications = jsonSettings, 
                 outputFolder=outputFolder)
  
  transportPlpModels(analysesDir = modelFolder,
                     minCellCount = minCellCount,
                     databaseName = databaseName,
                     outputDir = file.path(outputFolder,"inst/plp_models"),
                     analysisIds = analysisIds,
                     cohortVariableSetting = cohortVariableSetting)
  
  transportCohort(packageName = "SkeletonPredictionStudy",
                  outputDir = file.path(outputFolder,"inst"))
  
  return(TRUE)
  
}

transportPlpModels <- function(analysesDir,
                               minCellCount = 5,
                               databaseName = 'sharable name of development data',
                               outputDir = "./inst/plp_models",
                               analysisIds = NULL,
                               cohortVariableSetting){
  
  files <- dir(analysesDir, recursive = F, full.names = F)
  files <- files[grep('Analysis_', files)]
  
  if(!is.null(analysisIds)){
    #restricting to analysisIds
    files <- files[gsub('Analysis_','',files)%in%analysisIds]
  }
  
  filesIn <- file.path(analysesDir, files , 'plpResult')
  filesOut <- file.path(outputDir, files, 'plpResult')
  
  cohortCovs <- c()
  for(i in 1:length(filesIn)){
    if(file.exists(filesIn[i])){
      plpResult <- PatientLevelPrediction::loadPlpResult(filesIn[i])
      PatientLevelPrediction::transportPlp(plpResult,
                                           modelName= files[i], dataName=databaseName,
                                           outputFolder = filesOut[i],
                                           n=minCellCount,
                                           includeEvaluationStatistics=T,
                                           includeThresholdSummary=T, includeDemographicSummary=T,
                                           includeCalibrationSummary =T, includePredictionDistribution=T,
                                           includeCovariateSummary=T, save=T)
      
      tempCohortCovs <- plpResult$covariateSummary$covariateId[plpResult$covariateSummary$analysisId == 456 & plpResult$covariateSummary$covariateValue !=0]
      if(length(tempCohortCovs)!=0){
        cohortCovs <- c(cohortCovs, tempCohortCovs)
      }
    }
    
  }
  
  if(length(cohortCovs)>0 & !is.null(cohortVariableSetting)){
    # move the custom cohort covariates
    pathToCustom <- system.file("settings", cohortVariableSetting, package = "SkeletonPredictionStudy")
    cohortVarsToCreate <- utils::read.csv(pathToCustom)
    temp <- cohortVarsToCreate$cohortId*1000+456
    utils::write.csv(cohortVarsToCreate[temp%in%cohortCovs,], 
              file.path(gsub('plp_models','settings', outputDir),'cohortVariableSetting.csv'), 
              row.names = F)
  }
  
}


transportCohort <- function(packageName = "SkeletonPredictionStudy",
                            outputDir = "./inst"){
  
  cohortLocation <- system.file("cohorts",package = packageName)
  cohortFiles <- dir(cohortLocation, recursive = F, full.names = F)
  if(!dir.exists(file.path(outputDir,'cohorts'))){dir.create(file.path(outputDir,'cohorts'), recursive = T)}
  file.copy(file.path(cohortLocation, cohortFiles), file.path(outputDir,'cohorts',cohortFiles), 
            overwrite = TRUE)
  sqlLocation <- system.file("sql","sql_server",package = packageName)
  sqlFiles <- dir(sqlLocation, recursive = F, full.names = F)
  if(!dir.exists(file.path(outputDir,'sql','sql_server'))){dir.create(file.path(outputDir,'sql','sql_server'), recursive = T)}
  file.copy(file.path(sqlLocation,sqlFiles), 
            file.path(outputDir,'sql','sql_server', sqlFiles), overwrite = TRUE )
  
  return(TRUE)
}
