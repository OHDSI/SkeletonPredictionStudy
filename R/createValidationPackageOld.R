createValidationPackage <- function(modelFolder, 
                                    outputFolder,
                                    minCellCount = 5,
                                    databaseName = 'sharable name of development data',
                                    analysisIds = NULL,
                                    skeletonVersion = 'v1.0.1'){
  
  # create validationJson
  ParallelLogger::logInfo('Creating validation json list')
  jsonSettings <- createValidationJsonOld(modelFolder, 
                                       analysisIds,
                                       skeletonVersion)
  pn <- jsonSettings$packageName
  
  #====
  ParallelLogger::logInfo('Converting list to json')
  jsonSettings <- tryCatch({RJSONIO::toJSON(jsonSettings, digits = 23)},
                           error=function(cond) {
                             ParallelLogger::logInfo(cond);
                             stop('Issue converting to json.')
                           })
  #====
  
  ParallelLogger::logInfo(paste0('Running Hydra to save to: ',file.path(outputFolder,pn)))
  Hydra::hydrate(specifications = jsonSettings, 
                 outputFolder=file.path(outputFolder,pn)) #===
  
  ParallelLogger::logInfo('Transporting models')
  transportPlpModels(analysesDir = modelFolder,
                     minCellCount = minCellCount,
                     databaseName = databaseName,
                     outputDir = file.path(outputFolder,pn,"inst/plp_models"),
                     analysisIds = analysisIds)
  
  ParallelLogger::logInfo('Transporting cohorts')
  transportCohort(packageName = "SkeletonPredictionStudy",
                  outputDir = file.path(outputFolder,pn,"inst"))
  
  return(TRUE)
  
}

transportPlpModels <- function(analysesDir,
                               minCellCount = 5,
                               databaseName = 'sharable name of development data',
                               outputDir = "./inst/plp_models",
                               analysisIds = NULL){
  
  files <- dir(analysesDir, recursive = F, full.names = F)
  files <- files[grep('Analysis_', files)]
  
  if(!is.null(analysisIds)){
    #restricting to analysisIds
    files <- files[gsub('Analysis_','',files)%in%analysisIds]
  }
  
  filesIn <- file.path(analysesDir, files , 'plpResult')
  filesOut <- file.path(outputDir, files, 'plpResult')
  
  for(i in 1:length(filesIn)){
    if(file.exists(filesIn[i])){
      plpResult <- PatientLevelPrediction::loadPlpResult(filesIn[i])
      #updating the covariate dep
      plpResult$model$metaData$call$covariateSettings <- replaceFunAttr(plpResult$model$metaData$call$covariateSettings)
      
      PatientLevelPrediction::transportPlp(plpResult,
                                           modelName= files[i], dataName=databaseName,
                                           outputFolder = filesOut[i],
                                           n=minCellCount,
                                           includeEvaluationStatistics=T,
                                           includeThresholdSummary=T, includeDemographicSummary=T,
                                           includeCalibrationSummary =T, includePredictionDistribution=T,
                                           includeCovariateSummary=T, save=T)
      
    }
    
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



createValidationJsonOld <- function(modelFolder, 
                                 analysisIds,
                                 skeletonVersion){
  
  predictionAnalysisListFile <- system.file("settings",
                                            "predictionAnalysisList.json",
                                            package = "SkeletonPredictionStudy")
  
  devJS <- jsonlite::read_json(predictionAnalysisListFile)
  
  valJS <- list()
  
  valJS$packageName <- paste0(devJS$packageName,'Validation')
  valJS$skeletonType <- 'PatientLevelPredictionValidationStudy'
  valJS$skeletonVersion <- skeletonVersion
  
  valJS$cohortDefinitions <- devJS$cohortDefinitions #update?
  
  valJS$modelSettings <- createModelJson(modelFolder, 
                                         analysisIds) # pop, cov, modelLoc
  
  
  #result <- jsonlite::serializeJSON(valJS)
  return(valJS)
}


createModelJson <- function(modelFolder, 
                            analysisIds){
  
  files <- dir(modelFolder, recursive = F, full.names = F)
  files <- files[grep('Analysis_', files)]
  
  if(!is.null(analysisIds)){
    #restricting to analysisIds
    files <- files[gsub('Analysis_','',files)%in%analysisIds]
  }
  
  modelList <- list()
  length(modelList) <- length(files)
  i <- 0
  
  for(file in files){
    i <- i+1
    filesIn <- file.path(modelFolder, file , 'plpResult')
    
    modelRes <- PatientLevelPrediction::loadPlpResult(filesIn)
    
    modelList[[i]] <- list()
    modelList[[i]]$populationSettings <- modelRes$model$populationSettings
    modelList[[i]]$covariateSettings <- modelRes$model$metaData$call$covariateSettings
    modelList[[i]]$covariateSettings <- replaceFunAttr(modelList[[i]]$covariateSettings)
    
    #====
    if(class(modelList[[i]]$covariateSettings) == "covariateSettings"){
      if(!is.null(attr(modelList[[i]]$covariateSettings,'fun'))){
        modelList[[i]]$covariateSettings$attr_fun <- attr(modelList[[i]]$covariateSettings,'fun')
      }
      if(!is.null(attr(modelList[[i]]$covariateSettings,'class'))){
        modelList[[i]]$covariateSettings$attr_class <- attr(modelList[[i]]$covariateSettings,'class')
      }
    }
    
    if(class(modelList[[i]]$covariateSettings) == "list"){
      for(j in 1:length(modelList[[i]]$covariateSettings)){
        if(!is.null(attr(modelList[[i]]$covariateSettings[[j]],'fun'))){
          modelList[[i]]$covariateSettings[[j]]$attr_fun <- attr(modelList[[i]]$covariateSettings[[j]],'fun')
        }
        if(!is.null(attr(modelList[[i]]$covariateSettings[[j]],'class'))){
          modelList[[i]]$covariateSettings[[j]]$attr_class <- attr(modelList[[i]]$covariateSettings[[j]],'class')
        }
      }
    }
    #====
    
    modelList[[i]]$plpDataSettings <- list(cohortId = modelRes$inputSetting$dataExtrractionSettings$cohortId,
                                           outcomeIds = modelRes$inputSetting$dataExtrractionSettings$outcomeIds,
                                           cdmVersion = modelRes$inputSetting$dataExtrractionSettings$cdmVersion,
                                           firstExposureOnly = modelRes$inputSetting$dataExtrractionSettings$firstExposureOnly,
                                           washoutPeriod = modelRes$inputSetting$dataExtrractionSettings$washoutPeriod,
                                           studyStartDate = modelRes$inputSetting$dataExtrractionSettings$studyStartDate,
                                           studyEndDate = modelRes$inputSetting$dataExtrractionSettings$studyEndDate)
    
    
    modelList[[i]]$modelSettings <- list(type = modelRes$model$modelSettings$model,
                                         analysisId = file,
                                         includedCovs = modelRes$model$varImp$covariateId[modelRes$model$varImp$covariateValue!=0]
    )
    
  }
  
  return(modelList)
}


replaceFunAttr <- function(x){
  
  if(class(x)=="covariateSettings"){
    attr(x, 'fun') <- gsub("SkeletonPredicionStudy::", 
                           "SkeletonPredicionStudyValidation::", 
                           attr(x, 'fun'))
  } else{
    for(i in 1:length(x)){
      attr(x[[i]], 'fun') <- gsub("SkeletonPredicionStudy::", 
                                  "SkeletonPredicionStudyValidation::", 
                                  attr(x[[i]], 'fun'))
    }
  }
  return(x)
}