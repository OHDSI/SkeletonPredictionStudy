createValidationPackageJson <- function(devPackageName = 'SkeletonPredictionStudy',
                                        devDatabaseName,
                                        analysisLocation,
                                        analysisIds = NULL,
                                        outputFolder,
                                        packageName,
                                        description = 'missing',
                                        skeletonVersion = 'v1.0.0',
                                        createdBy = 'anonymous',
                                        organizationName = 'none'){
  
  if(missing(analysisLocation)){
    stop('Must Enter analysisLocation')
  }
  if(missing(outputFolder)){
    stop('Must Enter outputFolder')
  }
  if(missing(packageName)){
    stop('Must Enter packageName')
  }
  
  
  packageLoc <- file.path(outputFolder, packageName)
  
  # get settings
  plpSettingsLocation <- system.file("settings",
                                     "predictionAnalysisList.json",
                                     package = devPackageName)
  plpSettings <- Hydra::loadSpecifications(plpSettingsLocation)
  plpSettings  <- RJSONIO::fromJSON(plpSettings)
  cohortDefinitions <- plpSettings$cohortDefinitions
  
  # get models
  results <- dir(file.path(analysisLocation), pattern = 'Analysis_')
  if(!is.null(analysisIds)){
    results <- results[gsub('Analysis_','',results) %in% analysisIds]
  }
  
  modelJson <- list()
  length(modelJson) <- length(results)
  
  for(i in 1:length(results)){
    
    plpModel <- PatientLevelPrediction::loadPlpResult(file.path(analysisLocation,
                                                                results[i],
                                                                'plpResult')
    )
    
    modelJson[[i]] <- createModelSettingsJson(modelName = results[i],
                                              plpModel = plpModel$model,
                                              devDatabaseName = devDatabaseName,
                                              cohortId = plpModel$inputSetting$dataExtrractionSettings$cohortId,
                                              outcomeId = plpModel$inputSetting$populationSettings$outcomeId,
                                              populationSettings = NULL,
                                              cohortDefinitions = cohortDefinitions)
  }
  
  
  jsonSet <- createValidationJson(packageName = packageName,
                                  description = description,
                                  skeletonVersion = skeletonVersion,
                                  createdBy = createdBy,
                                  organizationName = organizationName,
                                  modelsJson = modelJson)
  
  jsonSet <- RJSONIO::toJSON(jsonSet, digits = 23)
  Hydra::hydrate(jsonSet,
                 outputFolder = packageLoc)
  
  transportModelsToJson(modelJson, packageLoc)
  
  return(packageLoc)
}







createModelSettingsJson <- function(modelName, plpModel,
                                    devDatabaseName,
                                    cohortId, outcomeId,
                                    populationSettings = NULL,
                                    cohortDefinitions = NULL,
                                    baseUrl = NULL,
                                    authMethod = NULL,
                                    webApiUser = NULL,
                                    webApiPw = NULL){
  
  if(base::missing(modelName)){
    stop('modelName required')
  }
  if(base::missing(plpModel)){
    stop('plpModel required')
  }
  if(!class(plpModel) %in% c('plpModel')){
    stop('Wrong plpModel object')
  }
  if(base::missing(cohortId)){
    stop('cohortId required')
  }
  if(base::missing(outcomeId)){
    stop('outcomeId required')
  }
  
  # check either cohortDefinitions or baseUrl
  if(!is.null(cohortDefinitions)){
    if(!class(cohortDefinitions) %in% c('list')){
      stop('Wrong cohortDefinitions object')
    }
    useWebApi <- F
  } else{
    if(is.null(baseUrl)){
      stop('Missing cohortDefinitions and baseUrl')
    }
    useWebApi <- T
  }
  
  plpModel <- removeSensitive(plpModel) # replace this with just extracting main parts
  
  model <- list()
  model$name <- modelName
  model$attr_type <- attr(plpModel, 'type')
  if(model$attr_type  == 'pythonReticulate'){
    model$attr_type <- 'pythonJson'
  }
  model$attr_predictionType <- attr(plpModel, 'predictionType')
  
  model$cohortId <- cohortId
  model$outcomeId <- outcomeId
  
  if(is.null(populationSettings)){
    plpModel$populationSettings$attrition <- NULL
    model$populationSettings <- plpModel$populationSettings
  } else{
    
    model$populationSettings <- populationSettings
  }
  
  if(class(plpModel$metaData$call$covariateSettings) == 'covariateSettings'){
    model$covariateSettings <- convertCovariateSettings(plpModel$metaData$call$covariateSettings)
  } else{
    model$covariateSettings <- lapply(1:length(plpModel$metaData$call$covariateSettings),
                                      function(i){convertCovariateSettings(plpModel$metaData$call$covariateSettings[[i]])})
  }
  
  cohortIds <- unique(c(cohortId,outcomeId, getCovariateCohorts(model$covariateSettings)))
  
  if(!useWebApi){
    cohortDefinitionsOfInt <- extractCohortDefinitionsJson(cohortDefinitions, cohortIds)
  } else{
    cohortDefinitionsOfInt <- extractCohortDefinitionsWebApi(baseUrl,
                                                             authMethod,
                                                             webApiUser,
                                                             webApiPw,
                                                             cohortIds)
  }
  
  # add the extras things that are needed for the prediction
  model$settings <- list(cohortId = model$populationSettings$cohortId,
                         outcomeId = model$populationSettings$outcomeId,
                         dense = plpModel$dense,
                         metaData = list(preprocessSettings = list(deletedRedundantCovariateIds = plpModel$metaData$preprocessSettings$deletedRedundantCovariateIds,
                                                                   deletedInfrequentCovariateIds = plpModel$metaData$preprocessSettings$deletedInfrequentCovariateIds,
                                                                   normFactors = plpModel$metaData$preprocessSettings$normFactors),
                                         call = list(connectionDetails = NULL,
                                                     cdmDatabaseSchema =  devDatabaseName)
                         )
                         
  )
  
  result <- list(modelSettingsJson = model,
                 model = plpModel$model,
                 covariateMap = plpModel$covariateMap,
                 varImp = plpModel$varImp,
                 cohortDefinitions = cohortDefinitionsOfInt)
  class(result) <- 'modelsJson'
  
  return(result)
}


removeSensitive <- function(plpModel){
  plpModel$predict <- NULL
  plpModel$index <- NULL
  plpModel$trainCVAuc <- NULL
  if(!is.null(names(plpModel$model))){
    if('cv' %in% names(plpModel$model)){
      plpModel$model$cv <- NULL
    }
  }
  cvset <- plpModel$metaData$call$covariateSettings
  plpModel$metaData$preprocessSettings$call <- NULL
  plpModel$metaData$preprocessSettings$sql <- NULL
  plpModel$metaData$call <- NULL
  plpModel$metaData$call$covariateSettings <- cvset
  
  return(plpModel)
  
}

convertCovariateSettings <- function(covariateSettings){
  
  fnct <- attr(covariateSettings, "fun")
  
  if(fnct == "getDbDefaultCovariateData"){
    
    covariateSettings$attr_class <- "covariateSettings"
    covariateSettings$attr_fun <- "getDbDefaultCovariateData"
    
    result <- list(fnct = 'createCovariateSettings',
                   settings = covariateSettings)
    
  } else{
    
    fnct <- strsplit(fnct, '::')[[1]][2]
    fnct <- gsub('get','create', fnct)
    fnct <- gsub('CovariateData','CovariateSettings', fnct)
    
    if(!is.null(covariateSettings$cohortDatabaseSchema)){
      covariateSettings$cohortDatabaseSchema <- 'to be defined'
    }
    if(!is.null(covariateSettings$cohortTable)){
      covariateSettings$cohortTable <- 'to be defined'
    }
    
    if(!is.null(covariateSettings$scaleMap)){
      covariateSettings$scaleMap <- deparse(covariateSettings$scaleMap)
    }
    
    result <- list(fnct = fnct,
                   settings = covariateSettings)
  }
  
  return(result)
  
}


getCovariateCohorts <- function(covariateSettings){
  
  if(is.null(covariateSettings$attr_fun)){
    cohortIds <- do.call('c', lapply(covariateSettings, getCohorts))
    return(cohortIds)
  } else{
    return(NULL)
  }
}

getCohorts <- function(covariateSettings){
  
  if(class(covariateSettings)!='list'){
    return(NULL)
  }
  
  if(covariateSettings$fnct %in% c("createCohortCovariateSettings", "createMeasurementCohortCovariateSettings")){
    return(covariateSettings$settings$cohortId)
  }else{
    return(NULL)
  }
  
}

extractCohortDefinitionsJson <- function(cohortDefinitions, cohortIds){
  
  cohortDefinitionsIds <- unlist(lapply(1:length(cohortDefinitions),
                                        function(j){cohortDefinitions[[j]]$id}))
  
  cohortDefinitionsRestricted <- list()
  length(cohortDefinitionsRestricted) <- length(cohortIds)
  
  for(i in 1:length(cohortIds)){
    ind <- min(which(cohortIds[i] == cohortDefinitionsIds))
    cohortDefinitionsRestricted[[i]] <- cohortDefinitions[[ind]]
  }
  
  return(cohortDefinitionsRestricted)
  
}

extractCohortDefinitionsWebApi <- function(baseUrl,
                                           authMethod,
                                           webApiUser,
                                           webApiPw,
                                           cohortIds){
  
  # connect to webApi
  if(!is.null(authMethod)){
    ROhdsiWebApi::authorizeWebApi(baseUrl = baseUrl,
                                  authMethod = authMethod,
                                  webApiUsername = webApiUser,
                                  webApiPassword = webApiPw)
  }
  
  # download cohorts..
  cohortDefinitions <- lapply(cohortIds, function(x){ROhdsiWebApi::getCohortDefinition(cohortId = x, baseUrl = baseUrl)})
  
  return(cohortDefinitions)
}



# code to create validation json
createValidationJson <- function(packageName,
                                 description = "an example of the skeleton",
                                 skeletonVersion = 'v1.0.0',
                                 createdBy = 'name',
                                 organizationName = "add organization",
                                 modelsJson){
  
  # study details
  settings <- list(skeletonType = "PatientLevelPredictionValidationStudy")
  
  settings$packageName <- packageName
  settings$organizationName <- organizationName
  settings$description <- description
  settings$skeletonVersion <- skeletonVersion
  settings$createdDate <- Sys.Date()
  settings$createdBy <-  createdBy
  
  cohortDefinitions <- combineCohortDefinitions(modelsJson)
  
  settings$cohortDefinitions <- cohortDefinitions
  
  # models (list) - name/attr_type/attr_predictionType/pop/cov/cohort ids/cohort names/settings(plpModel)
  if(class(modelsJson)=='modelsJson'){
    settings$models <- list(modelsJson$modelSettingsJson)
  }else{
    settings$models <- lapply(1:length(modelsJson), function(i){modelsJson[[i]]$modelSettingsJson})
  }
  
  return(settings)
}

combineCohortDefinitions <- function(modelsJson){
  
  if(class(modelsJson)=='modelsJson'){
    cohortDefinitions <- modelsJson$cohortDefinitions
  } else{
    cohortDefinitions <- lapply(1:length(modelsJson), function(i){modelsJson[[i]]$cohortDefinitions})
    
    # combine into single list
    cohortDefinitionsAll <- cohortDefinitions[[1]]
    
    if(length(cohortDefinitions)>1){
      for(i in 2:length(cohortDefinitions)){
        cohortDefinitionsAll <- append(cohortDefinitionsAll, cohortDefinitions[[i]])
      }
    }
    
    cohortDefinitions <- unique(cohortDefinitionsAll)
    
    # check for duplicate ids
    details <- do.call(rbind, lapply(1:length(cohortDefinitions), function(i){c(cohortDefinitions[[i]]$name,
                                                                                cohortDefinitions[[i]]$id)}))
    
    if(length(unique(details[,2])) != length(details[,2])){
      warning('Multiple ids with different cohort names - this will cause issues...')
    }
    
  }
  
  return(cohortDefinitions)
}


transportModelsToJson <- function(modelJson, packageLoc){
  
  if(!dir.exists(file.path(packageLoc,'inst', 'models'))){
    dir.create(file.path(packageLoc,'inst', 'models'), recursive = T)
  }
  
  # save model/covMap/varImp to list to save as files after Hydra
  if(class(modelJson) == 'modelsJson'){
    saveModelJson(modelJson, packageLoc)
  } else{
    lapply(1:length(modelJson), function(i){saveModelJson(modelJson[[i]], packageLoc)})
  }
  
  return(invisible(NULL))
}

saveModelJson <- function(modelJson, packageLoc){
  
  if(!dir.exists(file.path(packageLoc,'inst', 'models', modelJson$modelSettingsJson$name))){
    dir.create(file.path(packageLoc,'inst', 'models',modelJson$modelSettingsJson$name), recursive = T)
  }
  
  #save varImp and covariateMap
  write.csv(x = modelJson$varImp,
            file = file.path(packageLoc,'inst', 'models', modelJson$modelSettingsJson$name,'varImp.csv'),
            row.names = F)
  write.csv(x = modelJson$covariateMap,
            file = file.path(packageLoc,'inst', 'models', modelJson$modelSettingsJson$name,'covariateMap.csv'),
            row.names = F)
  
  
  if(modelJson$modelSettingsJson$attr_type == 'xgboost'){
    
    xgboost::xgb.save(model = modelJson$model,
                      fname = file.path(packageLoc,'inst', 'models', modelJson$modelSettingsJson$name,'model.json')
    )
    
    #file.copy(from =  modelsJson$model,
    #          to = file.path(packageLoc,'inst', 'models', modelsJson$modelSettingsJson$name,'model.json'))
    
  }else if(modelJson$modelSettingsJson$attr_type %in% c('pythonJson','pythonReticulate')){
    
    #load pick and save as json
    covertPickleToJson(pickLoc = modelJson$model,
                       jsonLoc = file.path(packageLoc,'inst', 'models', modelJson$modelSettingsJson$name,'model.json'))
  } else{
    modAsJson <- RJSONIO::toJSON(x = modelJson$model, digits = 23)
    write(modAsJson, file.path(packageLoc,'inst', 'models', modelJson$modelSettingsJson$name,'model.json'))
  }
  
  return(invisible(NULL))
}

covertPickleToJson <- function(pickLoc, jsonLoc){
  
  jb <- reticulate::import('joblib')
  model <- jb$load(file.path(pickLoc,"model.pkl"))
  
  skljson <- reticulate::import('sklearn_json')
  skljson$to_json(model = model, model_name =  jsonLoc)
  
  return(invisible(NULL))
}

createValidationPackageList <- function(modelJson,
                                    outputFolder,
                                    packageName,
                                    description = 'missing',
                                    skeletonVersion = 'v1.0.0',
                                    createdBy = 'anonymous',
                                    organizationName = 'none'){
  
  if(missing(modelJson)){
    stop('Must Enter modelJson')
  }
  if(missing(outputFolder)){
    stop('Must Enter outputFolder')
  }
  if(missing(packageName)){
    stop('Must Enter packageName')
  }
  
  packageLoc <- file.path(outputFolder, packageName)
  
  jsonSet <- createValidationJson(packageName = packageName,
                                  description = description,
                                  skeletonVersion = skeletonVersion,
                                  createdBy = createdBy,
                                  organizationName = organizationName,
                                  modelsJson = modelJson)
  
  jsonSet <- RJSONIO::toJSON(jsonSet, digits = 23)
  Hydra::hydrate(jsonSet,
                 outputFolder = packageLoc)
  
  transportModelsToJson(modelJson, packageLoc)
  
  return(packageLoc)
}




