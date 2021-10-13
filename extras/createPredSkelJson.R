createDevelopmentStudyPackage <- function(packageName = 'exampleStudy',
                                          packageDescription = 'an example of the skeleton',
                                          createdBy = 'add name',
                                          organizationName = 'add organization',
                                          targetIds = c(1,2,3),
                                          outcomeIds = c(4,5),
                                          populationSettings = list(),
                                          modelList = list(),
                                          covariateSettings = list(),
                                          resrictOutcomePops =NULL,
                                          resrictModelCovs = NULL,
                                          resrictTargetCovs = NULL,
                                          executionSettings = list(),
                                          baseUrl = 'https://...',
                                          outputFolder = getwd(),
                                          useHydra = F,
                                          skeletonVersion = "v0.0.5"){
  
  # check inputs
  
  jsonList <- createDevelopmentStudyJson(packageName = packageName,
                                         packageDescription = packageDescription,
                                         createdBy = createdBy,
                                         organizationName = organizationName,
                                         targetIds = targetIds,
                                         outcomeIds = outcomeIds,
                                         populationSettings = populationSettings,
                                         modelList = modelList,
                                         covariateSettings = covariateSettings,
                                         resrictOutcomePops = resrictOutcomePops,
                                         resrictModelCovs = resrictModelCovs,
                                         resrictTargetCovs = resrictTargetCovs,
                                         executionSettings = executionSettings,
                                         baseUrl = baseUrl)
  
  if(useHydra){
    jsonList$skeletonVersion <- skeletonVersion
    json <- RJSONIO::toJSON(jsonList, digits = 23)
    Hydra::hydrate(json, outputFolder = outputFolder)
  } else{
    
    packageLocation <- downLoadSkeleton(outputFolder = outputFolder,
                                        packageName = packageName,
                                        skeletonType = 'SkeletonPredictionStudy')
    
    replaceName(packageLocation = packageLocation,
                packageName = packageName,
                skeletonType = 'SkeletonPredictionStudy')
    
    saveAnalysisJson(packageLocation = packageLocation,
                     analysisList = jsonList)
    
    saveCohorts(packageLocation = packageLocation,
                analysisList = jsonList,
                baseUrl = baseUrl)
  }
  
  return(invisible(NULL))
}


createDevelopmentStudyJson <- function(packageName = 'exampleStudy',
                                       packageDescription = 'an example of the skeleton',
                                       createdBy = 'add name',
                                       organizationName = 'add organization',
                                       targetIds = c(1,2,3),
                                       outcomeIds = c(4,5),
                                       populationSettings = list(),
                                       modelList = list(),
                                       covariateSettings = list(),
                                       resrictOutcomePops =NULL,
                                       resrictModelCovs = NULL,
                                       resrictTargetCovs = NULL,
                                       executionSettings = list(),
                                       baseUrl = 'https://...'#,
                                       #outputLocation = './',
                                       #jsonName = 'predictionAnalysisList.json'
                                       ){
  
  result <- list()
  
  result$skeletonType <-  "PatientLevelPredictionStudy"
  result$packageName <- packageName
  result$description <- packageDescription
  
  result$createdBy <- createdBy
  result$organizationName <-  organizationName
  result$createdDate <- Sys.Date()
  
  result$runPlpArgs <- executionSettings
  result$getPlpDataArgs<- list(washoutPeriod = 0)
  result$targetIds <- unique(targetIds)
  result$outcomeIds <- unique(outcomeIds)
  
  #cohortsToCreate <- getCohortDetails(targets, outcomes, covariateSettings)
  #json$cohortDefinitions  <- getCohorts(cohortsToCreate,
  #                                      baseUrl = webApi)
  #
  
  # cohort definitions
  ##cohortIds <- unique(unlist(lapply(covariateSettings, function(x) x$settings$cohortId)))
  cohortIds <- unique(unlist(lapply(1:length(covariateSettings), function(i) {lapply(covariateSettings[[i]], function(x) x$settings$cohortId)})))
  cohortDefinitionList <- getCohorts(unique(c(targetIds, outcomeIds, cohortIds)), 
                                     baseUrl = baseUrl)
  result$cohortDefinitions <- cohortDefinitionList
  
  # restrict settings
  result$settings <- createSettings(targetIds, 
                                  outcomeIds, 
                                  populationSettings, 
                                  resrictOutcomePops,
                                  modelList, 
                                  covariateSettings,
                                  resrictModelCovs,
                                  resrictTargetCovs)
  
  
  # add the settings
  result$populationSettings <- populationSettings
  
  result$modelSettings <- modelList
  
  result$covariateSettings <- addAttr(covariateSettings)
  
  #if(!dir.exists(outputLocation)){
  #  dir.create(outputLocation, recursive = T)
  #}
  
  #json <- RJSONIO::toJSON(json)
  #ParallelLogger::saveSettingsToJson(json,
  #                                   file.path(outputLocation, jsonName))
  
  #exportJson <- RJSONIO::toJSON(json, digits = 23)
  #write(exportJson, file=file.path(outputLocation, jsonName))
  
  return(result)
}


addAttr <- function(covariateSettings){
  #find standard
  if(!is.null(covariateSettings$fnct)){
    if(covariateSettings$fnct =='createCovariateSettings'){
      covariateSettings$settings$attr_fun <- attr(covariateSettings$settings,'fun')
      covariateSettings$settings$attr_class <- attr(covariateSettings$settings,'class')
    }
    return(covariateSettings)
  }
  
  if(!is.null(covariateSettings[[1]]$fnct)){
    for(i in 1:length(covariateSettings)){
        if(covariateSettings[[i]]$fnct =='createCovariateSettings'){
          covariateSettings[[i]]$settings$attr_fun <- attr(covariateSettings[[i]]$settings,'fun')
          covariateSettings[[i]]$settings$attr_class <- attr(covariateSettings[[i]]$settings,'class')
        }
    }
    return(covariateSettings)
  }
  
  if(class(covariateSettings)=='list' && is.null(covariateSettings$fnct))
  for(i in 1:length(covariateSettings)){
    for(j in 1:length(covariateSettings[[i]]))
      if(covariateSettings[[i]][[j]]$fnct =='createCovariateSettings'){
        covariateSettings[[i]][[j]]$settings$attr_fun <- attr(covariateSettings[[i]][[j]]$settings,'fun')
          covariateSettings[[i]][[j]]$settings$attr_class <- attr(covariateSettings[[i]][[j]]$settings,'class')
      }
  }
  
  return(covariateSettings)
  
}

# extracts the jsons for cohort each id
getCohorts <- function(cohortIds,
                       baseUrl){
  cohorts <- list()
  length(cohorts) <- length(cohortIds)
  for(i in 1:length(cohortIds)){
    cohorts[[i]] <- ROhdsiWebApi::getCohortDefinition(cohortId = cohortIds[[i]], baseUrl = baseUrl)
  }
  return(cohorts)
}

# create the settings data.frame 
createSettings <- function(targetIds, 
                           outcomeIds, 
                           populationSettings, 
                           resrictOutcomePops = NULL,
                           modelList, 
                           covariateSettings,
                           resrictModelCovs = NULL,
                           resrictTargetCovs= NULL){
  
  #cartesian product of tId, oId, modelId, covId, popId
  settings <- split(expand.grid(cohortId = targetIds,
                                outcomeId = outcomeIds,
                                populationSettingId = 1:length(populationSettings),
                                modelSettingId = 1:length(modelList),
                                covariateSettingId = 1:length(covariateSettings)), 
                    1:(length(targetIds) * length(outcomeIds)* length(populationSettings)* length(modelList)* length(covariateSettings)))
  settings <- do.call(rbind, settings)
  
  # restrict to resrictOutcomePops if not null
  if(!is.null(resrictOutcomePops)){
    ParallelLogger::logInfo('Restricting to specified oId and populationSettingId pairs...')
    settings <- merge(settings, resrictOutcomePops, by = c('outcomeId','populationSettingId'))
  }
  # restrict to resrictModelCovs if not null
  if(!is.null(resrictModelCovs)){
    ParallelLogger::logInfo('Restricting to specified modelId and covariateSettingId pairs...')
    settings <- merge(settings, resrictModelCovs, by = c('modelSettingId','covariateSettingId'))
  }
  # restrict to resrictTargetCovs if not null
  if(!is.null(resrictTargetCovs)){
    ParallelLogger::logInfo('Restricting to specified targetId and covariateSettingId pairs...')
    settings <- merge(settings, resrictTargetCovs, by.x = c('cohortId','covariateSettingId'),by.y = c('targetId','covariateSettingId'))
  }
  
  #return result
  return(settings)
}

#=====================
# create package functions
#=====================

# code to use skeleton master from github rather than hydra
# download a .zip file of the repository
# from the "Clone or download - Download ZIP" button
# on the GitHub repository of interest
downLoadSkeleton <- function(outputFolder,
                             packageName,
                             skeletonType = 'SkeletonPredictionStudy'){
  # check outputFolder exists
  
  # check file.path(outputFolder,  packageName) does not exist
  
  # download, unzip and rename:
  
  download.file(url = paste0("https://github.com/ohdsi/",skeletonType,"/archive/master.zip")
                , destfile = file.path(outputFolder, "package.zip"))
  # unzip the .zip file
  unzip(zipfile = file.path(outputFolder, "package.zip"), exdir = outputFolder)
  file.rename( from = file.path(outputFolder, paste0(skeletonType, '-master')),
               to = file.path(outputFolder,  packageName))
  unlink(file.path(outputFolder, "package.zip"))
  return(file.path(outputFolder, packageName))
}

# change name
replaceName <- function(packageLocation = getwd(),
                        packageName = 'ValidateRCRI',
                        skeletonType = 'SkeletonPredictionValidationStudy'){
  
  filesToRename <- c(paste0(skeletonType,".Rproj"),paste0("R/",skeletonType,".R"))
  for(f in filesToRename){
    ParallelLogger::logInfo(paste0('Renaming ', f))
    fnew <- gsub(skeletonType, packageName, f)
    file.rename(from = file.path(packageLocation,f), to = file.path(packageLocation,fnew))
  }
  
  filesToEdit <- c(file.path(packageLocation,"DESCRIPTION"),
                   file.path(packageLocation,"README.md"),
                   file.path(packageLocation,"extras/CodeToRun.R"),
                   dir(file.path(packageLocation,"R"), full.names = T))
  for( f in filesToEdit ){
    ParallelLogger::logInfo(paste0('Editing ', f))
    x <- readLines(f)
    y <- gsub( skeletonType, packageName, x )
    cat(y, file=f, sep="\n")
    
  }
  
  return(packageName)
}

# save json file into isnt/settings/predictionAnalysisList.json
saveAnalysisJson <- function(packageLocation,
                             analysisList){
  write(RJSONIO::toJSON(analysisList, digits =23),
        file=file.path(packageLocation, 'inst', 'settings', 'predictionAnalysisList.json')
  )
  
  return(packageLocation)
}

# create cohorts to create from cohortDefinitions
# save json and convert+save sql into inst/cohorts and inst/sql/sql_server
saveCohorts <- function(packageLocation,
                        analysisList,
                        baseUrl){
  
  nameForFile <- function(name){
    writeLines(name)
    name <- gsub(' ','_', name)
    name <- gsub("[[:punct:]]", "_", name)
    writeLines(name)
    return(name)
  }
  
  details <- lapply(1:length(analysisList$cohortDefinitions), function(i){c(name = analysisList$cohortDefinitions[[i]]$name,
                                                                            cohortId = analysisList$cohortDefinitions[[i]]$id,
                                                                            atlasId = analysisList$cohortDefinitions[[i]]$id)})
  details <- do.call('rbind', details)
  details <- as.data.frame(details, stringsAsFactors = F)
  details$name <- nameForFile(details$name) # failing dev
  
  write.csv(x = details,
            file = file.path(packageLocation, 'inst', 'settings','cohortsToCreate.csv'),
            row.names = F)
  
  # make sure cohorts and sql/sql_server exist
  if(!dir.exists(file.path(packageLocation, 'inst', 'cohorts'))){
    dir.create(file.path(packageLocation, 'inst', 'cohorts'), recursive = T)
  }
  if(!dir.exists(file.path(packageLocation, 'inst', 'sql', 'sql_server'))){
    dir.create(file.path(packageLocation, 'inst', 'sql', 'sql_server'), recursive = T)
  }
  
  # save the cohorts as json
  lapply(1:length(analysisList$cohortDefinitions), function(i){
    write(RJSONIO::toJSON(analysisList$cohortDefinitions[[i]], digits = 23),
          file=file.path(packageLocation, 'inst', 'cohorts', paste0(nameForFile(analysisList$cohortDefinitions[[i]]$name),'.json')))
  })
  
  # save the cohorts as sql
  lapply(1:length(analysisList$cohortDefinitions), function(i){
    write(ROhdsiWebApi::getCohortSql(analysisList$cohortDefinitions[[i]], baseUrl = baseUrl),
          file=file.path(packageLocation, 'inst', 'sql', 'sql_server', paste0(nameForFile(analysisList$cohortDefinitions[[i]]$name), '.sql')))
  })
  
  return(packageLocation)
}

#===================
# covariateSettings
#===================
createCohortCovariateSetting <- function(atlasId = 1,
                                         covariateName = '',
                                         startDay=-30,
                                         endDay=0,
                                         count=F,
                                         ageInteraction = F,
                                         lnAgeInteraction= F,
                                         analysisId = 456,
                                         points,
                                         offset = 0,
                                         power = 1){
  
  if(base::missing(points)){
    settings <- list(fnct = 'createCohortCovariateSettings',
                     settings = list(covariateName = covariateName,
                                     covariateId = atlasId*1000+analysisId,
                                     cohortId = atlasId,
                                     startDay = startDay,
                                     endDay = endDay,
                                     count = count,
                                     ageInteraction = ageInteraction,
                                     lnAgeInteraction = lnAgeInteraction,
                                     analysisId = analysisId))
  }else{
    settings <- list(fnct = 'createCohortCovariateSettings',
                     settings = list(covariateName = covariateName,
                                     covariateId = atlasId*1000+analysisId,
                                     cohortId = atlasId,
                                     startDay = startDay,
                                     endDay = endDay,
                                     count = count,
                                     ageInteraction = ageInteraction,
                                     lnAgeInteraction = lnAgeInteraction,
                                     analysisId = analysisId),
                     coeffs  = list(covariateId = atlasId*1000+analysisId,
                                    points = points,
                                    offset = offset,
                                    power = power ))
  }
  
  return(settings)
}


createGenderCovariateSetting <- function(male = T,
                                         points,
                                         offset,
                                         power){
  
  if(base::missing(points)){
    settings <- list(fnct = 'createCovariateSettings',
                     settings = FeatureExtraction::createCovariateSettings(useDemographicsGender = T,
                                                                           includedCovariateIds = ifelse(male, 8507, 8532)*1000+1))
  }else{
    settings <- list(fnct = 'createCovariateSettings',
                     settings = FeatureExtraction::createCovariateSettings(useDemographicsGender = T,
                                                                           includedCovariateIds = ifelse(male, 8507, 8532)*1000+1),
                     coeffs  = list(covariateId = ifelse(male, 8507, 8532)*1000+1,
                                    points = points,
                                    offset = offset,
                                    power = power ))
  }
  
  return(settings)
}

createAgeCovariateSetting <- function(covariateName = 'Age at index',
                                      ageMap = function(x){return(x)},
                                      covariateId = 1458,
                                      analysisId = 458,
                                      points,
                                      offset = 0,
                                      power = 1){
  
  if(base::missing(points)){
    settings <- list(fnct = 'createAgeCovariateSettings',
                     settings = list(covariateName = covariateName,
                                     ageMap = ageMap,
                                     covariateId = covariateId,
                                     analysisId = analysisId))
  }else{
    settings <- list(fnct = 'createAgeCovariateSettings',
                     settings = list(covariateName = covariateName,
                                     ageMap = ageMap,
                                     covariateId = covariateId,
                                     analysisId = analysisId),
                     coeffs  = list(covariateId = covariateId,
                                    points = points,
                                    offset = offset,
                                    power = power ))
  }
  
  return(settings)
}

createMeasurementCovariateSetting <- function(covariateName,
                                              conceptSet,
                                              startDay=-30,
                                              endDay=0,
                                              scaleMap = NULL,
                                              aggregateMethod = 'recent',
                                              imputationValue = 0,
                                              ageInteraction = F,
                                              lnAgeInteraction = F,
                                              lnValue = F,
                                              covariateId = 1466,
                                              analysisId = 466,
                                              points,
                                              offset = 0,
                                              power = 1){
  
  if(base::missing(points)){
    settings <- list(fnct = 'createMeasurementCovariateSettings',
                     settings = list(covariateName = covariateName,
                                     conceptSet = conceptSet,
                                     startDay=startDay,
                                     endDay=endDay,
                                     scaleMap = scaleMap,
                                     aggregateMethod = aggregateMethod,
                                     imputationValue = imputationValue,
                                     ageInteraction = ageInteraction,
                                     lnAgeInteraction = lnAgeInteraction,
                                     lnValue = lnValue,
                                     covariateId = covariateId,
                                     analysisId = analysisId))
  } else{
    settings <- list(fnct = 'createMeasurementCovariateSettings',
                     settings = list(covariateName = covariateName,
                                     conceptSet = conceptSet,
                                     startDay=startDay,
                                     endDay=endDay,
                                     scaleMap = scaleMap,
                                     aggregateMethod = aggregateMethod,
                                     imputationValue = imputationValue,
                                     ageInteraction = ageInteraction,
                                     lnAgeInteraction = lnAgeInteraction,
                                     lnValue = lnValue,
                                     covariateId = covariateId,
                                     analysisId = analysisId),
                     coeffs  = list(covariateId = covariateId,
                                    points = points,
                                    offset = offset,
                                    power = power ))
  }
 
  
  return(settings)
}

createStandardCovariateSetting <- function(settings = FeatureExtraction::createCovariateSettings()){
  settings <- list(fnct = 'createCovariateSettings',
                   settings = settings)
  return(settings)
}

