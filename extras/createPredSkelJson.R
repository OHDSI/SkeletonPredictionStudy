createDevelopmentStudyJson <- function(packageName = 'exampleStudy',
                                       packageDescription = 'an example of the skeleton',
                                       createdBy = 'add name',
                                       organizationName = 'add organization',
                                       targets = data.frame(targetId = c(1,2,3),
                                                            targetName = c('Example 1','Example 2','Example 3')),
                                       outcomes = data.frame(outcomeId = c(4,5),
                                                             outcomeName = c('Example 4','Example 5')),
                                       populationSettings = list(),
                                       modelList = list(),
                                       covariateSettings = list(),
                                       resrictOutcomePops =NULL,
                                       resrictModelCovs = NULL,
                                       resrictTargetCovs = NULL,
                                       executionSettings = list(),
                                       webApi = 'https://...',
                                       outputLocation = './',
                                       jsonName = 'predictionAnalysisList.json'){
  
  json <- list()
  
  json$skeletonType <-  "PatientLevelPredictionStudy"
  json$skeletonVersion <- "v0.0.5"  #update?
  json$packageName <- packageName
  json$description <- packageDescription
  
  json$createdBy <- createdBy
  json$organizationName <-  organizationName
  json$createdDate <- Sys.Date()
  
  json$runPlpArgs <- executionSettings
  json$getPlpDataArgs<- list(washoutPeriod = 0)
  json$targetIds <- unique(targets$targetId)
  json$targetNames <- unique(targets$targetName)
  json$outcomeIds <- unique(outcomes$outcomeId)
  json$outcomeNames <- unique(outcomes$outcomeName)
  
  cohortsToCreate <- getCohortDetails(targets, outcomes, covariateSettings)
  json$cohortDefinitions  <- getCohorts(cohortsToCreate,
                                        baseUrl = webApi)
  
  
  # restrict settings
  json$settings <- createSettings(targets, 
                                  outcomes, 
                                  populationSettings, 
                                  resrictOutcomePops,
                                  modelList, 
                                  covariateSettings,
                                  resrictModelCovs,
                                  resrictTargetCovs)
  
  
  # add the settings
  json$populationSettings <- populationSettings
  
  json$modelSettings <- modelList
  
  json$covariateSettings <- addAttr(covariateSettings)
  
  if(!dir.exists(outputLocation)){
    dir.create(outputLocation, recursive = T)
  }
  
  #json <- RJSONIO::toJSON(json)
  #ParallelLogger::saveSettingsToJson(json,
  #                                   file.path(outputLocation, jsonName))
  
  exportJson <- RJSONIO::toJSON(json, digits = 23)
  write(exportJson, file=file.path(outputLocation, jsonName))
  
  return(json)
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


getCohortId <- function(x){
  if('cohortId' %in% names(x)){
    return(data.frame(name = ifelse(!is.null(x$covariateName), x$covariateName, paste0('covariate_',x$cohortId)),
                      cohortId = x$cohortId)
    )
  }
  return(NULL)
}

# returns cohortIds in the covariate settings or NULL if there are none
findCohorts <- function(singleCovariateList){
  if(!is.null(singleCovariateList$fnct)){
    singleCovariateList <- list(singleCovariateList)
  }
  result <- do.call(rbind,lapply(singleCovariateList, function(x){getCohortId(x$settings)}))
  return(result)
}

# extracts cohort covariates and create data.frame of ts, os, and covariate cohorts (ids and names)
getCohortDetails <- function(targets, outcomes, covariateSettings){
  
  # create data.frame with columns: name, atlasId, cohortId, type
  targetDetails <- data.frame(name = targets$targetName,
                              atlasId = targets$targetId,
                              cohortId = targets$cohortId,
                              type = 'target')
  
  outcomeDetails <- data.frame(name = outcomes$outcomeName,
                               atlasId = outcomes$outcomeId,
                               cohortId = outcomes$outcomeId,
                               type = 'outcome')
  
  covariateDetails <- unique(do.call(rbind,lapply(covariateSettings, findCohorts)))
  if(!is.null(covariateDetails)){
    covariateDetails <- data.frame(name = covariateDetails$name,
                                   atlasId = covariateDetails$cohortId,
                                   cohortId = covariateDetails$cohortId,
                                   type = 'covariate')
    cohortDetails <- rbind(targetDetails,
                           outcomeDetails,
                           covariateDetails)
  } else{
    cohortDetails <- rbind(targetDetails,
                           outcomeDetails)
  }
  
  return(cohortDetails)
}

# extracts the jsons for cohort each id
getCohorts <- function(cohortsToCreate, baseUrl){
  
  cohortDefinitions <- list()
  length(cohortDefinitions) <- nrow(cohortsToCreate)
  
  for (i in 1:nrow(cohortsToCreate)) {
    writeLines(paste("Extracting cohort:", cohortsToCreate$name[i]))
    cohortDefinitions[[i]] <- ROhdsiWebApi::getCohortDefinition(cohortId = cohortsToCreate$atlasId[i], 
                                                                baseUrl = baseUrl)
    cohortDefinitions[[i]]$expressionSql <- RJSONIO::toJSON(cohortDefinitions[[i]]$expression)
    cohortDefinitions[[i]]$name = cohortsToCreate$name[i]
  }
  
  return(cohortDefinitions)
  
}

# create the settings data.frame 
createSettings <- function(targets, 
                           outcomes, 
                           populationSettings, 
                           resrictOutcomePops = NULL,
                           modelList, 
                           covariateSettings,
                           resrictModelCovs = NULL,
                           resrictTargetCovs= NULL){
  
  #cartesian product of tId, oId, modelId, covId, popId
  settings <- split(expand.grid(cohortId = targets$targetId,
                                outcomeId = outcomes$outcomeId,
                                populationSettingId = 1:length(populationSettings),
                                modelSettingId = 1:length(modelList),
                                covariateSettingId = 1:length(covariateSettings)), 
                    1:(length(targets$targetId) * length(outcomes$outcomeId)* length(populationSettings)* length(modelList)* length(covariateSettings)))
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


replaceName <- function(packageLocation = getwd(), 
                        packageName = 'ValidateRCRI'){
  
  # rename files:
  #=====
  # file.path(packageLocation,"SkeletonExistingPredictionModelStudy.Rproj")
  # file.path(packageLocation,"R/SkeletonExistingPredictionModelStudy.R")
  filesToRename <- c("SkeletonExistingPredictionModelStudy.Rproj","R/SkeletonExistingPredictionModelStudy.R")
  for(f in filesToRename){
    ParallelLogger::logInfo(paste0('Renaming ', f))
    fnew <- gsub("SkeletonExistingPredictionModelStudy", packageName, f)
    file.rename(from = file.path(packageLocation,f), to = file.path(packageLocation,fnew))
  }
  
  # edit test in files:
  #=====
  # file.path(packageLocation,"DESCRIPTION")
  # file.path(packageLocation,"README.md")
  # file.path(packageLocation,"extras/CodeToRun.R")
  # each file in dir(file.path(packageLocation,"R"))
  
  filesToEdit <- c(file.path(packageLocation,"DESCRIPTION"),
                   file.path(packageLocation,"README.md"),
                   file.path(packageLocation,"extras/CodeToRun.R"),
                   dir(file.path(packageLocation,"R"), full.names = T))
  for( f in filesToEdit ){
    ParallelLogger::logInfo(paste0('Editing ', f))
    x <- readLines(f)
    y <- gsub( "SkeletonExistingPredictionModelStudy", packageName, x )
    cat(y, file=f, sep="\n")
    
  }
}