createDevelopmentSkeletonSettings <- function(
  packageName = 'BestPrediction',
  skeletonVersion = 'v1.0.0',
  organizationName = "your name",
  modelDesignList = list(),
  baseUrl,
  saveDirectory = NULL
){
  
  if(class(modelDesignList) == 'modelDesign'){
    modelDesignList <- list(modelDesignList)
  }
  
  if(class(modelDesignList[[1]]) != 'modelDesign'){
    stop('Incorrect modelDesignList')
  }
  
  cohortDefinitions <- getCohortDefinitions(
    modelDesignList = modelDesignList,
    baseUrl = baseUrl
  )
  
  jsonSettings <- list()
  jsonSettings$skeletonType <-  "PatientLevelPredictionStudy"
  jsonSettings$organizationName <- organizationName
  jsonSettings$skeletonVersion <- skeletonVersion
  jsonSettings$packageName <- packageName 
  
  jsonSettings$analysis <- modelDesignList
  
  jsonSettings$cohortDefinitions <- cohortDefinitions
  
  if(!is.null(saveDirectory)){
    
    fileName <- file.path(saveDirectory, 'predictionAnalysisList.json')
    if(!dir.exists(saveDirectory)){
      ParallelLogger::logInfo('Creating saveDirectory')
      dir.create(saveDirectory, recursive = T)
    }
    ParallelLogger::logInfo('Saving jsonsettings')
    
    ParallelLogger::saveSettingsToJson(
      object = jsonSettings,
      fileName = fileName
      )
    
  }
  
  return(invisible(jsonSettings))
}



getCohortDefinitions <- function(
  modelDesignList,
  baseUrl
){
  
  ParallelLogger::logInfo('Finding cohorts to extract')
  
  # get outcome and target ids
  componentIds <- c(
    unlist(lapply(modelDesignList, function(x) x$targetId)),
    unlist(lapply(modelDesignList, function(x) x$outcomeId))
  )
  
  covariateIds <- c()
  for(i in 1:length(modelDesignList)){
    if(class(modelDesignList[[i]]$covariateSettings) == 'covariateSettings'){
      modelDesignList[[i]]$covariateSettings <- list(modelDesignList[[i]]$covariateSettings)
    }
    
    newCovariateIds <- unlist(lapply(modelDesignList[[i]]$covariateSettings, function(x) x[grep('cohortId', names(x))]))
    
    covariateIds <- c(covariateIds, newCovariateIds)
  }
  
  allIds <- unique(c(componentIds, covariateIds))
  
  ParallelLogger::logInfo('Extracting cohorts using webapi')
  
  cohortDefinitions <- list()
  length(cohortDefinitions) <- length(allIds)
  for (i in 1:length(allIds)) {
    ParallelLogger::logInfo(paste("Extracting cohort:", allIds[i]))
    cohortDefinitions[[i]] <- ROhdsiWebApi::getCohortDefinition(
      cohortId = allIds[i], 
      baseUrl = baseUrl
    )
    
    ParallelLogger::logInfo(paste0('Extracted ', cohortDefinitions[[i]]$name ))
  }
  
  return(cohortDefinitions)
}


createDevelopmentPackage <- function(
  jsonList = NULL,
  jsonFileLocation = NULL, 
  baseUrl,
  skeletonLocation,
  skeletonUrl = "https://github.com/ohdsi/SkeletonPredictionStudy/archive/main.zip",
  outputLocation,
  packageName){
  
  packageLocation <- file.path(outputLocation, packageName)
  
  if(is.null(jsonList)){
    jsonList <- PatientLevelPrediction::loadPlpAnalysesJson(jsonFileLocation)
  }

  # create the output location
  if(!dir.exists(outputLocation)){
    dir.create(outputLocation, recursive = T)
  }
  
  if(!missing(skeletonLocation)){
    if(!dir.exists(packageLocation)){
      dir.create(packageLocation, recursive = T)
    }
  # copy the skeleton to the output location
  file.copy(list.files(skeletonLocation, full.names = TRUE), 
    to = packageLocation, 
    recursive = TRUE
    )
  } else if(!missing(skeletonUrl)){
    utils::download.file(
      url = skeletonUrl,
      destfile = file.path(outputLocation,"plp-skeleton.zip")
      )
    # unzip the .zip file
    utils::unzip(zipfile = file.path(outputLocation, "plp-skeleton.zip"), 
                 exdir = outputLocation
                 )
    #rename
    file.rename(file.path(outputLocation,'SkeletonPredictionStudy-main'), packageLocation)
    
  } else{
    stop('Please enter either skeletonLocation or skeletonUrl')
  }
  
  
  # replace 'SkeletonPredictionStudy' with packageName 
  replaceName(
    packageLocation = packageLocation,
    packageName = packageName,
    skeletonType = 'SkeletonPredictionStudy'
    )
  
  # save json fileinto package
  saveAnalysisJson(
    packageLocation = packageLocation,
    jsonList = jsonList
    )
  
  # download cohorts + create the cohortsToCreate.csv
  saveCohorts(
    packageLocation = packageLocation,
    analysisList = jsonList,
    baseUrl = baseUrl
    )
  

}


# Helpers
# change name
replaceName <- function(
  packageLocation = getwd(),
  packageName = 'ValidateRCRI',
  skeletonType = 'SkeletonPredictionValidationStudy'
  ){
  
  filesToRename <- c(paste0(skeletonType,".Rproj"),paste0("R/",skeletonType,".R"))
  for(f in filesToRename){
    ParallelLogger::logInfo(paste0('Renaming ', f))
    fnew <- gsub(skeletonType, packageName, f)
    file.rename(from = file.path(packageLocation,f), to = file.path(packageLocation,fnew))
  }
  
  filesToEdit <- c(
    file.path(packageLocation,"DESCRIPTION"),
    file.path(packageLocation,"README.md"),
    file.path(packageLocation,"extras/CodeToRun.R"
      ),
    dir(file.path(packageLocation,"R"), full.names = T)
    )
  for( f in filesToEdit ){
    ParallelLogger::logInfo(paste0('Editing ', f))
    x <- readLines(f)
    y <- gsub( skeletonType, packageName, x )
    cat(y, file=f, sep="\n")
    
  }
  
  return(packageName)
}

# save json file into isnt/settings/predictionAnalysisList.json
saveAnalysisJson <- function(
  packageLocation,
  jsonList
  ){
  
  ParallelLogger::saveSettingsToJson(
    object = jsonList$analysis, 
    fileName = file.path(packageLocation, 'inst', 'settings', 'predictionAnalysisList.json')
    )
  
  return(packageLocation)
}

# create cohorts to create from cohortDefinitions
# save json and convert+save sql into inst/cohorts and inst/sql/sql_server
saveCohorts <- function(
  packageLocation,
  analysisList,
  baseUrl
  ){
  

  details <- lapply(
    1:length(analysisList$cohortDefinitions), 
    function(i){
      c(
        cohort_name = analysisList$cohortDefinitions[[i]]$name,
        cohort_id = analysisList$cohortDefinitions[[i]]$id,
        web_api_cohort_id = analysisList$cohortDefinitions[[i]]$id 
      )
    }
  )
  details <- do.call('rbind', details)
  details <- as.data.frame(details, stringsAsFactors = F)

  write.csv(
    x = details,
    file = file.path(packageLocation, 'inst','Cohorts.csv'),
    row.names = F
    )
  
  #CohortGenerator::getCohortDefinitionSet()
  
  # make sure cohorts and sql/sql_server exist
  if(!dir.exists(file.path(packageLocation, 'inst', 'cohorts'))){
    dir.create(file.path(packageLocation, 'inst', 'cohorts'), recursive = T)
  }
  if(!dir.exists(file.path(packageLocation, 'inst', 'sql', 'sql_server'))){
    dir.create(file.path(packageLocation, 'inst', 'sql', 'sql_server'), recursive = T)
  }
  
  # save the cohorts as json
  lapply(
    1:length(analysisList$cohortDefinitions), 
    function(i){
      ParallelLogger::saveSettingsToJson(
        object = analysisList$cohortDefinitions[[i]], 
        fileName = file.path(
          packageLocation, 
          'inst', 
          'cohorts', 
          paste0(analysisList$cohortDefinitions[[i]]$id,'.json')
          )
      )
      #jsonObject  <- jsonlite::toJSON(analysisList$cohortDefinitions[[i]], digits = 23)
      #write(
      #  x = jsonObject,
      #  file = file.path(packageLocation, 'inst', 'cohorts', paste0(analysisList$cohortDefinitions[[i]]$id,'.json'))
      #)
    }
  )
  
  # save the cohorts as sql
  lapply(
    1:length(analysisList$cohortDefinitions), 
    function(i){
      SqlRender::writeSql(
        sql = ROhdsiWebApi::getCohortSql(analysisList$cohortDefinitions[[i]], baseUrl = baseUrl, generateStats = F),
        targetFile = file.path(packageLocation, 'inst', 'sql', 'sql_server', paste0(analysisList$cohortDefinitions[[i]]$id, '.sql'))
      )
    }
  )
  
  return(packageLocation)
}
