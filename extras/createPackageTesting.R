createPackage <- function(
  jsonFileLocation, 
  baseUrl,
  skeletonLocation, 
  outputLocation,
  packageName){
  
  
  packageLocation <- file.path(outputLocation, packageName)
  
  jsonList <- PatientLevelPrediction::loadPlpAnalysesJson(jsonFileLocation)

  # create the output location
  if(!dir.exists(packageLocation)){
    dir.create(packageLocation, recursive = T)
  }
  
  # copy the skeleton to the output location
  file.copy(list.files(skeletonLocation, full.names = TRUE), 
    to = packageLocation, 
    recursive = TRUE)
  
  # replace 'SkeletonPredictionStudy' with packageName 
  replaceName(
    packageLocation = packageLocation,
    packageName = packageName,
    skeletonType = 'SkeletonPredictionStudy'
    )
  
  # save json fileinto package
  copyAnalysisJson(
    packageLocation = packageLocation,
    jsonLocation = jsonFileLocation
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
copyAnalysisJson <- function(
  packageLocation,
  jsonLocation
  ){
  
  file.copy(
    from = jsonLocation, 
    to = file.path(packageLocation, 'inst', 'settings', 'predictionAnalysisList.json'), 
    overwrite = T)
  
  return(packageLocation)
}

# create cohorts to create from cohortDefinitions
# save json and convert+save sql into inst/cohorts and inst/sql/sql_server
saveCohorts <- function(
  packageLocation,
  analysisList,
  baseUrl
  ){
  
  nameForFile <- function(name){
    name <- gsub(' ','_', name)
    name <- gsub("[[:punct:]]", "_", name)
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
