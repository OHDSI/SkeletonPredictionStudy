source(file.path(getwd(),'extras/createDevelopmentPackageFunctions.R'))
source(file.path(getwd(),'R/BackwardsComp.R'))

convertCohortDef <- function(cohortDefinitions){
  # cohortName, cohortId, json, sql
  cohortDefinitionsNew <- data.frame(
    cohortName = unlist(lapply(cohortDefinitions, function(x) x$name)),
    cohortId = unlist(lapply(cohortDefinitions, function(x) x$id)),
    json = unlist(lapply(cohortDefinitions, function(x) as.character(ParallelLogger::convertSettingsToJson(x$expression)))),
    sql = unlist(lapply(cohortDefinitions, function(x){
      CirceR::buildCohortQuery(
        as.character(ParallelLogger::convertSettingsToJson(x$expression)), 
        options = CirceR::createGenerateOptions(
          generateStats =  F
        )
      )
    }
    )
    )
  )
  return(cohortDefinitionsNew)
}

createStudyPackageFromATLASJson <- function(
  jsonLocation,
  studyPackageLocation,
  skeletonLocation = getwd()
  ){
  
  # apply backwards
  analysisJson <- backwards(jsonLocation)
  
  packageName <- analysisJson$packageName
  studyPackageLocation <- file.path(studyPackageLocation, packageName)
  
  # create studyPackageLocation (if empty)
  if(dir.exists(studyPackageLocation)){
    stop('Package location exists - please pick a different location or delete existing files')
  } else{
    dir.create(studyPackageLocation, recursive = T)
  }
  
  # convert the cohortDefinitions to:
  # cohortName, cohortId, json, sql
  analysisJson$cohortDefinitions <- convertCohortDef(
    analysisJson$cohortDefinitions
    )
  
  # copy the skeleton to studyPackageLocation
  if(!dir.exists(studyPackageLocation)){
    dir.create(studyPackageLocation, recursive = T)
  }
  # copy the skeleton to the output location
  file.copy(list.files(skeletonLocation, full.names = TRUE), 
            to = studyPackageLocation, 
            recursive = TRUE
  )
  
  # rename 
  replaceName(
    packageLocation = studyPackageLocation,
    packageName = packageName,
    skeletonType = 'SkeletonPredictionStudy'
  )
  
  # save json fileinto package
  saveAnalysisJson(
    packageLocation = studyPackageLocation,
    jsonList = analysisJson
  )
  
  # download cohorts + create the cohortsToCreate.csv
  saveCohorts(
    packageLocation = studyPackageLocation,
    analysisList = analysisJson
  )
  
  return(studyPackageLocation)
}

# testing using old json in package
createStudyPackageFromATLASJson(
  jsonLocation = file.path(getwd(), 'inst', 'settings', 'predictionAnalysisList_old.json'),
  studyPackageLocation = file.path('/Users/jreps/Documents/testAtlas'),
  skeletonLocation = getwd()
)
