# Borrowed from devtools: https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L44
is_installed <- function (pkg, version = 0) {
  installed_version <- tryCatch(utils::packageVersion(pkg), 
                                error = function(e) NA)
  !is.na(installed_version) && installed_version >= version
}

# Borrowed and adapted from devtools: https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L74
ensure_installed <- function(pkg) {
  if (!is_installed(pkg)) {
    msg <- paste0(sQuote(pkg), " must be installed for this functionality.")
    if (interactive()) {
      message(msg, "\nWould you like to install it?")
      if (utils::menu(c("Yes", "No")) == 1) {
        if(pkg%in%c('Hydra')){
          if(!is_installed('devtools')){
            utils::install.packages('devtools')
          }
          devtools::install_github(paste0('OHDSI/',pkg))
        }else{
          utils::install.packages(pkg)
        }
      } else {
        stop(msg, call. = FALSE)
      }
    } else {
      stop(msg, call. = FALSE)
    }
  }
}



addExecutionSettings <- function(covariateSettings,
                                 cohortDatabaseSchema,
                                 cohortTable){
  
  #createCovariateSettings <- FeatureExtraction::createCovariateSettings
  #createCovariateSettings <- function(x){return(x)}
  if(covariateSettings$fnct == 'createCovariateSettings'){
    return(covariateSettings$settings)
  }
  
  if(covariateSettings$fnct == 'createCohortCovariateSettings'){
    covariateSettings$settings$cohortDatabaseSchema <- cohortDatabaseSchema
    covariateSettings$settings$cohortTable <- cohortTable
  }
  
  
  if('scaleMap' %in% names(covariateSettings$settings)){
    covariateSettings$settings$scaleMap <- eval(str2lang(paste0(covariateSettings$settings$scaleMap, collapse = ' ')))
  }
  
  
  res <- do.call(covariateSettings$fnct, covariateSettings$settings)
  return(res)
}

evaluateCovariateSettings <- function(covariateSettings,
                                      cohortDatabaseSchema,
                                      cohortTable){
  
  for(i in 1:length(covariateSettings)){
    covariateSettings[[i]] <- lapply(1:length(covariateSettings[[i]]), function(j){addExecutionSettings(covariateSettings[[i]][[j]],
                                                                                                        cohortDatabaseSchema,
                                                                                                        cohortTable)} )
  }
  
  return(covariateSettings)
}
