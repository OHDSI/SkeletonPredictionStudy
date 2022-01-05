# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of SkeletonCompartiveEffectStudy
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

#' Create the exposure and outcome cohorts
#'
#' @details
#' This function will create the exposure and outcome cohorts following the definitions included in
#' this package.
#'
#' @param databaseDetails      The databaseDetails
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/)                         
#'
#' @export
createCohorts <- function(databaseDetails,
                          outputFolder) {
  if (!file.exists(outputFolder)){
    dir.create(outputFolder)
  }
  
  conn <- DatabaseConnector::connect(databaseDetails$connectionDetails)
  
  .createCohorts(
    connection = conn,
    cdmDatabaseSchema = databaseDetails$cdmDatabaseSchema,
    cohortDatabaseSchema = databaseDetails$cohortDatabaseSchema,
    cohortTable = databaseDetails$cohortTable,
    tempEmulationSchema = databaseDetails$tempEmulationSchema,
    outputFolder = outputFolder
  )
  
  # Check number of subjects per cohort:
  ParallelLogger::logInfo("Counting cohorts")
  sql <- SqlRender::loadRenderTranslateSql(
    "GetCounts.sql",
    "SkeletonPredictionStudy",
    dbms = connectionDetails$dbms,
    tempEmulationSchema = databaseDetails$tempEmulationSchema,
    cdm_database_schema = databaseDetails$cdmDatabaseSchema,
    work_database_schema = databaseDetails$cohortDatabaseSchema,
    study_cohort_table = databaseDetails$cohortTable
  )
  counts <- DatabaseConnector::querySql(conn, sql)
  colnames(counts) <- SqlRender::snakeCaseToCamelCase(colnames(counts))
  counts <- addCohortNames(counts)
  utils::write.csv(counts, file.path(outputFolder, "CohortCounts.csv"), row.names = FALSE)
  
  DatabaseConnector::disconnect(conn)
}

addCohortNames <- function(data, IdColumnName = "cohortDefinitionId", nameColumnName = "cohortName") {
  pathToCsv <- system.file("settings", "CohortsToCreate.csv", package = "SkeletonPredictionStudy")
  cohortsToCreate <- utils::read.csv(pathToCsv)
  
  idToName <- data.frame(cohortId = c(cohortsToCreate$cohortId),
                         cohortName = c(as.character(cohortsToCreate$name)))
  idToName <- idToName[order(idToName$cohortId), ]
  idToName <- idToName[!duplicated(idToName$cohortId), ]
  names(idToName)[1] <- IdColumnName
  names(idToName)[2] <- nameColumnName
  data <- merge(data, idToName, all.x = TRUE)
  # Change order of columns:
  idCol <- which(colnames(data) == IdColumnName)
  if (idCol < ncol(data) - 1) {
    data <- data[, c(1:idCol, ncol(data) , (idCol+1):(ncol(data)-1))]
  }
  return(data)
}
