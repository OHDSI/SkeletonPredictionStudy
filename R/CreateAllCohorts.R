# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of SkeletonPredictionStudy
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

createCohorts <- function(
  databaseDetails,
  outputFolder
) {
  if (!file.exists(outputFolder))
    dir.create(outputFolder)
  
  connection <- DatabaseConnector::connect(databaseDetails$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  
  CohortGenerator::createCohortTables(connection = connection,
                                      cohortDatabaseSchema = databaseDetails$cohortDatabaseSchema,
                                      cohortTableNames = CohortGenerator::getCohortTableNames(
                                        cohortTable = databaseDetails$cohortTable
                                      )
  )
  cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(packageName = "SkeletonPredictionStudy",
                                                                 settingsFileName = "Cohorts.csv",
                                                                 cohortFileNameValue = "cohortId")
  CohortGenerator::generateCohortSet(connection = connection,
                                     cohortDatabaseSchema = databaseDetails$cohortDatabaseSchema,
                                     #cohortTableNames = databaseDetails$cohortTable,
                                     cohortTableNames = CohortGenerator::getCohortTableNames(
                                       cohortTable = databaseDetails$cohortTable
                                     ),
                                     cdmDatabaseSchema = databaseDetails$cdmDatabaseSchema,
                                     tempEmulationSchema = databaseDetails$tempEmulationSchema,
                                     cohortDefinitionSet = cohortDefinitionSet)
  

  # Check number of subjects per cohort:
  message("Counting cohorts")
  counts <- CohortGenerator::getCohortCounts(connection = connection,
                                             cohortDatabaseSchema = databaseDetails$cohortDatabaseSchema,
                                             cohortTable = databaseDetails$cohortTable)
  
  counts <- addCohortNames(counts)
  utils::write.csv(counts, file.path(outputFolder, "CohortCounts.csv"), row.names = FALSE)
}

addCohortNames <- function(data) {
  pathToCsv <- system.file("Cohorts.csv", package = "SkeletonPredictionStudy")
  
  idToName <- utils::read.csv(pathToCsv)
  idToName <- idToName[order(idToName$cohortId), ]
  idToName <- idToName[!duplicated(idToName$cohortId), ]
  
  data <- merge(data, idToName, all.x = TRUE)
  
  return(data)
}