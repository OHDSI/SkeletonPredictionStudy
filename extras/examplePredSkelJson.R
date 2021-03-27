# code to create the json prediction:
webApi = 'https://yourWebAPI'

populationSettings <- list(PatientLevelPrediction::createStudyPopulationSettings(riskWindowEnd = 365),
                           PatientLevelPrediction::createStudyPopulationSettings(riskWindowEnd = 730))
modelList <- list(list("LassoLogisticRegressionSettings" = list("variance" = 0.01)),
                  list("AdaBoostSettings" =  list("nEstimators" = c(25,50),
                                             "learningRate" = 0.1))
                  )
covariateSettings <- list(list(list(fnct = 'createCovariateSettings',
                                    settings = FeatureExtraction::createCovariateSettings(useDemographicsGender = T)),
                               list(fnct = 'createCohortCovariateSettings', 
                                    settings = list(covariateName = 'test', covariateId = 16442956,
                                                    cohortId = 16442,
                                                    startDay=-30, endDay=0, count=F, 
                                                    ageInteraction = F))),
                          list(fnct = 'createCovariateSettings',
                               settings = FeatureExtraction::createCovariateSettings(useDemographicsGender = T))
)

resrictOutcomePops <- data.frame(outcomeId = c(16428,16435),
                                 populationSettingId = c(1,2))
resrictModelCovs = data.frame(modelSettingId = c(1,1,2),
                              covariateSettingId = c(1,2,1))

executionSettings <- list(washoutPeriod = 365,
                            minCovariateFraction = 0.001,
                            normalizeData = T,
                            testSplit = "stratified",
                            testFraction = 0.25,
                            splitSeed = 1250,
                            nfold = 3)

json <- createDevelopmentStudyJson(packageName = 'exampleStudy',
                           packageDescription = 'an example of the skeleton',
                           createdBy = 'add name',
                           organizationName = 'add organization',
                           targets = data.frame(targetId = c(16425,16377,16387),
                                                cohortId = c(16425,16377,16387),
                                                targetName = c('diabetes','liver issues','fall')),
                           outcomes = data.frame(outcomeId = c(16428,16435),
                                                 cohortId = c(16428,16435),
                                                 outcomeName = c('congestive heart failure','afib')),
                           populationSettings = populationSettings,
                           modelList = modelList,
                           covariateSettings = covariateSettings,
                           resrictOutcomePops = resrictOutcomePops,
                           resrictModelCovs = resrictModelCovs,
                           executionSettings = executionSettings,
                           webApi = webApi,
                           outputLocation = 'D:/testing/',
                           jsonName = 'predictionAnalysisList.json')

specifications <- Hydra::loadSpecifications(file.path('D:/testing', 'predictionAnalysisList.json'))
Hydra::hydrate(specifications = specifications, outputFolder = 'D:/testing/package')
