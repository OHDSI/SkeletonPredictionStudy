source('./extras/createPredSkelJson.R')

# code to create the json prediction:
webApi = 'https://yourWebAPI'

populationSettings <- list(PatientLevelPrediction::createStudyPopulationSettings(riskWindowEnd = 365),
                           PatientLevelPrediction::createStudyPopulationSettings(riskWindowEnd = 730))
modelList <- list(list("LassoLogisticRegressionSettings" = list("variance" = 0.01)),
                  list("AdaBoostSettings" =  list("nEstimators" = c(25,50),
                                             "learningRate" = 0.1))
                  )

covariateGender <- createStandardCovariateSetting(settings = FeatureExtraction::createCovariateSettings(useDemographicsGender = T)
                                                    )

covariateCohortTest <- createCohortCovariateSetting(atlasId = 16442,
                                                    covariateName = 'test', 
                                                    startDay=-30, 
                                                    endDay=0, 
                                                    count=F, 
                                                    ageInteraction = F,
                                                    analysisId = 456)

covariateSetting1 <- list(covariateGender, covariateCohortTest)
covariateSetting2 <- list(covariateGender)
covariateSettings <- list(covariateSetting1, covariateSetting2)

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


createDevelopmentStudyPackage(packageName = 'exampleStudy',
                              packageDescription = 'an example of the skeleton',
                              createdBy = 'add name',
                              organizationName = 'add organization',
                              targetIds = c(16425,16377,16387),
                              outcomeIds = c(16428,16435),
                              populationSettings = populationSettings,
                              modelList = modelList,
                              covariateSettings = covariateSettings,
                              resrictOutcomePops = resrictOutcomePops,
                              resrictModelCovs = resrictModelCovs,
                              executionSettings = executionSettings,
                              baseUrl = webApi,
                              outputFolder = '/Users/jreps/Documents/testing')
