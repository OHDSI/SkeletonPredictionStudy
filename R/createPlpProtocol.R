#' Create a protocol template for the study 
#'
#' @details
#' This function will create a template protocol 
#'
#' @param predictionAnalysisListFile       the location of the json analysis settings
#' @param outputLocation    Directory location where you want the protocol written to
#' @export
createPlpProtocol <- function(predictionAnalysisListFile = NULL, outputLocation = getwd()){
  
  if(is.null(predictionAnalysisListFile)){
    predictionAnalysisListFile <- system.file("settings",
      "predictionAnalysisList.json",
      package = "SkeletonPredictionStudy")
  }
  
  #figure1 <- 'vignettes/Figure1.png'
  figure1 <- system.file("doc",
              "Figure1.png",
              package = "PatientLevelPrediction")
  
  #============== STYLES =======================================================
  style_title <- officer::shortcuts$fp_bold(font.size = 28)
  style_title_italic <- officer::shortcuts$fp_bold(font.size = 30, italic = TRUE)
  style_toc <- officer::shortcuts$fp_bold(font.size = 16)
  style_helper_text <- officer::shortcuts$fp_italic(color = "#FF8C00")
  style_citation <- officer::shortcuts$fp_italic(shading.color = "grey")
  style_table_title <- officer::shortcuts$fp_bold(font.size = 14, italic = TRUE)
  
  style_hidden_text <- officer::shortcuts$fp_italic(color = "#FFFFFF")
  
  #============== VARIABLES ====================================================
  json <- tryCatch({PatientLevelPrediction::loadPlpAnalysesJson(predictionAnalysisListFile)},
                   error=function(cond) {
                     stop('Issue with json file...')
                   })
  
# code the process json
  
  
  concepts <- formatConcepts(json)
  
  modelNames <- unique(unlist(lapply(json$analyses, function(x) attr(x$modelSettings$param, 'settings')$name)))
  
  predictionDf <- unique(
    data.frame(
      targetId = unlist(lapply(json$analyses, function(x) x$targetId)),
      targetName = getNames(json$cohortDefinitions, unlist(lapply(json$analyses, function(x) x$targetId))),
      outcomeId = unlist(lapply(json$analyses, function(x) x$outcomeId)),
      outcomeName = getNames(json$cohortDefinitions, unlist(lapply(json$analyses, function(x) x$outcomeId))),
      tar = unlist(lapply(json$analyses, function(x) getTar(x$populationSettings)))
    )
  )
  names(predictionDf) <-c("Target id","Target Name", "outcome Id" , "Outcome Name","Time at Risk") 
  
  
  
  #-----------------------------------------------------------------------------
  
  #============== CITATIONS =====================================================
  plpCitation <- paste0("Citation:  ", utils::citation("PatientLevelPrediction")$textVersion)
  tripodCitation <- paste0("Citation:  Collins, G., et al. (2017.02.01). 'Transparent reporting of a multivariable prediction model for individual prognosis or diagnosis (TRIPOD): The TRIPOD statement.' from https://www.equator-network.org/reporting-guidelines/tripod-statement/ ")
  progressCitation <- paste0("Citation:  Steyerberg EW, Moons KG, van der Windt DA, Hayden JA, Perel P, Schroter S, Riley RD, Hemingway H, Altman DG; PROGRESS Group. Prognosis Research Strategy (PROGRESS) 3: prognostic model research. PLoS Med. 2013;10(2):e1001381. doi: 10.1371/journal.pmed.1001381. Epub 2013 Feb 5. Review. PubMed PMID: 23393430; PubMed Central PMCID: PMC3564751.")
  rCitation <- paste0("Citation:  R Core Team (2013). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL http://www.R-project.org/.")
  #-----------------------------------------------------------------------------
  
  #============== CREATE DOCUMENT ==============================================
  # create new word document
  doc = officer::read_docx()
  #-----------------------------------------------------------------------------
  
  #============ TITLE PAGE =====================================================
  title <- officer::fpar(
    officer::ftext("Patient-Level Prediction:  ", prop = style_title), 
    officer::ftext(json$packageName, prop = style_title_italic)
  )
  
  doc <- doc %>%
    officer::body_add_par("") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(title) %>%
    officer::body_add_par("") %>%
    officer::body_add_par("") %>%
    officer::body_add_par(paste0("Prepared on:  ", Sys.Date()), style = "Normal") %>%
    officer::body_add_par(paste0("Created by:  ", json$createdBy$name, " (", json$createdBy$email,")"), style = "Normal") %>%
    officer::body_add_break() 
  #-----------------------------------------------------------------------------  
  
  #============ TOC ============================================================
  toc <- officer::fpar(
    officer::ftext("Table of Contents", prop = style_toc)
  )
  
  doc <- doc %>%
    officer::body_add_fpar(toc) %>%
    officer::body_add_toc(level = 2) %>%
    officer::body_add_break() 
  #----------------------------------------------------------------------------- 
  
  #============ LIST OF ABBREVIATIONS ==========================================
  abb <- data.frame(rbind(
    c("AUC", "Area Under the Receiver Operating Characteristic Curve"),
    c("CDM","Common Data Model"),
    c("O","Outcome Cohort"),
    c("OHDSI","Observational Health Data Sciences & Informatics"),
    c("OMOP","Observational Medical Outcomes Partnership"),
    c("T", "Target Cohort"),
    c("TAR", "Time at Risk")
  ))
  names(abb) <- c("Abbreviation","Phrase")
  abb <- abb[order(abb$Abbreviation),]
  
  doc <- doc %>%
    officer::body_add_par("List of Abbreviations", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_table(abb, header = TRUE) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< Rest to be completed outside of ATLAS >>", prop = style_helper_text)
      ))
  #----------------------------------------------------------------------------- 
  
  
  #============ RESPONSIBLE PARTIES ============================================
  doc <- doc %>%
    officer::body_add_par("Responsible Parties", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS ", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("Includes author, investigator, and reviewer names and sponsor information. >>", prop = style_helper_text)
      ))
  #----------------------------------------------------------------------------- 
  
  #============ Executive Summary ==============================================
  
  doc <- doc %>%
    officer::body_add_par("Executive Summary", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< A few statements about the rational and background for this study. >>", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_par(paste0(
      "The objective of this study is to develop and validate patient-level prediction models for ",
      nrow(predictionDf), " prediction problems across ", 
      length(unique(unlist(lapply(json$analyses, function(x) x$targetId)))), " target cohorts and ",
      length(unique(unlist(lapply(json$analyses, function(x) x$outcomeId)))), " outcomes"
      ), 
      style = "Normal") %>%
  
    officer::body_add_par("") %>%
    officer::body_add_par(paste0("The prediction will be implemented using ",
                                 length(modelNames)," algorithms (",
                                 paste(modelNames, collapse = ', '),")."),
                          style = "Normal")
  #----------------------------------------------------------------------------- 
  
  #============ RATIONAL & BACKGROUND ==========================================
  doc <- doc %>%
    officer::body_add_par("Rational & Background", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS.", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("Provide a short description of the reason that led to the initiation of or need for the study and add a short critical review of available published and unpublished data to explain gaps in knowledge that the study is intended to fill. >>", prop = style_helper_text)
      )) 
  #-----------------------------------------------------------------------------
  
  #============ OBJECTIVE ======================================================

  doc <- doc %>%
    officer::body_add_par("Objective", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_par(paste0("The objective is to develop and validate patient-level prediction models for the following prediction problems:"),style = "Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_table(predictionDf, header = TRUE, style = "Table Professional")
  
  #----------------------------------------------------------------------------- 
  
  
  algorithms <- data.frame(rbind(
    c("Lasso Logistic Regression", "Lasso logistic regression belongs to the family of generalized linear models, where a linear combination of the variables is learned and finally a logistic function maps the linear combination to a value between 0 and 1.  The lasso regularization adds a cost based on model complexity to the objective function when training the model.  This cost is the sum of the absolute values of the linear combination of the coefficients.  The model automatically performs feature selection by minimizing this cost. We use the Cyclic coordinate descent for logistic, Poisson and survival analysis (Cyclops) package to perform large-scale regularized logistic regression: https://github.com/OHDSI/Cyclops"),
    c("Gradient boosting machine", "Gradient boosting machines is a boosting ensemble technique and in our framework it combines multiple decision trees.  Boosting works by iteratively adding decision trees but adds more weight to the data-points that are misclassified by prior decision trees in the cost function when training the next tree.  We use Extreme Gradient Boosting, which is an efficient implementation of the gradient boosting framework implemented in the xgboost R package available from CRAN."),
    c("Random forest", "Random forest is a bagging ensemble technique that combines multiple decision trees.  The idea behind bagging is to reduce the likelihood of overfitting, by using weak classifiers, but combining multiple diverse weak classifiers into a strong classifier.  Random forest accomplishes this by training multiple decision trees but only using a subset of the variables in each tree and the subset of variables differ between trees. Our packages uses the sklearn learn implementation of Random Forest in python."),
    c("KNN", "K-nearest neighbors (KNN) is an algorithm that uses some metric to find the K closest labelled data-points, given the specified metric, to a new unlabelled data-point.  The prediction of the new data-points is then the most prevalent class of the K-nearest labelled data-points.  There is a sharing limitation of KNN, as the model requires labelled data to perform the prediction on new data, and it is often not possible to share this data across data sites.  We included the BigKnn classifier developed in OHDSI which is a large scale k-nearest neighbor classifier using the Lucene search engine: https://github.com/OHDSI/BigKnn"),
    c("AdaBoost", "AdaBoost is a boosting ensemble technique. Boosting works by iteratively adding decision trees but adds more weight to the data-points that are misclassified by prior decision trees in the cost function when training the next tree.  We use the sklearn 'AdaboostClassifier' implementation in Python."),
    c("DecisionTree", "A decision tree is a classifier that partitions the variable space using individual tests selected using a greedy approach.  It aims to find partitions that have the highest information gain to separate the classes.  The decision tree can easily overfit by enabling a large number of partitions (tree depth) and often needs some regularization (e.g., pruning or specifying hyper-parameters that limit the complexity of the model). We use the sklearn 'DecisionTreeClassifier' implementation in Python."),
    c("Neural network", "Neural networks contain multiple layers that weight their inputs using an non-linear function.  The first layer is the input layer, the last layer is the output layer the between are the hidden layers.  Neural networks are generally trained using feed forward back-propagation.  This is when you go through the network with a data-point and calculate the error between the true label and predicted label, then go backwards through the network and update the linear function weights based on the error.  This can also be performed as a batch, where multiple data-points are feed through the network before being updated.  We use the sklearn 'MLPClassifier' implementation in Python."),
    c("Naive Bayes","The Naive Bayes algorithm applies the Bayes' theorem with the 'naive' assumption of conditional  independence between every pair of features given the value of the class variable. Based on the likelihood of the data belong to a class and the prior distribution of the class, a posterior distribution is obtained.")
  ))
  names(algorithms) <- c("Algorithm","Description")
  algorithms <- algorithms[order(algorithms$Algorithm),]
  
  #============ METHODS ======================================================
  doc <- doc %>%
    officer::body_add_par("Methods", style = "heading 1") %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Study Design", style = "heading 2") %>%
    officer::body_add_par("This study will follow a retrospective, observational, patient-level prediction design. We define 'retrospective' to mean the study will be conducted using data already collected prior to the start of the study. We define 'observational' to mean there is no intervention or treatment assignment imposed by the study. We define 'patient-level prediction' as a modeling process wherein an outcome is predicted within a time at risk relative to the target cohort start and/or end date.  Prediction is performed using a set of covariates derived using data prior to the start of the target cohort.",style = "Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Figure 1, illustrates the prediction problem we will address. Among a population at risk, we aim to predict which patients at a defined moment in time (t = 0) will experience some outcome during a time-at-risk. Prediction is done using only information about the patients in an observation window prior to that moment in time.", style="Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_img(src = figure1, width = 6.5, height = 2.01, style = "centered") %>%
    officer::body_add_par("Figure 1: The prediction problem", style="graphic title") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext(plpCitation, prop = style_citation)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_par("We follow the PROGRESS best practice recommendations for model development and the TRIPOD guidance for transparent reporting of the model results.", style="Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext(progressCitation, prop = style_citation)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext(tripodCitation, prop = style_citation)
      )) %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Data Source(s)", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS.", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("For each database, provide database full name, version information (if applicable), the start and end dates of data capture, and a brief description of the data source.  Also include information on data storage (e.g. software and IT environment, database maintenance and anti-fraud protection, archiving) and data protection.", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("Important Citations: OMOP Common Data Model:  'OMOP Common Data Model (CDM).' from https://github.com/OHDSI/CommonDataModel.", prop = style_helper_text)
      )) %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Model Designs", style = "heading 2")
  
  
  for(i in 1:length(json$analyses)){
    
    targetCohorts <- data.frame(
      cohortId = json$analyses[[i]]$targetId,
      cohortName = getNames(json$cohortDefinitions, json$analyses[[i]]$targetId),
      description = '<< add >>'
    )
    
    outcomeCohorts <- data.frame(
      outcomeId = json$analyses[[i]]$outcomeId,
      outcomeName = getNames(json$cohortDefinitions, json$analyses[[i]]$outcomeId),
      timeAtRisk = getTar(json$analyses[[i]]$populationSettings),
      description = '<< add >>'
    )
    
    onePopSettings <- data.frame(
      Item = names(unlist(json$analyses[[i]]$populationSettings)),
      Settings = unlist(json$analyses[[i]]$populationSettings)
    )
    
    
    algorithmsFiltered <- algorithms[algorithms$Algorithm == attr(json$analyses[[i]]$modelSettings$param, 'settings')$name,]
    
    if(sum(names(json$analyses[[i]]$modelSettings$param)== "") == length(json$analyses[[i]]$modelSettings$param)){
      
      modelParam <- do.call(rbind, lapply(json$analyses[[i]]$modelSettings$param, function(x) unlist(x)))
      modelParam <- as.data.frame(modelParam)
    } else{
      modelParam <- data.frame(
        names = names(unlist(json$analyses[[i]]$modelSettings$param)), 
        values = unlist(json$analyses[[i]]$modelSettings$param)
        )
      row.names(modelParam) <- NULL
    }
    
    
    if(is.null(json$analyses[[i]]$restrictPlpDataSettings$sampleSize)){
    json$analyses[[i]]$restrictPlpDataSettings$sampleSize <- 'None'
    }
    restrictCohort <- data.frame(
      Item = names(unlist(json$analyses[[i]]$restrictPlpDataSettings)),
      Settings = unlist(json$analyses[[i]]$restrictPlpDataSettings)
    )
    
    doc <- doc %>% 
      officer::body_add_par("Target Cohort [T]", style = "heading 3") %>%
      officer::body_add_par("") %>%
      officer::body_add_fpar(
        officer::fpar(
          officer::ftext("<< Currently cohort definitions need to be grabbed from ATLAS, in a Cohort Definition, Export Tab, from Text View. >>", prop = style_helper_text)
        )) %>%
      officer::body_add_par("") %>%
      officer::body_add_table(targetCohorts, header = TRUE, style = "Table Professional") %>%
      officer::body_add_par("") %>% 
      officer::body_add_par("Data Restriction Settings:") %>%
      officer::body_add_table(restrictCohort, header = TRUE, style = "Table Professional") %>%
      
      officer::body_add_par("") %>%
      officer::body_add_par("Outcome", style = "heading 3") %>%
      officer::body_add_par("") %>%
      officer::body_add_fpar(
        officer::fpar(
          officer::ftext("<< Currently cohort definitions need to be grabbed from ATLAS, in a Cohort Definition, Export Tab, from Text View. >>", prop = style_helper_text)
        )) %>%
      officer::body_add_par("") %>%
      officer::body_add_table(outcomeCohorts, header = TRUE, style = "Table Professional") %>%
      officer::body_add_par("")%>% 
      
      officer::body_add_par("Population Settings", style = "heading 3") %>%
      officer::body_add_par("") %>%
      officer::body_add_table(onePopSettings, header = TRUE, style = "Table Professional") %>% 
      officer::body_add_par("") %>% 
      
      # model settings
      officer::body_add_par("Model hyper-parameters", style = "heading 3") %>%
      officer::body_add_par("") %>%
      officer::body_add_table(algorithmsFiltered, header = TRUE, style = "Table Professional") %>% 
      officer::body_add_par("") %>%
      officer::body_add_table(modelParam, header = TRUE, style = "Table Professional") %>% 
      officer::body_add_par("")
    
    # covariate settings
    
    covStatement <- paste0("The covariates (constructed using records on or prior to the target cohort start date) are used within this prediction mode include the following.")
    
    doc <- doc %>%
      officer::body_add_par("Covariate Settings", style = "heading 2") %>%
      officer::body_add_par("") %>%
      officer::body_add_par(covStatement,
        style="Normal") %>%
      officer::body_add_par("") 
    
    if(class(json$analyses[[i]]$covariateSettings) == 'covariateSettings'){
      json$analyses[[i]]$covariateSettings <- list(json$analyses[[i]]$covariateSettings)
    }
    
    for(j in 1:length(json$analyses[[i]]$covariateSettings)){
      covSettings <- data.frame(
        names = names(unlist(json$analyses[[i]]$covariateSettings[[j]])),
        values = unlist(json$analyses[[i]]$covariateSettings[[j]])
        )
      
      doc <- doc %>% 
        officer::body_add_fpar(
          officer::fpar(
            officer::ftext(paste0("Covariate Settings for", attr(json$analyses[[i]]$covariateSettings[[j]], 'fun')), prop = style_table_title)
          )) %>%
        officer::body_add_table(covSettings, header = TRUE, style = "Table Professional") %>% 
        officer::body_add_par("")
    }
    
    # preprocessing settings
    
    doc <- doc %>%
      officer::body_add_par("Data Preprocessing", style = "heading 2")
    
    if(json$analyses[[i]]$executeSettings$runSampleData){
      
      if(class(json$analyses[[i]]$sampleSettings)=='sampleSettings'){
        json$analyses[[i]]$sampleSettings <- list(json$analyses[[i]]$sampleSettings)
      }
      
      sampleSet <- c()
      for(j in 1:length(json$analyses[[i]]$sampleSettings)){
        sampleSet <- rbind(
          sampleSet, 
          data.frame(
            funct = attr(json$analyses[[i]]$sampleSettings,"fun"),
            setting = names(unlist(json$analyses[[i]]$sampleSettings)),
            value = unlist(json$analyses[[i]]$sampleSettings)
          )
        )
      }
      
      doc <- doc %>% 
        officer::body_add_fpar(
          officer::fpar(
            officer::ftext(paste0("Sampling Settings"), prop = style_table_title)
          )) %>%
        officer::body_add_table(sampleSet, header = TRUE, style = "Table Professional") %>% 
        officer::body_add_par("")
      
    }
    
    if(json$analyses[[i]]$executeSettings$runfeatureEngineering & length(json$analyses[[i]]$featureEngineeringSettings)>0){
      
      if(class(json$analyses[[i]]$sampleSettings)=='featureEngineeringSettings'){
        json$analyses[[i]]$featureEngineeringSettings <- list(json$analyses[[i]]$featureEngineeringSettings)
      }
      
      featSet <- c()
      for(j in 1:length(json$analyses[[i]]$featureEngineeringSettings)){
        featSet <- rbind(
          featSet, 
          data.frame(
            funct = attr(json$analyses[[i]]$featureEngineeringSettings,"fun"),
            setting = names(unlist(json$analyses[[i]]$featureEngineeringSettings)),
            value = unlist(json$analyses[[i]]$featureEngineeringSettings)
          )
        )
      }
      
      doc <- doc %>% 
        officer::body_add_fpar(
          officer::fpar(
            officer::ftext(paste0("Feature engineering Settings"), prop = style_table_title)
          )) %>%
        officer::body_add_table(featSet, header = TRUE, style = "Table Professional") %>% 
        officer::body_add_par("")
      
    }
    
    if(json$analyses[[i]]$executeSettings$runPreprocessData){
      preproSet <- data.frame(
        setting = names(unlist(json$analyses[[i]]$preprocessSettings)),
        value = unlist(json$analyses[[i]]$preprocessSettings)
      )
      
      doc <- doc %>% 
        officer::body_add_fpar(
          officer::fpar(
            officer::ftext(paste0("Preprocess Settings"), prop = style_table_title)
          )) %>%
        officer::body_add_table(preproSet, header = TRUE, style = "Table Professional") %>% 
        officer::body_add_par("")
      
    }
    
  
  }
  
  #```````````````````````````````````````````````````````````````````````````

  
  
  modelEvaluation <- data.frame(rbind(
    c("ROC Plot", "The ROC plot plots the sensitivity against 1-specificity on the test set. The plot shows how well the model is able to discriminate between the people with the outcome and those without. The dashed diagonal line is the performance of a model that randomly assigns predictions. The higher the area under the ROC plot the better the discrimination of the model."),
    c("Calibration Plot", "The calibration plot shows how close the predicted risk is to the observed risk. The diagonal dashed line thus indicates a perfectly calibrated model. The ten (or fewer) dots represent the mean predicted values for each quantile plotted against the observed fraction of people in that quantile who had the outcome (observed fraction). The straight black line is the linear regression using these 10 plotted quantile mean predicted vs observed fraction points. The two blue straight lines represented the 95% lower and upper confidence intervals of the slope of the fitted line."),
    c("Smooth Calibration Plot", "Similar to the traditional calibration shown above the Smooth Calibration plot shows the relationship between predicted and observed risk. the major difference is that the smooth fit allows for a more fine grained examination of this. Whereas the traditional plot will be heavily influenced by the areas with the highest density of data the smooth plot will provide the same information for this region as well as a more accurate interpretation of areas with lower density. the plot also contains information on the distribution of the outcomes relative to predicted risk.  However the increased information game comes at a computational cost. It is recommended to use the traditional plot for examination and then to produce the smooth plot for final versions."),
    c("Prediction Distribution Plots", "The preference distribution plots are the preference score distributions corresponding to i) people in the test set with the outcome (red) and ii) people in the test set without the outcome (blue)."),
    c("Box Plots", "The prediction distribution boxplots are box plots for the predicted risks of the people in the test set with the outcome (class 1: blue) and without the outcome (class 0: red)."),
    c("Test-Train Similarity Plot", "The test-train similarity is presented by plotting the mean covariate values in the train set against those in the test set for people with and without the outcome."),
    c("Variable Scatter Plot", "The variable scatter plot shows the mean covariate value for the people with the outcome against the mean covariate value for the people without the outcome. The size and color of the dots correspond to the importance of the covariates in the trained model (size of beta) and its direction (sign of beta with green meaning positive and red meaning negative), respectively."),
    c("Precision Recall Plot", "The precision-recall curve is valuable for dataset with a high imbalance between the size of the positive and negative class. It shows the tradeoff between precision and recall for different threshold. High precision relates to a low false positive rate, and high recall relates to a low false negative rate. High scores for both show that the classifier is returning accurate results (high precision), as well as returning a majority of all positive results (high recall). A high area under the curve represents both high recall and high precision."),
    c("Demographic Summary Plot", "This plot shows for females and males the expected and observed risk in different age groups together with a confidence area.")
  ))
  names(modelEvaluation) <- c("Evaluation","Description")
  modelEvaluation <- modelEvaluation[order(modelEvaluation$Evaluation),]
  
  doc <- doc %>%
    officer::body_add_par("Statistical Analysis Method(s)", style = "heading 2") %>%
    
    officer::body_add_par("Design", style = "heading 3") %>%
    officer::body_add_par("") %>%
    officer::body_add_par(paste0("To build and internally validate the models, we will partition the labelled data into a train set (",
      (json$splitSettings$train)*100,
      "%) and a test set (",
      json$splitSettings$test*100,
      "%)."), 
      style = "Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_par(paste0("The hyper-parameters for the models will be assessed using ",
      json$splitSettings$nfold,
      "-fold cross validation on the train set and a final model will be trained using the full train set and optimal hyper-parameters."),
      style = "Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("The internal validity of the models will be assessed on the test set.  We will use the area under the receiver operating characteristic curve (AUC) to evaluate the discriminative performance of the models and plot the predicted risk against the observed fraction to visualize the calibration.  See 'Model Evaluation' section for more detailed information about additional model evaluation metrics.") %>%
    officer::body_add_par("") %>%
    
    officer::body_add_par("Model Evaluation", style = "heading 3") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("The following evaluations will be performed on the model:", style = "Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_table(modelEvaluation, header = TRUE, style = "Table Professional") %>%
    officer::body_add_par("") %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Quality Control", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("The PatientLevelPrediction package itself, as well as other OHDSI packages on which PatientLevelPrediction depends, use unit tests for validation.",style="Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext(plpCitation, prop = style_citation)
      )) %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Tools", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("This study will be designed using OHDSI tools and run with R.",style="Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext(rCitation, prop = style_citation)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_par("More information about the tools can be found in the Appendix 'Study Generation Version Information'.", style = "Normal")
  #----------------------------------------------------------------------------- 
  
  #============ DIAGNOSTICS ====================================================
  doc <- doc %>%
    officer::body_add_par("Diagnostics", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Reviewing the incidence rates of the outcomes in the target population prior to performing the analysis will allow us to assess its feasibility.  The full table can be found in the 'Table and Figures' section under 'Incidence Rate of Target & Outcome'.",style="Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Additionally, reviewing the characteristics of the cohorts provides insight into the cohorts being reviewed.  The full table can be found below in the 'Table and Figures' section under 'Characterization'.",style="Normal")
  #----------------------------------------------------------------------------- 
  
  #============ DATA ANALYSIS PLAN =============================================
  
  #```````````````````````````````````````````````````````````````````````````

  #```````````````````````````````````````````````````````````````````````````

  
  #----------------------------------------------------------------------------- 
  
  #============ STRENGTHS & LIMITATIONS ========================================
  doc <- doc %>%
    officer::body_add_par("Strengths & Limitations", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS.", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("Some limitations to consider:", 
                       prop = style_helper_text), 
        officer::ftext("--It may not be possible to develop prediction models for rare outcomes. ", 
                       prop = style_helper_text),
        officer::ftext("--Not all medical events are recorded into the observational datasets and some recordings can be incorrect.  This could potentially lead to outcome misclassification.", 
                       prop = style_helper_text),
        officer::ftext("--The prediction models are only applicable to the population of patients represented by the data used to train the model and may not be generalizable to the wider population. >>", 
                       prop = style_helper_text)
      ))
  
  #----------------------------------------------------------------------------- 
  
  #============ PROTECTION OF HUMAN SUBJECTS ===================================
  doc <- doc %>%
    officer::body_add_par("Protection of Human Subjects", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS.", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("Describe any additional safeguards that are appropriate for the data being used.", 
                       prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("Here is an example statement:", prop = style_helper_text),
        officer::ftext("Confidentiality of patient records will be maintained always. All study reports will contain aggregate data only and will not identify individual patients or physicians. At no time during the study will the sponsor receive patient identifying information except when it is required by regulations in case of reporting adverse events.", prop = style_helper_text),
        officer::ftext(">>", prop = style_helper_text)
      ))
  #----------------------------------------------------------------------------- 
  
  #============ DISSEMINATING & COMMUNICATING ==================================
  doc <- doc %>%
    officer::body_add_par("Plans for Disseminating & Communicating Study Results", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS.", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("List any plans for submission of progress reports, final reports, and publications.", 
                       prop = style_helper_text),
        officer::ftext(">>", 
                       prop = style_helper_text)
      )) %>%
    officer::body_add_break()
  
  #----------------------------------------------------------------------------- 
  
  #============ TABLES & FIGURES ===============================================
  doc <- doc %>%
    officer::body_add_par("Tables & Figures", style = "heading 1") %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Incidence Rate of Target & Outcome", style = "heading 2") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< add incidence here. >>", prop = style_hidden_text)
      )) %>%
    officer::body_add_par("") %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Characterization", style = "heading 2") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< add characterization table here. >>", prop = style_hidden_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< add results here. >>", prop = style_hidden_text)
      )) %>%
    officer::body_add_break() 
  #----------------------------------------------------------------------------- 
  
  #============ APPENDICES =====================================================
  doc <- doc %>%
    officer::body_add_par("Appendices", style = "heading 1") %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Study Generation Version Information", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_par(paste0("Skeleton Version:  ",json$skeletonType," - ", json$skeletonVersion),style="Normal") %>%
    officer::body_add_par(paste0("Identifier / Organization: ",json$organizationName),style="Normal") %>%
    officer::body_add_break() %>%
    officer::body_end_section_continuous() %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Code List", style = "heading 2") %>%
    officer::body_add_par("") 
  
  for(i in 1:length(concepts$uniqueConceptSets)){
    conceptSetId <- paste0("Concept Set #",concepts$uniqueConceptSets[[i]]$conceptId,
                           " - ",concepts$uniqueConceptSets[[i]]$conceptName)
    conceptSetTable <- as.data.frame(concepts$uniqueConceptSets[[i]]$conceptExpressionTable)
    
    id <- as.data.frame(concepts$conceptTableSummary[which(concepts$conceptTableSummary$newConceptId == i),]$cohortDefinitionId)
    names(id) <- c("ID")
    outcomeCohortsForConceptSet <-predictionDf[predictionDf$`Target id` %in% id$ID,c(1,2)]
    targetCohortsForConceptSet <- predictionDf[predictionDf$`outcome Id` %in% id$ID,c(3,4)]
    colnames(outcomeCohortsForConceptSet) <- c('cohort id', 'cohort name')
    colnames(targetCohortsForConceptSet) <- c('cohort id', 'cohort name')
    
    cohortsForConceptSet <- rbind(outcomeCohortsForConceptSet,targetCohortsForConceptSet)
    cohortsForConceptSet <- cohortsForConceptSet[,1:2]
    
    if(ncol(conceptSetTable)>0){
      
      doc <- doc %>% 
        officer::body_add_fpar(
          officer::fpar(
            officer::ftext(conceptSetId, prop = style_table_title)
          )) %>%
        officer::body_add_table(conceptSetTable[,c(1,2,4,6,7,8,9,10,11,12)], header = TRUE, style = "Table Professional") %>% 
        officer::body_add_par("") %>%
        officer::body_add_par("Cohorts that use this Concept Set:", style = "Normal") %>%
        officer::body_add_par("") %>%
        officer::body_add_table(cohortsForConceptSet, header = TRUE, style = "Table Professional") %>%
        officer::body_add_par("")
    }
    
  }
  
  #```````````````````````````````````````````````````````````````````````````
  doc <- doc %>%   
    officer::body_add_break() %>%
    officer::body_end_section_landscape() %>%
    officer::body_add_par("Complete Analysis List", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Below is a complete list of analysis that will be performed.  Definitions for the column 'Covariate Settings ID' can be found above in the 'Covariate Settings' section.  Definitions for the 'Population Settings Id' can be found above in the 'Additional Population Settings' section.",style="Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_table(predictionDf, header = TRUE, style = "Table Professional") %>% 
    officer::body_add_break() 
  
  
  doc <- doc %>% officer::body_add_fpar(
    officer::fpar(
      officer::ftext("<< add models here >>", prop = style_hidden_text)
    )) %>% officer::body_add_par("")
  #-----------------------------------------------------------------------------
  
  #============ REFERNCES ======================================================
  doc <- doc %>%
    officer::body_add_par("References", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS. >>", prop = style_helper_text)
      ))
  #----------------------------------------------------------------------------- 
  
  if(!dir.exists(outputLocation)){
    dir.create(outputLocation, recursive = T)
  }
  print(doc, target = file.path(outputLocation,'protocol.docx'))
}




getTar <- function(populationSettings){
  
  text <- paste0(
    '(',
    populationSettings$riskWindowStart, 
    ' day + ',
    populationSettings$startAnchor, 
    ') - (',
    populationSettings$riskWindowEnd, 
    ' days + ',
    populationSettings$endAnchor, 
    ')')
  
  return(text)
}
