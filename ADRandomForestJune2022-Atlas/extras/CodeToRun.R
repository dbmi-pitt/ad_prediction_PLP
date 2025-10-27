
library(ADRandomForestJune2022)
library(PatientLevelPrediction)
library(FeatureExtraction)

# USER INPUTS
#=======================
# The folder where the study intermediate and result files will be written:
outputFolder <- "<FIX PATH>/ADRandomForestJune2022/AD-out"

# Specify where the temporary files (used by the ff package) will be created:
options(fftempdir = "<FIX PATH>/FFTemp")

# Details for connecting to the server:
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms="", 
  server="", 
  user="", 
  password="", 
  port="")


# Add the database containing the OMOP CDM data
cdmDatabaseSchema <- "alz_case_controls_pool_omop"
# The name of the database schema and table where the study-specific cohorts will be instantiated:
cohortDatabaseSchema <- "plp_ohdsi"
cohortTable <- "plpcohort"
# Some meta-information that will be used by the export function:
databaseId <- "alz_ehr_etl_2022"
databaseName <- "Momentum AD database"
databaseDescription <- "Data from persons with both an AD diagnoses after 2016 and 10 years of health records data matched to up to 4 controls"

oracleTempSchema <- NULL
#=======================

execute(connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cdmDatabaseName = databaseName,
        cohortDatabaseSchema = cohortDatabaseSchema,
        oracleTempSchema = oracleTempSchema,
        cohortTable = cohortTable,
        outputFolder = outputFolder,
        createProtocol = F,
        createCohorts = T,
        runAnalyses = T,
        createResultsDoc = F,
        packageResults = F,
        createValidationPackage = F,  
        #analysesToValidate = 1,
        minCellCount= 5,
        createShiny = T,
        createJournalDocument = F,
        analysisIdDocument = 1)

# Uncomment and run the next line to see the shiny results:
PatientLevelPrediction::viewMultiplePlp(outputFolder)

#saving detailed results to CSV files (change file path for the analysis required)
#Analysis_1: RF
analysisResults = readRDS(file = "<FIX PATH>/ADRandomForestJune2022/AD-out/Analysis_1/plpResult/performanceEvaluation.rds")
evali = analysisResults$evaluationStatistics
thresh = analysisResults$thresholdSummary
demog = analysisResults$demographicSummary
pred = analysisResults$predictionDistribution
calib = analysisResults$calibrationSummary
#save to csv
file_eval = "<FIX PATH>/ADRandomForestJune2022/AD-out/model_analysis/RF_evaluationStatistics.csv"
file_thresh = "<FIX PATH>/ADRandomForestJune2022/AD-out/model_analysis/RF_thresholdSummary.csv"
file_demo = "<FIX PATH>/ADRandomForestJune2022/AD-out/model_analysis/RF_demographicSummary.csv"
file_pred = "<FIX PATH>/ADRandomForestJune2022/AD-out/model_analysis/RF_predictionDistribution.csv"
file_calib = "<FIX PATH>/ADRandomForestJune2022/AD-out/model_analysis/RF_calibrationSummary.csv"

write.csv(evali, file=file_eval)
write.csv(thresh, file=file_thresh)
write.csv(demog, file=file_demo)
write.csv(pred, file=file_pred)
write.csv(calib, file=file_calib)

modelResults = readRDS(file = "<FIX PATH>/ADRandomForestJune2022/AD-out/Analysis_1/plpResult/covariateSummary.rds")
cov_file = "<FIX PATH>/ADRandomForestJune2022/AD-out/model_analysis/RF_covariates.csv"
write.csv(modelResults, file=cov_file)

#calculate permutation importance of the features
#load plp data, population and plp results from file
plpData = loadPlpData("<FIX PATH>/ADRandomForestJune2022/AD-out/PlpData_L1_T21")
population = createStudyPopulation(plpData = plpData, 
                                   outcomeId = 33, 
                                   includeAllOutcomes = TRUE,
                                   firstExposureOnly = TRUE,
                                   requireTimeAtRisk = TRUE,
                                   minTimeAtRisk = 0,
                                   riskWindowStart = 0,
                                   riskWindowEnd = 2190,
                                   removeSubjectsWithPriorOutcome = TRUE)
plpResult = loadPlpResult("<FIX PATH>/ADRandomForestJune2022/AD-out/Analysis_1/plpResult")
test_pfi = pfi(plpResult = plpResult, 
               population = population,
               plpData = plpData,
               repeats = 5,
               covariates = NULL,
               cores = 6,
               log = '<FIX PATH>/ADRandomForestJune2022/AD-out/pfi_log.txt',
               logthreshold = 'TRACE')

View(merge(test_pfi, plpResult$covariateSummary %>% select(covariateId, covariateName, covariateValue)
           %>% collect(), by = 'covariateId'))
pfi_data = merge(test_pfi, plpResult$covariateSummary %>% select(covariateId, covariateName, covariateValue)
                 %>% collect(), by = 'covariateId')
write.csv(pfi_data, file="<FIX PATH>/ADRandomForestJune2022/AD-out/model_analysis/RF_permutationImportance.csv")

#get prediction probabilities for all patients in cohort 
cohortData = readRDS(file="<FIX PATH>/ADRandomForestJune2022/AD-out/PlpData_L1_T21/cohorts.rds")
outcomesData = readRDS(file="<FIX PATH>/ADRandomForestJune2022/AD-out/PlpData_L1_T21/outcomes.rds")
#get csv files from the rds data files in plpdata
write.csv(cohortData, file="<FIX PATH>/ADRandomForestJune2022/AD-out/PlpData_L1_T21/cohort.csv")
write.csv(outcomesData, file="<FIX PATH>/ADRandomForestJune2022/AD-out/PlpData_L1_T21/outcomes.csv")

modelPath = "<FIX PATH>/ADRandomForestJune2022/AD-out/Analysis_1/plpResult/model/"
plpModel = loadPlpModel(modelPath)

plpres1 = predictPlp(plpModel = plpModel, population = population, plpData = plpData)
write.csv(plpres1, file="<FIX PATH>/ADRandomForestJune2022/AD-out/model_analysis/RF_predictionProbability.csv")

#get test data
testData = plpResult$prediction$subjectId[plpResult$prediction$indexes<0]
write.csv(testData, file="<FIX PATH>/ADRandomForestJune2022/AD-out/model_analysis/test_subjects.csv")

##table 1
covariateSettingsTable1 = createTable1CovariateSettings()
#change cohortId as per cohort number from Atlas
covariateDataOutcome = getDbCovariateData(connectionDetails = connectionDetails,
                                   cdmDatabaseSchema = cdmDatabaseSchema,
                                   cohortDatabaseSchema = cohortDatabaseSchema,
                                   cohortTable = cohortTable,
                                   cohortId = 33,
                                   rowIdField = "subject_id",
                                   covariateSettings = covariateSettingsTable1,
                                   aggregated = TRUE)
summary(covariateDataOutcome)

result <- createTable1(covariateDataOutcome)
print(result, row.names = FALSE, right = FALSE)
write.csv(result, file='<FIX PATH>/ADRandomForestJune2022/AD-out/model_analysis/Table1-outcome.csv')

##all patients in cohort
covariateDataCohort = getDbCovariateData(connectionDetails = connectionDetails,
                                          cdmDatabaseSchema = cdmDatabaseSchema,
                                          cohortDatabaseSchema = cohortDatabaseSchema,
                                          cohortTable = cohortTable,
                                          cohortId = 21,
                                          rowIdField = "subject_id",
                                          covariateSettings = covariateSettingsTable1,
                                          aggregated = TRUE)
summary(covariateDataCohort)

result3 <- createTable1(covariateDataCohort)
write.csv(result3, file='<FIX PATH>/ADRandomForestJune2022/AD-out/model_analysis/Table1-all-cohort.csv')

##get table 1 summaries for train and test data
plpData = loadPlpData("<FIX PATH>/ADRandomForestJune2022/AD-out/PlpData_L1_T21")
population <- createStudyPopulation(plpData, outcomeId = 33)
plpResult = loadPlpResult("<FIX PATH>/ADRandomForestJune2022/AD-out/Analysis_1/plpResult")

# finding the people used in the test set and creating a new population restricted to these (need to add the metadata as this is needed but seems to get removed when restricting the data.frame)
testPlp <- plpResult$prediction$rowId[plpResult$prediction$indexes < 0]
trainPlp = plpResult$prediction$rowId[plpResult$prediction$indexes > 0]
md <- attr(population, 'metaData')
populationTest <- population[population$rowId%in%testPlp,]
populationTrain = population[population$rowId%in%trainPlp,]
attr(populationTest, 'metaData') <- md
attr(populationTrain, 'metaData') <- md

#get table1 and create features for train and test populations
#how


