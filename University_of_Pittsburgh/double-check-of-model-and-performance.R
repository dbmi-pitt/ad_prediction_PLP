## Cross-walking and checking model performance and supporting data prior to manuscript submission

modelSettings <- readRDS("/home/rdb20/DI_DIR/ad_prediction_PLP/University_of_Pittsburgh/plpResult/model/modelSettings.rds")
modelSettings
#$model
#[1] "randomForest_python"
#
#$modelParameters
#ntrees mtries maxDepth varImp     seed
#80   1000    200       25   TRUE 27252672

covariateSummary <- readRDS("/home/rdb20/DI_DIR/ad_prediction_PLP/University_of_Pittsburgh/plpResult/covariateSummary.rds")
dim(covariateSummary)
# [1] 20931 - matches what is reported
View(covariateSummary[covariateSummary$included==1,])
## 2499 included covariates - matches what is reported


performanceEvaluation <- readRDS("/home/rdb20/DI_DIR/ad_prediction_PLP/University_of_Pittsburgh/plpResult/performanceEvaluation.rds")
performanceEvaluation$evaluationStatistics
# performanceEvaluation$evaluationStatistics
# analysisId   Eval    Metric                 Value                  
# populationSize       "Analysis_1" "train" "populationSize"       "98547"                
# outcomeCount         "Analysis_1" "train" "outcomeCount"         "14605"                
# *** TRAIN *** # AUC.auc              "Analysis_1" "train" "AUC.auc"              "0.846228432160066"    
# AUC.auc_lb95ci       "Analysis_1" "train" "AUC.auc_lb95ci"       NA                     
# AUC.auc_ub95ci       "Analysis_1" "train" "AUC.auc_ub95ci"       NA                     
# *** TRAIN *** # AUPRC                "Analysis_1" "train" "AUPRC"                "0.677341278072639"    
# BrierScore           "Analysis_1" "train" "BrierScore"           "0.0813599092429386"   
# BrierScaled          "Analysis_1" "train" "BrierScaled"          "0.355509682040184"    
# CalibrationIntercept "Analysis_1" "train" "CalibrationIntercept" "8.24721197053568e-13" 
# CalibrationSlope     "Analysis_1" "train" "CalibrationSlope"     "1.13617326478246"     
# CalibrationInLarge   "Analysis_1" "train" "CalibrationInLarge"   "1"                    
# Emean.Eavg           "Analysis_1" "train" "Emean.Eavg"           "0.0363957071076521"   
# E90.E90              "Analysis_1" "train" "E90.E90"              "0.0732868471945172"   
# Emax.Emax            "Analysis_1" "train" "Emax.Emax"            "0.307458866595276"    
# populationSize       "Analysis_1" "test"  "populationSize"       "32848"                
# outcomeCount         "Analysis_1" "test"  "outcomeCount"         "4868"                 
#***** # AUC.auc              "Analysis_1" "test"  "AUC.auc"              "0.803728757276458"    
# AUC.auc_lb95ci       "Analysis_1" "test"  "AUC.auc_lb95ci"       NA                     
# AUC.auc_ub95ci       "Analysis_1" "test"  "AUC.auc_ub95ci"       NA                     
#***** # AUPRC                "Analysis_1" "test"  "AUPRC"                "0.546026415577612"    
# BrierScore           "Analysis_1" "test"  "BrierScore"           "0.094571027230188"    
# BrierScaled          "Analysis_1" "test"  "BrierScaled"          "0.250946967967184"    
# CalibrationIntercept "Analysis_1" "test"  "CalibrationIntercept" "-0.000269843208100155"
# CalibrationSlope     "Analysis_1" "test"  "CalibrationSlope"     "1.05707484384934"     
# CalibrationInLarge   "Analysis_1" "test"  "CalibrationInLarge"   "1.00018155942542"     
# Emean.Eavg           "Analysis_1" "test"  "Emean.Eavg"           "0.0173968855880072"   
# E90.E90              "Analysis_1" "test"  "E90.E90"              "0.0337898096109562"   
# Emax.Emax            "Analysis_1" "test"  "Emax.Emax"            "0.072417397711278"  



## Thresholds for the test run
write.csv(performanceEvaluation$thresholdSummary[performanceEvaluation$thresholdSummary$Eval=='test',],  file = "/home/rdb20/OneDrive/JournalSubmissions/TBD-AD-prediction-feature-analysis/threshold-summary-test-run.csv", row.names = FALSE)
df = performanceEvaluation$thresholdSummary[performanceEvaluation$thresholdSummary$Eval=='test',] # the dataframe to use moving forward

# Load required libraries
library(ggplot2)
library(dplyr)
library(pROC)
library(PRROC)

# Compute metrics for ROC and PR curves
df <- df %>%
  mutate(
    TPR = truePositiveCount / (truePositiveCount + falseNegativeCount),  # Sensitivity
    FPR = falsePositiveCount / (falsePositiveCount + trueNegativeCount),  # 1 - Specificity
    Precision = truePositiveCount / (truePositiveCount + falsePositiveCount),
    Recall = TPR
  )

# --- ROC curve and AUC ---
roc_auc <- auc(df$FPR, df$TPR)
cat("AUROC =", roc_auc, "\n")

ggplot(df, aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", size = 1) +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(
    title = paste0("ROC Curve (AUC = ", round(roc_auc, 3), ")"),
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme_minimal()

# --- Precision-Recall curve and AUC ---
# Remove NA/Inf before computing PR AUC
df_pr <- df %>% filter(!is.na(Precision) & !is.infinite(Precision))
pr_auc <- auc(df_pr$Recall, df_pr$Precision)
cat("AUPRC =", pr_auc, "\n")

ggplot(df_pr, aes(x = Recall, y = Precision)) +
  geom_line(color = "red", size = 1) +
  labs(
    title = paste0("Precision-Recall Curve (AUPRC = ", round(pr_auc, 3), ")"),
    x = "Recall (Sensitivity)",
    y = "Precision (PPV)"
  ) +
  theme_minimal()

