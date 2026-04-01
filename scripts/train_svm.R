# Project: Medical Cost Benchmarking
# Script:  RBF-SVM Hyperparameter Optimization
# Author:  Amogelang Ramatlo
# Date:    2025-05-30

# --- Load Dependencies ---
library(caret)      # Main training framework
library(kernlab)    # The RBF-SVM mathematical engine
library(parallel)   # Hardware detection
library(doParallel) # Parallel backend
library(here)       # Project-root path management

# --- Data Ingestion ---
train_data <- readRDS(here("data", "processed_train.rds"))

# --- Setup Parallel Backend ---
cores <- detectCores() - 2 
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

# --- Training of the data ---
# Define Training Parameters
train_control <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

svm_grid <- expand.grid(
  sigma = seq(0.01, 1, 0.01), 
  C = c(0.1, 1, 10, 50, 100)
)

# Execute Training
set.seed(123)
svm_tuned <- caret::train(
  charges ~ ., 
  data = train_data, 
  method = "svmRadial",
  metric = "ROC", 
  preProcess = c("center", "scale"),
  tuneGrid = svm_grid,
  trControl = train_control
)

# --- Cleanup ---
stopCluster(cl)
registerDoSEQ()

# --- Export Results ---
if (!dir.exists("models")) { 
  dir.create("models") 
}

saveRDS(svm_tuned, "models/svm_tuned_final.rds")

message("SVM Training Complete. Model saved to models/svm_tuned_final.rds")