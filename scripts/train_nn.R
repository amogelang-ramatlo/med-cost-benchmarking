# Project: Medical Cost Benchmarking
# Script:  Shallow NN Hyperparameter Optimization
# Author:  Amogelang Ramatlo
# Date:    2025-05-30

# --- Load Dependencies ---
library(caret)      # Main training framework
library(parallel)   # Hardware detection
library(doParallel) # Parallel backend
library(here)       # Project-root path management

# --- Data Ingestion ---
train_data <- readRDS(here("data", "train_data_processed.rds"))

# --- Setup Parallel Backend ---
cores <- detectCores() - 2 
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

# --- Training of the data ---
# Define Training Control
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary, 
  search = "grid",
  verboseIter = TRUE
)

# Define the Tuning Grid
# Size: Number of neurons in the hidden layer
# Decay: Weight decay to prevent overfitting
nn_grid <- expand.grid(
  size = seq(from = 1, to = 15, by = 2),
  decay = c(0, 0.001, 0.01, 0.1, 0.5)
)

# Train the Model
set.seed(2025)
nn_tuned <- caret::train(
  charges ~ ., 
  data = train_data, 
  method = "nnet",
  metric = "ROC",
  trControl = fitControl,
  tuneGrid = nn_grid,
  preProcess = c("center", "scale"), # Standardizing is vital for NNs
  trace = FALSE,                     
  maxit = 500,                       # Maximum iterations for convergence
  MaxNWts = 5000                     # Increases limit for larger networks
)

# --- Cleanup ---
stopCluster(cl)
registerDoSEQ()

# --- Export Results ---
if (!dir.exists("models")) { 
  dir.create("models") 
}

# Save for Report
saveRDS(nn_tuned, "models/nn_tuned.rds")

message("NN Training Complete. Model saved to models/nn_tuned.rds")