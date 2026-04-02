# Project: Medical Cost Benchmarking
# Script:  Helper functions
# Author:  Amogelang Ramatlo
# Date:    2025-05-30


# --------------------------------------------------
# Recode function, for ease of formatting 
# --------------------------------------------------

recode_variable <- function(x, type = c("all", "categorical", "numeric")) {
  type <- match.arg(type)
  
  labels <- switch(
    type,
    categorical = labels_categorical,
    numeric     = labels_numeric,
    all         = c(labels_categorical, labels_numeric)
  )
  
  recode(x, !!!labels, .default = x)
}

# --------------------------------------------------
# Compute performance metrics
# --------------------------------------------------

evaluate_metrics <- function(probs, actuals, model_name) {
  # Ensure actuals are factors with correct levels
  actuals <- factor(actuals, levels = c("Low", "High"))
  
  # Convert predicted probabilities to class labels ("high" or "low")
  preds <- factor(ifelse(probs > 0.5, "High", "Low"), levels = c("Low", "High"))
  
  # Confusion matrix using caret
  cm <- caret::confusionMatrix(preds, actuals, positive = "High")
  
  # Extract metrics from confusion matrix
  accuracy <- cm$overall[["Accuracy"]]
  precision <- cm$byClass[["Pos Pred Value"]]
  recall <- cm$byClass[["Sensitivity"]]
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # AUROC - probs is for the positive class
  auc_roc <- auc(roc(response = actuals, predictor = probs))
  
  # AUPRC - probs for class 1 (positive class)
  pr <- pr.curve(
    scores.class0 = probs[actuals == "Low"],   # probs for class 0 (low)
    scores.class1 = probs[actuals == "High"]   # probs for class 1 (high)
  )
  auc_prc <- pr$auc.integral
  
  # Return as single-row data.frame
  temp <- data.frame(
    Model = model_name,
    Accuracy = round(accuracy, 4),
    Precision = round(precision, 4),
    Recall = round(recall, 4),
    F1_Score = round(f1_score, 4),
    AU_ROC = round(auc_roc, 4),
    AU_PRC = round(auc_prc, 4),
    stringsAsFactors = FALSE
  )
  return(temp)
}