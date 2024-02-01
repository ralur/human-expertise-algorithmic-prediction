
compute_mcc <- function(data, indices) {
  X <- data$X[indices]
  Y <- data$Y[indices]
  
  mcc(X, Y)
}

bootstrap_mcc <- function(y, y.hat, bootstrap.replicates, conf.level) {
  if (sd(y.hat) == 0 || sd(y) == 0) { # if either vector is constant (e.g., all zeroes), the MCC is zero
    return(list(coef = 0, ci.lower = 0, ci.upper = 0))
  }
  data <- data.frame(X = y, Y = y.hat)
  point_est <- compute_mcc(data, 1:nrow(data))
  bootstrap <- boot(data, compute_mcc, R = bootstrap.replicates)
  ci <- boot.ci(bootstrap, conf = conf.level, type = "bca")$bca[4:5]
  
  list(coef = point_est, ci.lower = ci[1], ci.upper = ci[2])
}



process_file.etr <- function(file) {
  data <- read.csv(file)
  
  model_name <- gsub(pattern = "machine_predictions_", replacement = "", x = basename(file))
  model_name <- gsub("\\[|\\]|\\.csv$", "", x = model_name)
  
  colnames(data)[colnames(data) == 'pred_esc'] <- model_name
  return(data[, c('file_id', model_name)])
}

compute_stats.etr <- function(data, boostrap.replicates, conf.level = .95) {
  # for each model, bootstrap_mcc between that model's predictions and the outcome y
  model.stats.1 <- bootstrap_mcc(data$y, data$gradient_boosting, bootstrap.replicates, conf.level)
  model.stats.2 <- bootstrap_mcc(data$y, data$linear_svm, bootstrap.replicates, conf.level)
  model.stats.3 <- bootstrap_mcc(data$y, data$random_forest, bootstrap.replicates, conf.level)
  model.stats.4 <- bootstrap_mcc(data$y, data$logistic_regression, bootstrap.replicates, conf.level)
  model.stats.5 <- bootstrap_mcc(data$y, data$`naive bayes`, bootstrap.replicates, conf.level)
  human.stats.c <- bootstrap_mcc(data$y, data$human.pred.c, bootstrap.replicates, conf.level)
  human.stats.4 <- bootstrap_mcc(data$y, data$human.pred.t4, bootstrap.replicates, conf.level)
  human.stats.8 <- bootstrap_mcc(data$y, data$human.pred.t8, bootstrap.replicates, conf.level)
  human.stats.12 <- bootstrap_mcc(data$y, data$human.pred.t12, bootstrap.replicates, conf.level)
  
  
  combined_stats <- data.frame(
    gradient_boosting_coef = model.stats.1$coef,
    gradient_boosting_ci_lower = model.stats.1$ci.lower,
    gradient_boosting_ci_upper = model.stats.1$ci.upper,
    
    linear_svm_coef = model.stats.2$coef,
    linear_svm_ci_lower = model.stats.2$ci.lower,
    linear_svm_ci_upper = model.stats.2$ci.upper,
    
    random_forest_coef = model.stats.3$coef,
    random_forest_ci_lower = model.stats.3$ci.lower,
    random_forest_ci_upper = model.stats.3$ci.upper,
    
    logistic_regression_coef = model.stats.4$coef,
    logistic_regression_ci_lower = model.stats.4$ci.lower,
    logistic_regression_ci_upper = model.stats.4$ci.upper,
    
    naive_bayes_coef = model.stats.5$coef,
    naive_bayes_ci_lower = model.stats.5$ci.lower,
    naive_bayes_ci_upper = model.stats.5$ci.upper,
    
    human_pred.c_coef = human.stats.c$coef,
    human_pred.c_ci_lower = human.stats.c$ci.lower,
    human_pred.c_ci_upper = human.stats.c$ci.upper,
    
    human_pred.4_coef = human.stats.4$coef,
    human_pred.4_ci_lower = human.stats.4$ci.lower,
    human_pred.4_ci_upper = human.stats.4$ci.upper,
    
    human_pred.8_coef = human.stats.8$coef,
    human_pred.8_ci_lower = human.stats.8$ci.lower,
    human_pred.8_ci_upper = human.stats.8$ci.upper,
    
    human_pred.12_coef = human.stats.12$coef,
    human_pred.12_ci_lower = human.stats.12$ci.lower,
    human_pred.12_ci_upper = human.stats.12$ci.upper
    
  )
  
  return(combined_stats)
}

compute_stats.etr.mc <- function(data, bootstrap.replicates, conf.level = .95) {
  # for each model, bootstrap_mcc between that model's predictions and the outcome y
  model.stats.1 <- bootstrap_mcc(data$y, data$mc_pred, bootstrap.replicates, conf.level)
  human.stats.c <- bootstrap_mcc(data$y, data$human.pred.c, bootstrap.replicates, conf.level)
  human.stats.4 <- bootstrap_mcc(data$y, data$human.pred.t4, bootstrap.replicates, conf.level)
  human.stats.8 <- bootstrap_mcc(data$y, data$human.pred.t8, bootstrap.replicates, conf.level)
  human.stats.12 <- bootstrap_mcc(data$y, data$human.pred.t12, bootstrap.replicates, conf.level)
  human.stats.baseline <- bootstrap_mcc(data$y, data$human.pred.baseline, bootstrap.replicates, conf.level)
  
  
  combined_stats <- data.frame(
    mc_coef = model.stats.1$coef,
    mc_ci_lower = model.stats.1$ci.lower,
    mc_ci_upper = model.stats.1$ci.upper,
    
    human_pred.c_coef = human.stats.c$coef,
    human_pred.c_ci_lower = human.stats.c$ci.lower,
    human_pred.c_ci_upper = human.stats.c$ci.upper,
    
    human_pred.4_coef = human.stats.4$coef,
    human_pred.4_ci_lower = human.stats.4$ci.lower,
    human_pred.4_ci_upper = human.stats.4$ci.upper,
    
    human_pred.8_coef = human.stats.8$coef,
    human_pred.8_ci_lower = human.stats.8$ci.lower,
    human_pred.8_ci_upper = human.stats.8$ci.upper,
    
    human_pred.12_coef = human.stats.12$coef,
    human_pred.12_ci_lower = human.stats.12$ci.lower,
    human_pred.12_ci_upper = human.stats.12$ci.upper,
    
    human_pred.baseline_coef = human.stats.baseline$coef,
    human_pred.baseline_ci_lower = human.stats.baseline$ci.lower,
    human_pred.baseline_ci_upper = human.stats.baseline$ci.upper
    
  )
  
  return(combined_stats)
}

compute_stats.cols <- function(data, colnames, bootstrap.replicates, conf.level = .95) {
  
  stats <- lapply(colnames, function(colname) {
    s <- bootstrap_mcc(data$y, data %>% pull(!!colname), bootstrap.replicates, conf.level)
    data.frame(coef = s$coef, ci.lower = s$ci.lower, ci.upper = s$ci.upper, type = colname)
  }) %>% rbindlist
  
  return(stats)
}


