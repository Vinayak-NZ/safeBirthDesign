
evaluate_and_save_model <- function(model_input, 
                                    model_name, 
                                    rope_lower = -0.10, 
                                    rope_upper = 0.10) {
  
  draws <- as_draws_matrix(model_input)
  all_parameters <- colnames(draws)

  r2_draws  <- bayes_R2(model_input, summary = FALSE)
  r2_median <- median(r2_draws)
  r2_lower  <- quantile(r2_draws, 0.025)
  r2_upper  <- quantile(r2_draws, 0.975)
  
  param_results <- list()
  
  for (param in all_parameters) {
    beta <- draws[, param]
    
    pd <- max(mean(beta > 0), mean(beta < 0))
    
    p_rope <- mean(beta >= rope_lower & beta <= rope_upper)

    beta_median <- median(beta)
    beta_lower  <- quantile(beta, 0.025)
    beta_upper  <- quantile(beta, 0.975)
    
    param_results[[param]] <- data.frame(
      Model     = model_name,
      Parameter = param,
      Median    = round(beta_median, 3),
      CI_Lower  = round(beta_lower, 3),
      CI_Upper  = round(beta_upper, 3),
      PD        = round(pd, 3),
      P_ROPE    = round(p_rope, 3),
      stringsAsFactors = FALSE
    )
  }
  
  parameter_summary <- do.call(rbind, param_results)
  
  csv_filename <- paste0("output/metrics_", model_name, ".csv")
  write.csv(parameter_summary, file = csv_filename, row.names = FALSE)
  
  txt_filename <- paste0("output/report_", model_name, ".txt")
  
  sink(txt_filename)
  
  cat("=========================================================\n")
  cat("MODEL:", model_name, "\n")
  cat("=========================================================\n")
  cat("Global Bayesian R² Median:", round(r2_median, 3), "\n")
  cat("Global Bayesian R² 95% CrI: [", round(r2_lower, 3), ",", 
      round(r2_upper, 3), "]\n\n")
  
  cat("=========================================================\n")
  cat("PARAMETER-LEVEL METRICS\n")
  cat("=========================================================\n")
  print(parameter_summary, row.names = FALSE)
  
  sink() 
  
  cat("Successfully saved files for:", model_name, "\n")
  cat(" -> CSV Table:", csv_filename, "\n")
  cat(" -> TXT Report:", txt_filename, "\n\n")
}

