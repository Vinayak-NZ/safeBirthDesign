## ---- remove-redundancy
remove_redundancy <- function(data, output){
  
  t2_vars <- grep("t2", names(data), value=TRUE)
  t3_vars <- grep("t3", names(data), value=TRUE)
  
  output <- data[,!(names(data) %in% c(t2_vars, t3_vars))]
  
  return(output)
  
}

## ---- rename-hapa
hapa_rename <- function(data){
  
  names(data) <- gsub("riskper", "hapa1", names(data))
  names(data) <- gsub("oe", "hapa2", names(data))
  names(data) <- gsub("cse", "hapa3", names(data))
  names(data) <- gsub("int", "hapa4", names(data))
  names(data) <- gsub("pl", "hapa5", names(data))
  
  return(data)
  
}

## ---- rename-comm
comm_rename <- function(data){
  
  names(data) <- gsub("comm1", "comm1", names(data))
  names(data) <- gsub("comm2", "comm2", names(data))
  names(data) <- gsub("comm3", "comm3", names(data))
  names(data) <- gsub("comm4", "comm4", names(data))
  names(data) <- gsub("comm5", "comm5", names(data))
  names(data) <- gsub("comm6", "comm6", names(data))
  names(data) <- gsub("comm7", "comm7", names(data))
  
  return(data)
  
}

## ---- rename-safety
safety_rename <- function(data){
  
  names(data) <- gsub("vueic1", "safe1", names(data))
  names(data) <- gsub("vueic2", "safe2", names(data))
  
  return(data)
  
}

## ---- rename-feedback
feedback_rename <- function(data){
  
  names(data) <- gsub("l10nuterfreundlichkeit_t1_a1", "ux", names(data))
  names(data) <- gsub("l10inhalt_t1_a1", "content", names(data))
  names(data) <- gsub("l10nutzen_t1_a1", "utility", names(data))
  
  return(data)
  
}

## ---- modify-repeated-vars
tidy_rep_var <- function(var, data, default_n = 5){
  
  var_relabel <- substr(var, 1, default_n)
  
  t <- substr(var, 8, 8)
  
  var_list <- grep(var, names(data), value=TRUE)
  
  if(length(grep("_a2", var_list)) > 0) {
    
    data[[paste0(var_relabel, "_t", t)]] <- ifelse(!is.na(data[[paste0(var, "_t1", "_a2")]]), 
                                                   data[[paste0(var, "_t1", "_a2")]], 
                                                   data[[paste0(var, "_t1", "_a1")]])
    
  } else {
    
    data[[paste0(var_relabel, "_t", t)]] <- data[[paste0(var, "_t1","_a1")]]
    
  }
  
  data[[paste0(var_relabel, "_t", t)]] <- as.numeric(data[[paste0(var_relabel, "_t", t)]])
  
  data <- data[, c("id", paste0(var_relabel, "_t", t))]
  
  return(data)  
  
}

## ---- modify-construct
tidy_con <- function(con, data, time){
  
  time_points <- paste0("_v", 1:time)
  
  con_list <- paste0(con, time_points)
  
  modified_con <- lapply(con_list, tidy_rep_var, data = data)
  
  data_output <- Reduce(function(x, y) merge(x, y, by = "id", ), modified_con) 
  
  return(data_output)
  
}

## ---- edit-scores
score_edit <- function(var, data){
  
  data[[paste0(var)]] <- ifelse(data[[paste0(var)]] < 0, 0, data[[paste0(var)]])
  
  data <- data[, c("id", paste0(var))]
  
  return(data)
  
}

## ---- edit-multiple-scores
score_edit_multiple <- function(con, data, time){
  
  time_points <- paste0("_t", 1:time)
  
  con_list <- paste0(con, time_points)
  
  modified_con <- lapply(con_list, score_edit, data = data)
  
  data_output <- Reduce(function(x, y) merge(x, y, by = "id"), modified_con) 
  
  return(data_output)
  
}

## ---- user-feedback-var
user_feedback_var <- function(var, label, mu_output, data){
  
  version_one <- data[data$version == "Version 1", ]
  
  version_one_median <- median(data[[var]], na.rm = TRUE)
  
  version_two <- data_feedback[data$version == "Version 2", ]
  
  version_two_median <- median(version_two[[var]], na.rm = TRUE)
  
  sig_mark <- ifelse(signif(mu_output$p.value[[1]], 2) < 0.001, 
                     "**", 
                     ifelse(signif(mu_output$p.value[[1]], 2) < 0.05, 
                            "*", ""))
  
  output_table <- data.frame(
    Variable = c(label), 
    Version_one = version_one_median, 
    Version_two = version_two_median, 
    U = paste0(mu_output$statistic[[1]], sig_mark)
  )
  
  return(output_table)
  
}

## ---- partial-eta-squared
eta_squared <- function(model) {
  
  SSeffect <- summary(aov(model))[[1]]["Sum Sq"]
  
  SSerror <- sum(SSeffect) - SSeffect
  
  eta_sq <- SSeffect / sum(SSeffect, SSerror)
  
  return(eta_sq)
}

## ---- cramers-v-function
cramers_V <- function(chi, n, df){
  
  output <- sqrt((chi) / (n * df))
  
  return(output)
  
}

## ---- transform-to-post
tx_post <- function(var, data){
  
  if (var %in% c("safe1", "safe2")) {
    
    data[[paste0(var, "_post")]] <- 
      ifelse(data[["group"]] == 0, data[[paste0(var, "_t1")]], 
             data[[paste0(var, "_t2")]])
    
  } else{
    
    data[[paste0(var, "_post")]] <- 
      ifelse(data[["group"]] == 0, data[[paste0(var, "_t1")]], 
             data[[paste0(var, "_t4")]])
    
  }
  
  return(data)
  
}

## ---- default-model
default_model <- function(data, outcome_prefix){
  
  outcome_var <- paste0(outcome_prefix, "_scaled")
  
  predictors <- c("group*time", "age_scaled", "education", "fam_comp")
  
  model_formula <- reformulate(termlabels = predictors, response = outcome_var)
  
  lm(model_formula, data = data)
  
}

## ---- extract-variance-estimates
model_variance_estimates <- function(fit) {
  
  sqrt(diag(vcov(fit)))
  
}

## ---- FMI-Rubin-Rule
fmi_rubin_rule <- function(data, outcome_var){
  
  fit_each <- lapply(data, default_model, outcome_prefix = outcome_var)
  
  estimates_matrix <- sapply(fit_each, coef)
  
  se_matrix <- sapply(fit_each, model_variance_estimates)
  
  M <- ncol(estimates_matrix)
  
  Q_bar <- rowMeans(estimates_matrix)
  U_bar <- rowMeans(se_matrix^2)
  B <- apply(estimates_matrix, 1, var)
  
  T      <- U_bar + (1 + 1/M) * B
  FMI    <- ((1 + 1/M) * B) / T
  RIV    <- ((1 + 1/M) * B) / U_bar
  lambda <- ((1 + 1/M) * B) / T
  
  fmi_output <- data.frame(
    FMI = FMI,
    RIV = RIV,
    lambda = lambda
  )
  
  return(fmi_output)
  
}

## ---- create-brms-formula
make_brms_formula <- function(outcome_prefix) {
  
  outcome_var <- paste0(outcome_prefix, "_scaled")
  
  model_string <- paste(outcome_var, 
                        "~ group*time + age_scaled + education + fam_comp + (1 | id)"
  )
  
  return(brms::brmsformula(model_string))
  
}

## ---- select-prior
select_prior <- function(type){
  
  if(type == "wide"){
    
    prior <- brms::prior("normal(0, 0.50)", class = "b", coef = "group1:time2")
    
  } else if(type == "very wide") {
    
    prior <- brms::prior("normal(0, 1.00)", class = "b", coef = "group1:time2")
    
  }
  
  return(prior)
  
}

## ---- create-directory
create_directory <- function(location){
  
  if (!dir.exists(paste0(location, "/prior_sensitivity"))) {
    dir.create(paste0(location, "/prior_sensitivity"), recursive = TRUE)
  }
  
}

## ---- prior-sensitivity-test
prior_sensitivity_test <- function(data, outcome, prior_type){
  
  formula <- make_brms_formula(outcome)
  
  prior <- select_prior(prior_type)
  
  create_directory("output")
  
  message("Running: ", outcome, " | prior = ", prior_type)
  
  model <- brms::brm_multiple(
    formula,
    data = data_imputed_output,
    chains = 4,
    cores = 4,
    iter = 4000,
    warmup = 500,
    backend = "cmdstanr",
    control = list(
      adapt_delta = 0.95,
      max_treedepth = 15
    ),
    prior = prior
  )
  
  model_file <- paste0("output/prior_sensitivity/", "bayesian_model_", outcome_name,
                       "_", prior_name, "_", Sys.Date(), ".rds"
  )
  
  summary_file <- paste0(
    "output/prior_sensitivity/", "bayesian_model_", outcome_name,
    "_", prior_name, "_", Sys.Date(), ".txt"
  )
  
  saveRDS(model, file = model_file)
  
  sink(summary_file)
  print(summary(model))
  sink()
  
  return(model)
  
}

## ---- create-bayesian-convergence-stats
bayesian_convergence <- function(model_input, var, imputations = 122){
  
  m <- imputations
  
  draws <- as_draws_array(model_input)
  
  nc <- nchains(model_input) / m
  
  draws_per_dat <- lapply(
    1:m,
    \(i) subset_draws(
      draws,
      chain = ((i - 1) * nc + 1):(i * nc)
    )
  )
  
  diagnostics_per_dat <- lapply(
    draws_per_dat,
    summarise_draws,
    default_convergence_measures()
  )
  
  saveRDS(diagnostics_per_dat, paste0("output/model_output/", 
                                      "bayesian_model_", 
                                      var, 
                                      "_output.rds"))
  
  return(diagnostics_per_dat)
  
}

## ---- output-bayesian-convergence-table
convergence_table <- function(model_input){
  
  diagnostics_per_dat <- readRDS(paste0("output/model_output/", 
                                    "bayesian_model_", 
                                    model_input, 
                                    "_output.rds"))
  
  diagnostics_all <- do.call(
    rbind,
    lapply(
      seq_along(diagnostics_per_dat),
      function(i) {
        x <- diagnostics_per_dat[[i]]
        x$imputation <- i
        x
      }
    )
  )
  
  rownames(diagnostics_all) <- NULL

  summary_diagnostics <- diagnostics_all %>%
    group_by(variable) %>%
    summarize(
      max_Rhat = max(rhat, na.rm = TRUE),
      min_bulk_ESS = min(ess_bulk, na.rm = TRUE),
      median_bulk_ESS = median(ess_bulk, na.rm = TRUE),
      min_tail_ESS = min(ess_tail, na.rm = TRUE),
      median_tail_ESS = median(ess_tail, na.rm = TRUE),
      .groups = "drop"
    )
  
  target_order <- c(
    "sd_id__Intercept",
    "b_Intercept",
    "b_group1",
    "b_time2",
    "b_age_scaled",
    "b_education.L",
    "b_education.Q",
    "b_education.C",
    "b_fam_comp.L",
    "b_fam_comp.Q",
    "b_group1:time2",
    "sigma"
  )
  
  summary_diagnostics <- summary_diagnostics[
    match(target_order, 
          summary_diagnostics$variable), ]
  
  rownames(summary_diagnostics) <- NULL
  
  summary_file_convergence <- paste0(
    "output/", "bayesian_model_convergence_", 
    model_input,
    "_", Sys.Date(), ".txt"
  )

  write.table(
    summary_diagnostics, 
    file = summary_file_convergence, 
    sep = "\t", 
    row.names = FALSE, 
    quote = FALSE
  )
  
  return(summary_diagnostics)
  
}