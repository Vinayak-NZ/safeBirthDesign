## ---- brm-multiple-quick-test

# Use the existing list of completed datasets.
# DO NOT rbind the imputations.
data_imputed_list <- data_imputed_output

# Check
length(data_imputed_list)
nrow(data_imputed_list[[1]])
nrow(data_imputed_list[[2]])

# Optional: first test with only 5 imputations
data_imputed_test <- data_imputed_list[1:10]

# ---- prior

prior_comm <- brms::prior(
  "normal(0.19, 0.10)",
  class = "b",
  coef = "group1:time2"
)

# ---- brm_multiple model

bayesian_model_comm_test <- brms::brm_multiple(
  formula = comm_mean_scaled ~ group*time +
    age_scaled +
    education +
    fam_comp +
    (1 | id),
  
  data = data_imputed_test,
  
  prior = prior_comm,
  
  chains = 2,
  cores = 2,
  iter = 1500,
  warmup = 500,
  
  backend = "cmdstanr",
  
  control = list(
    adapt_delta = 0.95,
    max_treedepth = 15
  ),
  
  seed = 555,
  
  silent = 0
)

# ---- summary

summary(bayesian_model_comm_test)
