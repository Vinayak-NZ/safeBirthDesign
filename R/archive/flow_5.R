## ---- bayes-model-input

# NOTE: Do NOT use do.call(rbind, data_imputed_output). 
# brm_multiple expects a list of data frames or a mice 'mids' object.

# formulate-priors-intervention-effect

prior_comm  <- brms::prior("normal(0.19, 0.10)", class = "b", coef = "group1:time2")
prior_safe  <- brms::prior("normal(-0.20, 0.14)", class = "b", coef = "group1:time2")
prior_hapa2 <- brms::prior("normal(0.31, 0.15)", class = "b", coef = "group1:time2")
prior_hapa3 <- brms::prior("normal(0.13, 0.20)", class = "b", coef = "group1:time2")
prior_hapa5 <- brms::prior("normal(0.33, 0.19)", class = "b", coef = "group1:time2")


# models

# --- Communication Model ---
bayesian_model_comm <- brms::brm_multiple(
  comm_mean_scaled ~ group*time + age_scaled + education + fam_comp + (1 | id), 
  data = data_imputed_output, 
  chains = 4, 
  cores = 4, 
  iter = 4000, 
  warmup = 500, 
  backend = "cmdstanr", 
  control = list(adapt_delta = 0.95, max_treedepth = 15), 
  prior = prior_comm
)

saveRDS(bayesian_model_comm, file = paste0("output/bayesian_model_comm_", Sys.Date(), ".rds"))

sink(paste0("output/bayesian_model_comm_", Sys.Date(), ".txt"))
print(summary(bayesian_model_comm))
sink()


# --- Safety Model ---
bayesian_model_safe <- brms::brm_multiple(
  safe_mean_scaled ~ group*time + age_scaled + education + fam_comp + (1 | id), 
  data = data_imputed_output, 
  chains = 4, 
  cores = 4, 
  iter = 4000, 
  warmup = 500, 
  backend = "cmdstanr", 
  control = list(adapt_delta = 0.95, max_treedepth = 15), 
  prior = prior_safe
)

saveRDS(bayesian_model_safe, file = paste0("output/bayesian_model_safe_", Sys.Date(), ".rds"))

sink(paste0("output/bayesian_model_safe_", Sys.Date(), ".txt"))
print(summary(bayesian_model_safe))
sink()


# --- HAPA 2 Model ---
bayesian_model_hapa2 <- brms::brm_multiple(
  hapa2_scaled ~ group*time + age_scaled + education + fam_comp + (1 | id), 
  data = data_imputed_output, 
  chains = 4, 
  cores = 4, 
  iter = 4000, 
  warmup = 500, 
  backend = "cmdstanr", 
  control = list(adapt_delta = 0.95, max_treedepth = 15), 
  prior = prior_hapa2
)

saveRDS(bayesian_model_hapa2, file = paste0("output/bayesian_model_hapa2_", Sys.Date(), ".rds"))

sink(paste0("output/bayesian_model_hapa2_", Sys.Date(), ".txt"))
print(summary(bayesian_model_hapa2))
sink()


# --- HAPA 3 Model ---
bayesian_model_hapa3 <- brms::brm_multiple(
  hapa3_scaled ~ group*time + age_scaled + education + fam_comp + (1 | id), 
  data = data_imputed_output, 
  chains = 4, 
  cores = 4, 
  iter = 4000, 
  warmup = 500, 
  backend = "cmdstanr", 
  control = list(adapt_delta = 0.95, max_treedepth = 15), 
  prior = prior_hapa3
)

saveRDS(bayesian_model_hapa3, file = paste0("output/bayesian_model_hapa3_", Sys.Date(), ".rds"))

sink(paste0("output/bayesian_model_hapa3_", Sys.Date(), ".txt"))
print(summary(bayesian_model_hapa3))
sink()


# --- HAPA 5 Model ---
bayesian_model_hapa5 <- brms::brm_multiple(
  hapa5_scaled ~ group*time + age_scaled + education + fam_comp + (1 | id), 
  data = data_imputed_output, 
  chains = 4, 
  cores = 4, 
  iter = 4000, 
  warmup = 500, 
  backend = "cmdstanr", 
  control = list(adapt_delta = 0.95, max_treedepth = 15), 
  prior = prior_hapa5
)

saveRDS(bayesian_model_hapa5, file = paste0("output/bayesian_model_hapa5_", Sys.Date(), ".rds"))

sink(paste0("output/bayesian_model_hapa5_", Sys.Date(), ".txt"))
print(summary(bayesian_model_hapa5))
sink()
