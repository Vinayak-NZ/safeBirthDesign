## ---- bayes-model-input

data_imputed_long <- do.call(rbind, data_imputed_output)

# formulate-priors-intervention-effect

prior_comm <- brms::prior("normal(0.19, 0.10)", class = "b", coef = "group1:time2")

prior_safe <- brms::prior("normal(0.20, 0.14)", class = "b", coef = "group1:time2")

prior_hapa2 <- brms::prior("normal(0.31, 0.15)", class="b", coef = "group1:time2")

prior_hapa3 <- brms::prior("normal(-0.13, 0.20)", class="b", coef = "group1:time2")

prior_hapa5 <- brms::prior("normal(-0.33, 0.19)", class="b", coef = "group1:time2")


# formulate-priors-intervention-effect

bayesian_model_comm <- brm(comm_mean_scaled ~ group*time + 
                             age_scaled + 
                             education + 
                             fam_comp + 
                             (1 | id), 
                           data = data_imputed_long, 
                           chains = 4, 
                           cores = 4, 
                           iter = 4000, 
                           warmup = 500, 
                           backend = "cmdstanr", 
                           control = list(adapt_delta = 0.95, max_treedepth = 15), 
                           prior = prior_comm)

bayesian_model_comm_summary <- summary(bayesian_model_comm)

saveRDS(bayesian_model_comm, file = paste0("output/bayesian_model_comm_", Sys.Date(), ".rds"))

sink(paste0("output/bayesian_model_comm_", Sys.Date(), ".txt"))
print(summary(bayesian_model_comm))
sink()


bayesian_model_safe <- brm(safe_mean_scaled ~ group*time + 
                             age_scaled + 
                             education + 
                             fam_comp + 
                             (1 | id), 
                           data = data_imputed_long, 
                           chains = 4, 
                           cores = 4, 
                           iter = 4000, 
                           warmup = 500, 
                           backend = "cmdstanr", 
                           control = list(adapt_delta = 0.95, max_treedepth = 15), 
                           prior = prior_safe)

bayesian_model_safe_summary <- summary(bayesian_model_safe)

saveRDS(bayesian_model_safe, file = paste0("output/bayesian_model_safe_", Sys.Date(), ".rds"))

sink(paste0("output/bayesian_model_safe_", Sys.Date(), ".txt"))
print(summary(bayesian_model_safe))
sink()

bayesian_model_hapa2 <- brm(hapa2_scaled ~ group*time + 
                              age_scaled + 
                              education + 
                              fam_comp + 
                              (1 | id), 
                            data = data_imputed_long, 
                            chains = 4, 
                            cores = 4, 
                            iter = 4000, 
                            warmup = 500, 
                            backend = "cmdstanr", 
                            control = list(adapt_delta = 0.95, max_treedepth = 15), 
                            prior = prior_hapa2)

bayesian_model_hapa2_summary <- summary(bayesian_model_hapa2)

saveRDS(bayesian_model_hapa2, file = paste0("output/bayesian_model_hapa2_", Sys.Date(), ".rds"))

sink(paste0("output/bayesian_model_hapa2_", Sys.Date(), ".txt"))
print(summary(bayesian_model_hapa2))
sink()


bayesian_model_hapa3 <- brm(hapa3_scaled ~ group*time + 
                              age_scaled + 
                              education + 
                              fam_comp + 
                              (1 | id), 
                            data = data_imputed_long, 
                            chains = 4, 
                            cores = 4, 
                            iter = 4000, 
                            warmup = 500, 
                            backend = "cmdstanr", 
                            control = list(adapt_delta = 0.95, max_treedepth = 15), 
                            prior = prior_hapa3)

bayesian_model_hapa3_summary <- summary(bayesian_model_hapa3)

saveRDS(bayesian_model_hapa3, file = paste0("output/bayesian_model_hapa3_", Sys.Date(), ".rds"))

sink(paste0("output/bayesian_model_hapa3_", Sys.Date(), ".txt"))
print(summary(bayesian_model_hapa3))
sink()

bayesian_model_hapa5 <- brm(hapa5_scaled ~ group*time + 
                              age_scaled + 
                              education + 
                              fam_comp + 
                              (1 | id), 
                            data = data_imputed_long, 
                            chains = 4, 
                            cores = 4, 
                            iter = 4000, 
                            warmup = 500, 
                            backend = "cmdstanr", 
                            control = list(adapt_delta = 0.95, max_treedepth = 15), 
                            prior = prior_hapa5)

bayesian_model_hapa5_summary <- summary(bayesian_model_hapa5)

saveRDS(bayesian_model_hapa5, file = paste0("output/bayesian_model_hapa5_", Sys.Date(), ".rds"))

sink(paste0("output/bayesian_model_hapa5_", Sys.Date(), ".txt"))
print(summary(bayesian_model_hapa5))
sink()
