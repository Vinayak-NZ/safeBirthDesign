## ---- bayes-model-input-niad

# comm-model

niad_bayesian_model_comm <- brm(comm_mean_scaled ~ group*time + 
                             age_scaled + 
                             education + 
                             fam_comp + 
                             (1 | id), 
                           data = app_v2_niad, 
                           chains = 4, 
                           cores = 4, 
                           iter = 4000, 
                           warmup = 500, 
                           backend = "cmdstanr", 
                           control = list(adapt_delta = 0.95, max_treedepth = 15))

niad_bayesian_model_comm_summary <- summary(niad_bayesian_model_comm)

saveRDS(niad_bayesian_model_comm, file = paste0("output/niad_bayesian_model_comm_", Sys.Date(), ".rds"))

sink(paste0("output/niad_bayesian_model_comm_", Sys.Date(), ".txt"))
print(summary(niad_bayesian_model_comm))
sink()


niad_bayesian_model_safe <- brm(safe_mean_scaled ~ group*time + 
                             age_scaled + 
                             education + 
                             fam_comp + 
                             (1 | id), 
                           data = app_v2_niad, 
                           chains = 4, 
                           cores = 4, 
                           iter = 4000, 
                           warmup = 500, 
                           backend = "cmdstanr", 
                           control = list(adapt_delta = 0.95, max_treedepth = 15))

niad_bayesian_model_safe_summary <- summary(niad_bayesian_model_safe)

saveRDS(niad_bayesian_model_safe, file = paste0("output/niad_bayesian_model_safe_", Sys.Date(), ".rds"))

sink(paste0("output/niad_bayesian_model_safe_", Sys.Date(), ".txt"))
print(summary(niad_bayesian_model_safe))
sink()

niad_bayesian_model_hapa2 <- brm(hapa2_scaled ~ group*time + 
                              age_scaled + 
                              education + 
                              fam_comp + 
                              (1 | id), 
                            data = app_v2_niad, 
                            chains = 4, 
                            cores = 4, 
                            iter = 4000, 
                            warmup = 500, 
                            backend = "cmdstanr", 
                            control = list(adapt_delta = 0.95, max_treedepth = 15))

niad_bayesian_model_hapa2_summary <- summary(niad_bayesian_model_hapa2)

saveRDS(niad_bayesian_model_hapa2, file = paste0("output/niad_bayesian_model_hapa2_", Sys.Date(), ".rds"))

sink(paste0("output/niad_bayesian_model_hapa2_", Sys.Date(), ".txt"))
print(summary(niad_bayesian_model_hapa2))
sink()


niad_bayesian_model_hapa3 <- brm(hapa3_scaled ~ group*time + 
                              age_scaled + 
                              education + 
                              fam_comp + 
                              (1 | id), 
                            data = app_v2_niad, 
                            chains = 4, 
                            cores = 4, 
                            iter = 4000, 
                            warmup = 500, 
                            backend = "cmdstanr", 
                            control = list(adapt_delta = 0.95, max_treedepth = 15))

niad_bayesian_model_hapa3_summary <- summary(niad_bayesian_model_hapa3)

saveRDS(niad_bayesian_model_hapa3, file = paste0("output/niad_bayesian_model_hapa3_", Sys.Date(), ".rds"))

sink(paste0("output/niad_bayesian_model_hapa3_", Sys.Date(), ".txt"))
print(summary(niad_bayesian_model_hapa3))
sink()

niad_bayesian_model_hapa5 <- brm(hapa5_scaled ~ group*time + 
                              age_scaled + 
                              education + 
                              fam_comp + 
                              (1 | id), 
                            data = app_v2_niad, 
                            chains = 4, 
                            cores = 4, 
                            iter = 4000, 
                            warmup = 500, 
                            backend = "cmdstanr", 
                            control = list(adapt_delta = 0.95, max_treedepth = 15))

niad_bayesian_model_hapa5_summary <- summary(niad_bayesian_model_hapa5)

saveRDS(niad_bayesian_model_hapa5, file = paste0("output/niad_bayesian_model_hapa5_", Sys.Date(), ".rds"))

sink(paste0("output/niad_bayesian_model_hapa5_", Sys.Date(), ".txt"))
print(summary(niad_bayesian_model_hapa5))
sink()
