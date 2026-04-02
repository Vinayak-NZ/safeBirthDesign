## ---- sensitivity-checks-build-models

prior_comm <- brms::prior("normal(0.19, 0.10)", class = "b", coef = "group1:time2")

prior_safe <- brms::prior("normal(-0.20, 0.14)", class = "b", coef = "group1:time2")

prior_hapa2 <- brms::prior("normal(0.31, 0.15)", class="b", coef = "group1:time2")

prior_hapa3 <- brms::prior("normal(0.13, 0.20)", class="b", coef = "group1:time2")

prior_hapa5 <- brms::prior("normal(0.33, 0.19)", class="b", coef = "group1:time2")

results_comm <- list()

for (nm in names(sensitivity_comm)) {
  dat_long <- prepare_analysis_data(sensitivity_comm[[nm]])
  
  fit <- brm(
    comm_mean_scaled ~ group * time + age_scaled + education + fam_comp + (1 | id),
    data = dat_long,
    chains = 4,
    cores = 4,
    iter = 4000,
    warmup = 500,
    backend = "cmdstanr",
    control = list(adapt_delta = 0.95, max_treedepth = 15),
    prior = prior_comm
  )
  
  results_comm[[nm]] <- fit
}

saveRDS(results_comm, "output/sensitivity_comm.rds")

results_safe <- list()

for (nm in names(sensitivity_safe)) {
  dat_long <- prepare_analysis_data(sensitivity_safe[[nm]])
  
  fit <- brm(
    safe_mean_scaled ~ group * time + age_scaled + education + fam_comp + (1 | id),
    data = dat_long,
    chains = 4,
    cores = 4,
    iter = 4000,
    warmup = 500,
    backend = "cmdstanr",
    control = list(adapt_delta = 0.95, max_treedepth = 15),
    prior = prior_safe
  )
  
  results_safe[[nm]] <- fit
}

saveRDS(results_safe, "output/sensitivity_safe.rds")

results_hapa2 <- list()

for (nm in names(sensitivity_hapa2)) {
  dat_long <- prepare_analysis_data(sensitivity_hapa2[[nm]])
  
  fit <- brm(
    hapa2_scaled ~ group * time + age_scaled + education + fam_comp + (1 | id),
    data = dat_long,
    chains = 4,
    cores = 4,
    iter = 4000,
    warmup = 500,
    backend = "cmdstanr",
    control = list(adapt_delta = 0.95, max_treedepth = 15),
    prior = prior_hapa2
  )
  
  results_hapa2[[nm]] <- fit
}

saveRDS(results_hapa2, "output/sensitivity_hapa2.rds")

results_hapa3 <- list()

for (nm in names(sensitivity_hapa3)) {
  dat_long <- prepare_analysis_data(sensitivity_hapa3[[nm]])
  
  fit <- brm(
    hapa3_scaled ~ group * time + age_scaled + education + fam_comp + (1 | id),
    data = dat_long,
    chains = 4,
    cores = 4,
    iter = 4000,
    warmup = 500,
    backend = "cmdstanr",
    control = list(adapt_delta = 0.95, max_treedepth = 15),
    prior = prior_hapa3
  )
  
  results_hapa3[[nm]] <- fit
}

saveRDS(results_hapa3, "output/sensitivity_hapa3.rds")

results_hapa5 <- list()

for (nm in names(sensitivity_hapa5)) {
  dat_long <- prepare_analysis_data(sensitivity_hapa5[[nm]])
  
  fit <- brm(
    hapa5_scaled ~ group * time + age_scaled + education + fam_comp + (1 | id),
    data = dat_long,
    chains = 4,
    cores = 4,
    iter = 4000,
    warmup = 500,
    backend = "cmdstanr",
    control = list(adapt_delta = 0.95, max_treedepth = 15),
    prior = prior_hapa5
  )
  
  results_hapa5[[nm]] <- fit
}

saveRDS(results_hapa5, "output/sensitivity_hapa5.rds")

