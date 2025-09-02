## ---- output-Bayesian-model-stats

# ---- model-comm

# model-estimates
brm_model_comm_output <- 
  posterior_summary(bayesian_model_comm, 
                    probs = c(0.025, 0.975), 
                    robust = TRUE, 
                    variable = c("b_Intercept", 
                                 "b_group1", 
                                 "b_time2", 
                                 "b_age_scaled", 
                                 "b_education.L", 
                                 "b_education.Q", 
                                 "b_education.C", 
                                 "b_fam_comp.L", 
                                 "b_fam_comp.Q", 
                                 "b_group1:time2", 
                                 "sd_id__Intercept", 
                                 "sigma"))

brm_model_comm_output

# statistical-significance-estimates
model_brm_comm_pd_values <- p_direction(bayesian_model_comm)

print(model_brm_01_pd_values)

# practical-significance-estimates
brm_comm_rope_values <- p_rope(bayesian_model_comm)

print(brm_comm_rope_values)

# explanatory-power
bayes_R2(bayesian_model_comm, summary = TRUE, ndraws = 1000)

# general-summary
summary(bayesian_model_comm)

# ---- model-safe

# model-estimates
brm_model_safe_output <- 
  posterior_summary(bayesian_model_safe, 
                    probs = c(0.025, 0.975), 
                    robust = TRUE, 
                    variable = c("b_Intercept", 
                                 "b_group1", 
                                 "b_time2", 
                                 "b_age_scaled", 
                                 "b_education.L", 
                                 "b_education.Q", 
                                 "b_education.C", 
                                 "b_fam_comp.L", 
                                 "b_fam_comp.Q", 
                                 "b_group1:time2", 
                                 "sd_id__Intercept", 
                                 "sigma"))

brm_model_safe_output

# statistical-significance-estimates
model_brm_safe_pd_values <- p_direction(bayesian_model_safe)

print(model_brm_safe_pd_values)

# practical-significance-estimates
brm_safe_rope_values <- p_rope(bayesian_model_safe)

print(brm_safe_rope_values)

# explanatory-power
bayes_R2(bayesian_model_safe, summary = TRUE, ndraws = 1000)

# general-summary
summary(bayesian_model_safe)

# ---- model-hapa2

# model-estimates
brm_model_hapa2_output <- 
  posterior_summary(bayesian_model_hapa2, 
                    probs = c(0.025, 0.975), 
                    robust = TRUE, 
                    variable = c("b_Intercept", 
                                 "b_group1", 
                                 "b_time2", 
                                 "b_age_scaled", 
                                 "b_education.L", 
                                 "b_education.Q", 
                                 "b_education.C", 
                                 "b_fam_comp.L", 
                                 "b_fam_comp.Q", 
                                 "b_group1:time2", 
                                 "sd_id__Intercept", 
                                 "sigma"))

brm_model_hapa2_output

# statistical-significance-estimates
model_brm_hapa2_pd_values <- p_direction(bayesian_model_hapa2)

print(model_brm_hapa2_pd_values)

# practical-significance-estimates
brm_hapa2_rope_values <- p_rope(bayesian_model_hapa2)

print(brm_hapa2_rope_values)

# explanatory-power
bayes_R2(bayesian_model_hapa2, summary = TRUE, ndraws = 1000)

# general-summary
summary(bayesian_model_hapa2)

# ---- model-hapa3

# model-estimates
brm_model_hapa3_output <- 
  posterior_summary(bayesian_model_hapa3, 
                    probs = c(0.025, 0.975), 
                    robust = TRUE, 
                    variable = c("b_Intercept", 
                                 "b_group1", 
                                 "b_time2", 
                                 "b_age_scaled", 
                                 "b_education.L", 
                                 "b_education.Q", 
                                 "b_education.C", 
                                 "b_fam_comp.L", 
                                 "b_fam_comp.Q", 
                                 "b_group1:time2", 
                                 "sd_id__Intercept", 
                                 "sigma"))

brm_model_hapa3_output

# statistical-significance-estimates
model_brm_hapa3_pd_values <- p_direction(bayesian_model_hapa3)

print(model_brm_hapa3_pd_values)

# practical-significance-estimates
brm_hapa3_rope_values <- p_rope(bayesian_model_hapa3)

print(brm_hapa3_rope_values)

# explanatory-power
bayes_R2(bayesian_model_hapa3, summary = TRUE, ndraws = 1000)

# general-summary
summary(bayesian_model_hapa3)

# ---- model-hapa5

# model-estimates
brm_model_hapa5_output <- 
  posterior_summary(bayesian_model_hapa5, 
                    probs = c(0.025, 0.975), 
                    robust = TRUE, 
                    variable = c("b_Intercept", 
                                 "b_group1", 
                                 "b_time2", 
                                 "b_age_scaled", 
                                 "b_education.L", 
                                 "b_education.Q", 
                                 "b_education.C", 
                                 "b_fam_comp.L", 
                                 "b_fam_comp.Q", 
                                 "b_group1:time2", 
                                 "sd_id__Intercept", 
                                 "sigma"))

brm_model_hapa5_output

# statistical-significance-estimates
model_brm_hapa5_pd_values <- p_direction(bayesian_model_hapa5)

print(model_brm_hapa5_pd_values)

# practical-significance-estimates
brm_hapa5_rope_values <- p_rope(bayesian_model_hapa5)

print(brm_hapa5_rope_values)

# explanatory-power
bayes_R2(bayesian_model_hapa5, summary = TRUE, ndraws = 1000)

# general-summary
summary(bayesian_model_hapa5)