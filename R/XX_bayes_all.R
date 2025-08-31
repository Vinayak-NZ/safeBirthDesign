## ---- bayes-model-input

data_imputed_long <- do.call(rbind, data_imputed_output)

# formulate-priors-intervention-effect

prior_comm <- brms::prior("normal(0.19, 0.10)", class = "b", coef = "group1:time2")

prior_safe <- brms::prior("normal(0.20, 0.14)", class = "b", coef = "group1:time2")

prior_hapa3 <- brms::prior("normal(-0.13, 0.20)", class="b", coef = "group1:time2")

prior_hapa5 <- brms::prior("normal(-0.33, 0.19)", class="b", coef = "group1:time2")


# formulate-priors-intervention-effect

bayesian_model_comm <- brm(comm_mean_scaled ~ group*time + 
                             age_scaled + 
                             education + 
                             fam_comp + 
                             (1 | id), 
                           data = data_imputed_long, 
                           prior = prior_comm)

summary(bayesian_model_comm)

bayesian_model_safe <- brm(safe_mean_scaled ~ group*time + 
                             age_scaled + 
                             education + 
                             fam_comp + 
                             (1 | id), 
                           data = data_imputed_long, 
                           prior = prior_safe)

summary(bayesian_model_safe)

bayesian_model_hapa3 <- brm(hapa3_scaled ~ group*time + 
                              age_scaled + 
                              education + 
                              fam_comp + 
                              (1 | id), 
                            data = data_imputed_long, 
                            prior = prior_hapa3)

summary(bayesian_model_hapa3)

bayesian_model_hapa5 <- brm(hapa5_scaled ~ group*time + 
                              age_scaled + 
                              education + 
                              fam_comp + 
                              (1 | id), 
                            data = data_imputed_long, 
                            prior = prior_hapa5)

summary(bayesian_model_hapa5)



#
posterior <- as_draws_df(bayesian_model_safe)
mean(posterior$`b_group1:time2` > 0)
