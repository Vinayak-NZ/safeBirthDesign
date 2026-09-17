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

# trace-plots
mcmc_plot(bayesian_model_hapa5, type = "trace")

# posterior-predictive-check
pp_check(bayesian_model_hapa5, ndraws = 100) +
  labs(
    title = "Action planning - Posterior Predictive Check",
    subtitle = "Observed (y) vs. posterior predicted distributions (yrep)",
    x = "Action planning (scaled)",
    y = "Density"
  ) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
    plot.subtitle = element_text(color = "#454543"),
    plot.caption = element_text(color = "#454543", face = "italic")
  )
