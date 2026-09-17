## ---- output-Bayesian-model-stats

# ---- model-comm

# model-estimates
brm_model_comm_output <- 
  posterior_summary(bayesian_model_single_comm, 
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

# trace-plots
mcmc_plot(bayesian_model_single_comm, type = "trace")

# posterior-predictive-check
pp_check(bayesian_model_single_comm, ndraws = 100) +
labs(
  title = "Communication competency - Posterior Predictive Check",
  subtitle = "Observed (y) vs. posterior predicted distributions (yrep)",
  x = "Communication competency (scaled)",
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

# ---- model-safe

# model-estimates
brm_model_safe_output <- 
  posterior_summary(bayesian_model_single_safe, 
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

# trace-plots
mcmc_plot(bayesian_model_single_safe, type = "trace")

# posterior-predictive-check
pp_check(bayesian_model_single_safe, ndraws = 100) +
  labs(
    title = "Perceived PAEs - Posterior Predictive Check",
    subtitle = "Observed (y) vs. posterior predicted distributions (yrep)",
    x = "Perceived PAEs (scaled)",
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

# ---- model-hapa2

# model-estimates
brm_model_hapa2_output <- 
  posterior_summary(bayesian_model_single_hapa2, 
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

# trace-plots
mcmc_plot(bayesian_model_single_hapa2, type = "trace")

# posterior-predictive-check
pp_check(bayesian_model_single_hapa2, ndraws = 100) +
  labs(
    title = "Outcome expectancy - Posterior Predictive Check",
    subtitle = "Observed (y) vs. posterior predicted distributions (yrep)",
    x = "Outcome expectancy (scaled)",
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

# ---- model-hapa3

# model-estimates
brm_model_hapa3_output <- 
  posterior_summary(bayesian_model_single_hapa3, 
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

# trace-plots
mcmc_plot(bayesian_model_single_hapa3, type = "trace")

# posterior-predictive-check
pp_check(bayesian_model_single_hapa3, ndraws = 100) +
  labs(
    title = "Coping self-efficacy - Posterior Predictive Check",
    subtitle = "Observed (y) vs. posterior predicted distributions (yrep)",
    x = "Coping self-efficacy (scaled)",
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

# ---- model-hapa5

# model-estimates
brm_model_hapa5_output <- 
  posterior_summary(bayesian_model_single_hapa5, 
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

# trace-plots
mcmc_plot(bayesian_model_single_hapa5, type = "trace")

# posterior-predictive-check
pp_check(bayesian_model_single_hapa5, ndraws = 100) +
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
