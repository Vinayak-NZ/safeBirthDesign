## ---- plot-model-estimates

# model-comm
model_01_comm <- 
  broom.mixed::tidy(bayesian_model_comm, effects = "fixed", conf.int = TRUE)

model_01_comm$term <- dplyr::recode(model_01_comm$term,
                           "(Intercept)" = "Intercept",
                           "group1" = "Intervention",
                           "time2" = "Time (Follow-up)",
                           "group1:time2" = "Intervention × Time", 
                           "age_scaled" = "Age", 
                           "education.L" = "Education (Linear)",
                           "education.Q" = "Education (Quadratic)",
                           "education.C" = "Education (Cubic)",
                           "fam_comp.L" = "Family Composition (Linear)",
                           "fam_comp.Q" = "Family Composition (Quadratic)"
)

model_01_comm_plot <- 
  ggplot(model_01_comm, aes(x = estimate, y = factor(term, 
                                                      levels = c("Intercept",
                                                                 "Age",
                                                                 "Education (Linear)",
                                                                 "Education (Quadratic)", 
                                                                 "Education (Cubic)",
                                                                 "Family Composition (Linear)",
                                                                 "Family Composition (Quadratic)",
                                                                 "Time (Follow-up)",
                                                                 "Intervention", 
                                                                 "Intervention × Time")))) +
  geom_point(color = "#205B87", size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 height = 0.2, color = "#205B87") +
  labs(title = "Pooled Posterior Estimates",
       subtitle = "95% Credible Intervals (from multiply imputed model)",
       x = "Estimate",
       y = NULL) +
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

ggsave("output/model_01_comm_plot.png", 
       plot = model_01_comm_plot, 
       device = 'tiff', 
       width = 12, height = 6)

# model-safe

model_02_safe <- 
  broom.mixed::tidy(bayesian_model_safe, effects = "fixed", conf.int = TRUE)

model_02_safe$term <- dplyr::recode(model_02_safe$term,
                             "(Intercept)" = "Intercept",
                             "group1" = "Intervention",
                             "time2" = "Time (Follow-up)",
                             "group1:time2" = "Intervention × Time", 
                             "age_scaled" = "Age", 
                             "education.L" = "Education (Linear)",
                             "education.Q" = "Education (Quadratic)",
                             "education.C" = "Education (Cubic)",
                             "fam_comp.L" = "Family Composition (Linear)",
                             "fam_comp.Q" = "Family Composition (Quadratic)"
)

model_02_safe_plot <- 
  ggplot(model_02_safe, aes(x = estimate, y = factor(term, 
                                                     levels = c("Intercept",
                                                                "Age",
                                                                "Education (Linear)",
                                                                "Education (Quadratic)", 
                                                                "Education (Cubic)",
                                                                "Family Composition (Linear)",
                                                                "Family Composition (Quadratic)",
                                                                "Time (Follow-up)",
                                                                "Intervention", 
                                                                "Intervention × Time")))) +
  geom_point(color = "#205B87", size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 height = 0.2, color = "#205B87") +
  labs(title = "Pooled Posterior Estimates",
       subtitle = "95% Credible Intervals (from multiply imputed model)",
       x = "Estimate",
       y = NULL) +
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

ggsave("output/model_02_safe_plot.png", 
       plot = model_02_safe_plot, 
       device = 'tiff', 
       width = 12, height = 6)

# model-hapa2

model_03_hapa2 <- 
  broom.mixed::tidy(bayesian_model_hapa2, effects = "fixed", conf.int = TRUE)

model_03_hapa2$term <- dplyr::recode(model_03_hapa2$term,
                             "(Intercept)" = "Intercept",
                             "group1" = "Intervention",
                             "time2" = "Time (Follow-up)",
                             "group1:time2" = "Intervention × Time", 
                             "age_scaled" = "Age", 
                             "education.L" = "Education (Linear)",
                             "education.Q" = "Education (Quadratic)",
                             "education.C" = "Education (Cubic)",
                             "fam_comp.L" = "Family Composition (Linear)",
                             "fam_comp.Q" = "Family Composition (Quadratic)"
)

model_03_hapa2_plot <- 
  ggplot(model_03_hapa2, aes(x = estimate, y = factor(term, 
                                                      levels = c("Intercept",
                                                                 "Age",
                                                                 "Education (Linear)",
                                                                 "Education (Quadratic)", 
                                                                 "Education (Cubic)",
                                                                 "Family Composition (Linear)",
                                                                 "Family Composition (Quadratic)",
                                                                 "Time (Follow-up)",
                                                                 "Intervention", 
                                                                 "Intervention × Time")))) +
  geom_point(color = "#205B87", size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 height = 0.2, color = "#205B87") +
  labs(title = "Pooled Posterior Estimates",
       subtitle = "95% Credible Intervals (from multiply imputed model)",
       x = "Estimate",
       y = NULL) +
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

ggsave("output/model_03_hapa2_plot.png", 
       plot = model_03_hapa2_plot, 
       device = 'tiff', 
       width = 12, height = 6)

# model-hapa3

model_04_hapa3 <- 
  broom.mixed::tidy(bayesian_model_hapa3, effects = "fixed", conf.int = TRUE)

model_04_hapa3$term <- dplyr::recode(model_04_hapa3$term,
                                     "(Intercept)" = "Intercept",
                                     "group1" = "Intervention",
                                     "time2" = "Time (Follow-up)",
                                     "group1:time2" = "Intervention × Time", 
                                     "age_scaled" = "Age", 
                                     "education.L" = "Education (Linear)",
                                     "education.Q" = "Education (Quadratic)",
                                     "education.C" = "Education (Cubic)",
                                     "fam_comp.L" = "Family Composition (Linear)",
                                     "fam_comp.Q" = "Family Composition (Quadratic)"
)

model_04_hapa3_plot <- 
  ggplot(model_04_hapa3, aes(x = estimate, y = factor(term, 
                                                      levels = c("Intercept",
                                                                 "Age",
                                                                 "Education (Linear)",
                                                                 "Education (Quadratic)", 
                                                                 "Education (Cubic)",
                                                                 "Family Composition (Linear)",
                                                                 "Family Composition (Quadratic)",
                                                                 "Time (Follow-up)",
                                                                 "Intervention", 
                                                                 "Intervention × Time")))) +
  geom_point(color = "#205B87", size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 height = 0.2, color = "#205B87") +
  labs(title = "Pooled Posterior Estimates",
       subtitle = "95% Credible Intervals (from multiply imputed model)",
       x = "Estimate",
       y = NULL) +
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

ggsave("output/model_04_hapa3_plot.png", 
       plot = model_04_hapa3_plot, 
       device = 'tiff', 
       width = 12, height = 6)

# model-hapa5

model_05_hapa5 <- 
  broom.mixed::tidy(bayesian_model_hapa5, effects = "fixed", conf.int = TRUE)

model_05_hapa5$term <- dplyr::recode(model_05_hapa5$term,
                              "(Intercept)" = "Intercept",
                              "group1" = "Intervention",
                              "time2" = "Time (Follow-up)",
                              "group1:time2" = "Intervention × Time", 
                              "age_scaled" = "Age", 
                              "education.L" = "Education (Linear)",
                              "education.Q" = "Education (Quadratic)",
                              "education.C" = "Education (Cubic)",
                              "fam_comp.L" = "Family Composition (Linear)",
                              "fam_comp.Q" = "Family Composition (Quadratic)"
)

model_05_hapa5_plot <- 
  ggplot(model_05_hapa5, aes(x = estimate, y = factor(term, 
                                                      levels = c("Intercept",
                                                                 "Age",
                                                                 "Education (Linear)",
                                                                 "Education (Quadratic)", 
                                                                 "Education (Cubic)",
                                                                 "Family Composition (Linear)",
                                                                 "Family Composition (Quadratic)",
                                                                 "Time (Follow-up)",
                                                                 "Intervention", 
                                                                 "Intervention × Time")))) +
  geom_point(color = "#205B87", size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 height = 0.2, color = "#205B87") +
  labs(title = "Pooled Posterior Estimates",
       subtitle = "95% Credible Intervals (from multiply imputed model)",
       x = "Estimate",
       y = NULL) +
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

ggsave("output/model_05_hapa5_plot.png", 
       plot = model_05_hapa5_plot, 
       device = 'tiff', 
       width = 12, height = 6)
