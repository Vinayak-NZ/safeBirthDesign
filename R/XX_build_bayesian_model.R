## ----apply-bayesian

app_v2_imputed$group <- 
  as.factor(app_v2_imputed$group)

app_v2_imputed$education <- 
  as.factor(app_v2_imputed$education)

app_v2_imputed$fam_comp <- 
  as.factor(app_v2_imputed$fam_comp)

app_v2_imputed_long <- 
  app_v2_imputed[, !(names(app_v2_imputed) %in% c("hapa1_post", 
                                                  "safe1_pre", 
                                                  "safe1_post", 
                                                  "safe2_pre", 
                                                  "safe2_post",
                                                  "comm1_pre", 
                                                  "comm1_post", 
                                                  "comm2_pre", 
                                                  "comm2_post", 
                                                  "comm3_pre", 
                                                  "comm3_post", 
                                                  "comm4_pre", 
                                                  "comm4_post", 
                                                  "comm5_pre", 
                                                  "comm5_post",
                                                  "comm6_pre", 
                                                  "comm6_post", 
                                                  "comm7_pre", 
                                                  "comm7_post"))]

setDT(app_v2_imputed_long)

app_v2_imputed_long <- melt(app_v2_imputed_long, 
                                 id.vars = c("UserCode",
                                             "group",
                                             "age",
                                             "education",
                                             "fam_comp"), 
                                 measure.vars = list(c("hapa2_pre", "hapa2_post"), 
                                                     c("hapa3_pre", "hapa3_post"), 
                                                     c("hapa4_pre", "hapa4_post"), 
                                                     c("hapa5_pre", "hapa5_post"), 
                                                     c("comm_mean_pre", "comm_mean_post"), 
                                                     c("safe_mean_pre", "safe_mean_post")),
                                 variable.name = "time", 
                                 value.name = c("hapa2", 
                                                "hapa3", 
                                                "hapa4", 
                                                "hapa5", 
                                                "comm_mean", 
                                                "safe_mean"))

app_v2_imputed_long$group <- 
  factor(app_v2_imputed_long$group, 
         order = FALSE, 
         levels = c(0, 1))

app_v2_imputed_long$time <- 
  factor(app_v2_imputed_long$time, 
         order = FALSE, 
         levels = c(1, 2))

app_v2_imputed_long$education <- 
  factor(app_v2_imputed_long$education, 
         order = TRUE, 
         levels = c(1, 2, 3, 4))

app_v2_imputed_long$fam_comp <- 
  factor(app_v2_imputed_long$fam_comp, 
         order = TRUE, 
         levels = c(1, 2, 3))

app_v2_imputed_long$comm_mean_scaled <- 
  scale(app_v2_imputed_long$comm_mean)[,1]

app_v2_imputed_long$safe_mean_scaled <- 
  scale(app_v2_imputed_long$safe_mean)[,1]

app_v2_imputed_long$age_scaled <- 
  scale(app_v2_imputed_long$age)[,1]

app_v2_imputed_long$hapa3_scaled <- 
  scale(app_v2_imputed_long$hapa3)[,1]

app_v2_imputed_long$hapa5_scaled <- 
  scale(app_v2_imputed_long$hapa5)[,1]

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
                        (1 | UserCode), 
                      data = app_v2_imputed_long, 
                      prior = prior_comm)

summary(bayesian_model_comm)

bayesian_model_safe <- brm(safe_mean_scaled ~ group*time + 
                             age_scaled + 
                             education + 
                             fam_comp + 
                             (1 | UserCode), 
                           data = app_v2_imputed_long, 
                           prior = prior_safe)

summary(bayesian_model_safe)

bayesian_model_hapa3 <- brm(hapa3_scaled ~ group*time + 
                             age_scaled + 
                             education + 
                             fam_comp + 
                             (1 | UserCode), 
                           data = app_v2_imputed_long, 
                           prior = prior_hapa3)

summary(bayesian_model_hapa3)

bayesian_model_hapa5 <- brm(hapa5 ~ group*time + 
                             age_scaled + 
                             education + 
                             fam_comp + 
                             (1 | UserCode), 
                           data = app_v2_imputed_long, 
                           prior = prior_hapa5)

summary(bayesian_model_hapa5)

posterior <- as_draws_df(bayesian_model_hapa5)
mean(posterior$`b_group1:time2` > 0)

#model_brm_01 <- brm(SCON ~ group*time + stress + age_cat + sex + (1 | id), 
                    # family = gaussian(link = "log"), 
                    # data = data_imputed_long,  
                    # chains = 4, 
                    # cores = 4,
                    # iter = 4000, 
                    # warmup = 500, 
                    # backend = "cmdstanr", 
                    # control = list(adapt_delta = 0.8, max_treedepth = 10))
