
app_v2_aux_var <- app_v2

names(app_v2_aux_var) <- 
  sub("_t0$", "_pre", names(app_v2_aux_var))

pre_vars <- 
  grep("_pre$", names(app_v2_aux_var), value = TRUE)

app_v2_aux_var <- Reduce(function(d, v) tx_post(v, d), 
                                   vars, 
                                   init = app_v2_aux_var)

app_v2_aux_var[pre_vars] <- lapply(
  app_v2_aux_var[pre_vars],
  function(x) {
    x[x == 0] <- 1            
    ((x - 1) / 5) * 100      
  }
)

app_v2_aux_var$comm_mean_pre <- rowMeans(
  app_v2_aux_var[, paste0("comm", 1:7, "_pre")],
  na.rm = TRUE
)

app_v2_aux_var$comm_mean_post <- rowMeans(
  app_v2_aux_var[, paste0("comm", 1:7, "_post")],
  na.rm = TRUE
)

app_v2_aux_var$safe_mean_pre <- rowMeans(
  app_v2_aux_var[, paste0("safe", 1:2, "_pre")],
  na.rm = TRUE
)

app_v2_aux_var$safe_mean_post <- rowMeans(
  app_v2_aux_var[, paste0("safe", 1:2, "_post")],
  na.rm = TRUE
)


sd(app_v2_aux_var$comm_mean_pre)*bayesian_model_comm_summary$fixed$Estimate[10]

sd(app_v2_aux_var$safe_mean_pre)*bayesian_model_safe_summary$fixed$Estimate[10]

sd(app_v2_aux_var$hapa3_pre)*bayesian_model_hapa3_summary$fixed$Estimate[10]

sd(app_v2_aux_var$hapa5_pre)*bayesian_model_hapa5_summary$fixed$Estimate[10]

rel_comm   <- 100 * 5.283634  / mean(app_v2_aux_var$comm_mean_pre,  na.rm=TRUE)
rel_safe   <- 100 * (-8.891069) / mean(app_v2_aux_var$safe_mean_pre,  na.rm=TRUE) 
rel_hapa3  <- 100 * 4.311998   / mean(app_v2_aux_var$hapa3_pre,      na.rm=TRUE)
rel_hapa5  <- 100 * 21.57816   / mean(app_v2_aux_var$hapa5_pre,      na.rm=TRUE)
