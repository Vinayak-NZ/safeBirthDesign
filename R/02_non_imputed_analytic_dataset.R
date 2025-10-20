## ---- non-imputed-analytic-dataset

app_v2_niad <- app_v2

vars <- c("comm1","comm2","comm3","comm4","comm5","comm6","comm7",
          "hapa2","hapa3","hapa4","hapa5",
          "safe1","safe2")

names(app_v2_niad) <- 
  sub("_t0$", "_pre", names(app_v2_niad))

pre_vars <- 
  grep("_pre$", names(app_v2_niad), value = TRUE)

app_v2_niad <- Reduce(function(d, v) tx_post(v, d), 
                                   vars, 
                                   init = app_v2_niad)

app_v2_niad[pre_vars] <- lapply(
  app_v2_niad[pre_vars],
  function(x) {
    x[x == 0] <- 1            
    ((x - 1) / 5) * 100      
  }
)

app_v2_niad$comm_mean_pre <- rowMeans(
  app_v2_niad[, paste0("comm", 1:7, "_pre")],
  na.rm = TRUE
)

app_v2_niad$comm_mean_post <- rowMeans(
  app_v2_niad[, paste0("comm", 1:7, "_post")],
  na.rm = TRUE
)

app_v2_niad$safe_mean_pre <- rowMeans(
  app_v2_niad[, paste0("safe", 1:2, "_pre")],
  na.rm = TRUE
)

app_v2_niad$safe_mean_post <- rowMeans(
  app_v2_niad[, paste0("safe", 1:2, "_post")],
  na.rm = TRUE
)

app_v2_niad <- 
  app_v2_niad[, !(names(app_v2_niad) %in% c("hapa1_post", 
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

setDT(app_v2_niad)

app_v2_niad <- melt(app_v2_niad, 
                                 id.vars = c("id",
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

app_v2_niad$time <- 
  factor(app_v2_niad$time, 
         order = FALSE, 
         levels = c(1, 2))

app_v2_niad$comm_mean_scaled <- 
  scale(app_v2_niad$comm_mean)[,1]

app_v2_niad$safe_mean_scaled <- 
  scale(app_v2_niad$safe_mean)[,1]

app_v2_niad$age_scaled <- 
  scale(app_v2_niad$age)[,1]

app_v2_niad$hapa2_scaled <- 
  scale(app_v2_niad$hapa2)[,1]

app_v2_niad$hapa3_scaled <- 
  scale(app_v2_niad$hapa3)[,1]

app_v2_niad$hapa5_scaled <- 
  scale(app_v2_niad$hapa5)[,1]

app_v2_niad <- as.data.frame(app_v2_niad)

app_v2_niad <- app_v2_niad[, c("id", 
                               "group", 
                               "time",
                               "age_scaled",
                               "education",
                               "fam_comp", 
                               "hapa2_scaled", 
                               "hapa3_scaled", 
                               "hapa4", 
                               "hapa5_scaled", 
                               "comm_mean_scaled", 
                               "safe_mean_scaled", 
                               "age",
                               "hapa2", 
                               "hapa3", 
                               "hapa5", 
                               "comm_mean", 
                               "safe_mean")]

## ---- create-subsets

app_v2_niad_con_pre <- app_v2_niad[app_v2_niad$group == 0 & 
                                     app_v2_niad$time == 1, ]

app_v2_niad_con_post <- app_v2_niad[app_v2_niad$group == 0 & 
                                      app_v2_niad$time == 2, ]

app_v2_niad_int_pre <- app_v2_niad[app_v2_niad$group == 1 & 
                                     app_v2_niad$time == 1, ]

app_v2_niad_int_post <- app_v2_niad[app_v2_niad$group == 1 & 
                                      app_v2_niad$time == 2, ]

## ---- missing-percent

colSums(is.na(app_v2_niad)) / nrow(app_v2_niad) * 100

colSums(is.na(app_v2_niad_con_pre)) / nrow(app_v2_niad_con_pre) * 100

colSums(is.na(app_v2_niad_con_post)) / nrow(app_v2_niad_con_post) * 100

colSums(is.na(app_v2_niad_int_pre)) / nrow(app_v2_niad_int_pre) * 100

colSums(is.na(app_v2_niad_int_post)) / nrow(app_v2_niad_int_post) * 100

## ---- mean-outcome-vars-non-imputed

outcome_vars <- c("hapa2", "hapa3", "hapa5", "comm_mean", "safe_mean")

means_non_imputed <- 
  data.frame(
    variable = outcome_vars,
    mean_con_pre = sapply(app_v2_niad_con_pre[outcome_vars], function(x) mean(x, na.rm = TRUE)),
    sd_con_pre = sapply(app_v2_niad_con_pre[outcome_vars], function(x) sd(x, na.rm = TRUE)),
    mean_con_post = sapply(app_v2_niad_con_post[outcome_vars], function(x) mean(x, na.rm = TRUE)),
    sd_con_post = sapply(app_v2_niad_con_post[outcome_vars], function(x) sd(x, na.rm = TRUE)),
    mean_int_pre = sapply(app_v2_niad_int_pre[outcome_vars], function(x) mean(x, na.rm = TRUE)),
    sd_int_pre = sapply(app_v2_niad_int_pre[outcome_vars], function(x) sd(x, na.rm = TRUE)),
    mean_int_post = sapply(app_v2_niad_int_post[outcome_vars], function(x) mean(x, na.rm = TRUE)),
    sd_int_post = sapply(app_v2_niad_int_post[outcome_vars], function(x) sd(x, na.rm = TRUE))
  )
