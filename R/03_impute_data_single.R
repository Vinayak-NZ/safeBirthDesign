## ---- imputation-single-data

# get-data

data_imputed_pooled <- merge_imputations(app_v2,
                                         data_imputed,
                                         ori = app_v2)

## ---- post-imputation-edit

vars <- c("comm1","comm2","comm3","comm4","comm5","comm6","comm7",
          "hapa2","hapa3","hapa4","hapa5",
          "safe1","safe2")

names(data_imputed_pooled) <- 
  sub("_t0$", "_pre", names(data_imputed_pooled))

pre_vars <- 
  grep("_pre$", names(data_imputed_pooled), value = TRUE)

data_imputed_pooled <- Reduce(function(d, v) tx_post(v, d), 
                                   vars, 
                                   init = data_imputed_pooled)

data_imputed_pooled[pre_vars] <- lapply(
  data_imputed_pooled[pre_vars],
  function(x) {
    x[x == 0] <- 1            
    ((x - 1) / 5) * 100      
  }
)

data_imputed_pooled$comm_mean_pre <- rowMeans(
  data_imputed_pooled[, paste0("comm", 1:7, "_pre")],
  na.rm = TRUE
)

data_imputed_pooled$comm_mean_post <- rowMeans(
  data_imputed_pooled[, paste0("comm", 1:7, "_post")],
  na.rm = TRUE
)

data_imputed_pooled$safe_mean_pre <- rowMeans(
  data_imputed_pooled[, paste0("safe", 1:2, "_pre")],
  na.rm = TRUE
)

data_imputed_pooled$safe_mean_post <- rowMeans(
  data_imputed_pooled[, paste0("safe", 1:2, "_post")],
  na.rm = TRUE
)

data_imputed_pooled <- 
  data_imputed_pooled[, !(names(data_imputed_pooled) %in% c("hapa1_post", 
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

setDT(data_imputed_pooled)

data_imputed_pooled <- melt(data_imputed_pooled, 
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

data_imputed_pooled$time <- 
  factor(data_imputed_pooled$time, 
         order = FALSE, 
         levels = c(1, 2))

data_imputed_pooled$comm_mean_scaled <- 
  scale(data_imputed_pooled$comm_mean)[,1]

data_imputed_pooled$safe_mean_scaled <- 
  scale(data_imputed_pooled$safe_mean)[,1]

data_imputed_pooled$age_scaled <- 
  scale(data_imputed_pooled$age)[,1]

data_imputed_pooled$hapa2_scaled <- 
  scale(data_imputed_pooled$hapa2)[,1]

data_imputed_pooled$hapa3_scaled <- 
  scale(data_imputed_pooled$hapa3)[,1]

data_imputed_pooled$hapa5_scaled <- 
  scale(data_imputed_pooled$hapa5)[,1]

data_imputed_pooled <- as.data.frame(data_imputed_pooled)

data_imputed_pooled$.id <- data_imputed_pooled$id

data_imputed_pooled$.imp <- i 

data_imputed_pooled <- data_imputed_pooled[, c(".imp", 
                                                         ".id", 
                                                         "id", 
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
                                                         "safe_mean_scaled")]


