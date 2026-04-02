## ---- sensitivity-check-prepare

data_imputed_output <- vector("list", data_imputed$m)

for (i in 1:data_imputed$m) {
  data_imputed_output[[i]] <- complete(data_imputed, i)
}

comm_post_vars <- paste0("comm", 1:7, "_t4")
oe_post_var    <- "hapa2_t4"
cse_post_var   <- "hapa3_t4"
plan_post_var  <- "hapa5_t4"
safe_post_vars <- c("safe1_t2", "safe2_t2")

apply_delta_to_imputed <- function(data_completed,
                                   original_data,
                                   vars,
                                   delta,
                                   group_var = "group",
                                   intervention_code = 1) {
  
  ig_rows <- data_completed[[group_var]] == intervention_code
  
  for (v in vars) {
    originally_missing <- is.na(original_data[[v]])
    rows_to_adjust <- ig_rows & originally_missing
    
    data_completed[rows_to_adjust, v] <- 
      data_completed[rows_to_adjust, v] + delta
  }
  
  data_completed
}

deltas_positive <- c(0, -2, -5, -10)

deltas_safety <- c(0, 2, 5, 10)

sensitivity_comm <- list()

for (d in deltas_positive) {
  tmp <- vector("list", length(data_imputed_output))
  
  for (i in seq_along(data_imputed_output)) {
    tmp[[i]] <- apply_delta_to_imputed(
      data_completed = data_imputed_output[[i]],
      original_data = app_v2,
      vars = comm_post_vars,
      delta = d,
      group_var = "group",
      intervention_code = 1
    )
  }
  
  sensitivity_comm[[paste0("delta_", d)]] <- tmp
}

sensitivity_hapa2 <- list()

for (d in deltas_positive) {
  tmp <- vector("list", length(data_imputed_output))
  
  for (i in seq_along(data_imputed_output)) {
    tmp[[i]] <- apply_delta_to_imputed(
      data_completed = data_imputed_output[[i]],
      original_data = app_v2,
      vars = oe_post_var,
      delta = d,
      group_var = "group",
      intervention_code = 1
    )
  }
  
  sensitivity_hapa2[[paste0("delta_", d)]] <- tmp
}

sensitivity_hapa3 <- list()

for (d in deltas_positive) {
  tmp <- vector("list", length(data_imputed_output))
  
  for (i in seq_along(data_imputed_output)) {
    tmp[[i]] <- apply_delta_to_imputed(
      data_completed = data_imputed_output[[i]],
      original_data = app_v2,
      vars = cse_post_var,
      delta = d,
      group_var = "group",
      intervention_code = 1
    )
  }
  
  sensitivity_hapa3[[paste0("delta_", d)]] <- tmp
}

sensitivity_hapa5 <- list()

for (d in deltas_positive) {
  tmp <- vector("list", length(data_imputed_output))
  
  for (i in seq_along(data_imputed_output)) {
    tmp[[i]] <- apply_delta_to_imputed(
      data_completed = data_imputed_output[[i]],
      original_data = app_v2,
      vars = plan_post_var,
      delta = d,
      group_var = "group",
      intervention_code = 1
    )
  }
  
  sensitivity_hapa5[[paste0("delta_", d)]] <- tmp
}

sensitivity_safe <- list()

for (d in deltas_safety) {
  tmp <- vector("list", length(data_imputed_output))
  
  for (i in seq_along(data_imputed_output)) {
    tmp[[i]] <- apply_delta_to_imputed(
      data_completed = data_imputed_output[[i]],
      original_data = app_v2,
      vars = safe_post_vars,
      delta = d,
      group_var = "group",
      intervention_code = 1
    )
  }
  
  sensitivity_safe[[paste0("delta_", d)]] <- tmp
}

prepare_analysis_data <- function(imputed_list) {
  out <- vector("list", length(imputed_list))
  
  vars <- c("comm1","comm2","comm3","comm4","comm5","comm6","comm7",
            "hapa2","hapa3","hapa4","hapa5",
            "safe1","safe2")
  
  tx_post <- function(var, data) {
    if (var %in% c("safe1", "safe2")) {
      data[[paste0(var, "_post")]] <- ifelse(
        data[["group"]] == 0,
        data[[paste0(var, "_t1")]],
        data[[paste0(var, "_t2")]]
      )
    } else {
      data[[paste0(var, "_post")]] <- ifelse(
        data[["group"]] == 0,
        data[[paste0(var, "_t1")]],
        data[[paste0(var, "_t4")]]
      )
    }
    data
  }
  
  for (i in seq_along(imputed_list)) {
    dat <- imputed_list[[i]]
    
    names(dat) <- sub("_t0$", "_pre", names(dat))
    pre_vars <- grep("_pre$", names(dat), value = TRUE)
    
    dat <- Reduce(function(d, v) tx_post(v, d), vars, init = dat)
    
    dat[pre_vars] <- lapply(dat[pre_vars], function(x) {
      x[x == 0] <- 1
      ((x - 1) / 5) * 100
    })
    
    dat$comm_mean_pre  <- rowMeans(dat[, paste0("comm", 1:7, "_pre")],  na.rm = TRUE)
    dat$comm_mean_post <- rowMeans(dat[, paste0("comm", 1:7, "_post")], na.rm = TRUE)
    
    dat$safe_mean_pre  <- rowMeans(dat[, paste0("safe", 1:2, "_pre")],  na.rm = TRUE)
    dat$safe_mean_post <- rowMeans(dat[, paste0("safe", 1:2, "_post")], na.rm = TRUE)
    
    data.table::setDT(dat)
    
    dat <- data.table::melt(
      dat,
      id.vars = c("id", "group", "age", "education", "fam_comp"),
      measure.vars = list(
        c("hapa2_pre", "hapa2_post"),
        c("hapa3_pre", "hapa3_post"),
        c("hapa5_pre", "hapa5_post"),
        c("comm_mean_pre", "comm_mean_post"),
        c("safe_mean_pre", "safe_mean_post")
      ),
      variable.name = "time",
      value.name = c("hapa2", "hapa3", "hapa5", "comm_mean", "safe_mean")
    )
    
    dat$time <- factor(dat$time, levels = c(1, 2))
    dat$comm_mean_scaled <- scale(dat$comm_mean)[,1]
    dat$safe_mean_scaled <- scale(dat$safe_mean)[,1]
    dat$hapa2_scaled <- scale(dat$hapa2)[,1]
    dat$hapa3_scaled <- scale(dat$hapa3)[,1]
    dat$hapa5_scaled <- scale(dat$hapa5)[,1]
    dat$age_scaled <- scale(dat$age)[,1]
    
    out[[i]] <- as.data.frame(dat)
  }
  
  dplyr::bind_rows(out)
}

