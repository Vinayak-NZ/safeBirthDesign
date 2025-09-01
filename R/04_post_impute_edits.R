## ---- prepare-mice-output

tx_post <- function(var, data){
  
  if (var %in% c("safe1", "safe2")) {
    
    data[[paste0(var, "_post")]] <- 
      ifelse(data[["group"]] == 0, data[[paste0(var, "_t1")]], 
             data[[paste0(var, "_t2")]])
    
  } else{
    
    data[[paste0(var, "_post")]] <- 
      ifelse(data[["group"]] == 0, data[[paste0(var, "_t1")]], 
             data[[paste0(var, "_t4")]])
    
  }
  
  return(data)
  
}

vars <- c("comm1","comm2","comm3","comm4","comm5","comm6","comm7",
          "hapa2","hapa3","hapa4","hapa5",
          "safe1","safe2")

for (i in 1:length(data_imputed_output)){
  
  names(data_imputed_output[[i]]) <- 
    sub("_t0$", "_pre", names(data_imputed_output[[i]]))
  
  pre_vars <- 
    grep("_pre$", names(data_imputed_output[[i]]), value = TRUE)
  
  data_imputed_output[[i]] <- Reduce(function(d, v) tx_post(v, d), 
                   vars, 
                   init = data_imputed_output[[i]])
  
  data_imputed_output[[i]][pre_vars] <- lapply(
    data_imputed_output[[i]][pre_vars],
    function(x) {
      x[x == 0] <- 1            
      ((x - 1) / 5) * 100      
    }
  )
  
  data_imputed_output[[i]]$comm_mean_pre <- rowMeans(
    data_imputed_output[[i]][, paste0("comm", 1:7, "_pre")],
    na.rm = TRUE
  )
  
  data_imputed_output[[i]]$comm_mean_post <- rowMeans(
    data_imputed_output[[i]][, paste0("comm", 1:7, "_post")],
    na.rm = TRUE
  )
  
  data_imputed_output[[i]]$safe_mean_pre <- rowMeans(
    data_imputed_output[[i]][, paste0("safe", 1:2, "_pre")],
    na.rm = TRUE
  )
  
  data_imputed_output[[i]]$safe_mean_post <- rowMeans(
    data_imputed_output[[i]][, paste0("safe", 1:2, "_post")],
    na.rm = TRUE
  )
  
  data_imputed_output[[i]] <- 
    data_imputed_output[[i]][, !(names(data_imputed_output[[i]]) %in% c("hapa1_post", 
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
  
  setDT(data_imputed_output[[i]])
  
  data_imputed_output[[i]] <- melt(data_imputed_output[[i]], 
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
  
  data_imputed_output[[i]]$time <- 
    factor(data_imputed_output[[i]]$time, 
           order = FALSE, 
           levels = c(1, 2))
  
  data_imputed_output[[i]]$comm_mean_scaled <- 
    scale(data_imputed_output[[i]]$comm_mean)[,1]
  
  data_imputed_output[[i]]$safe_mean_scaled <- 
    scale(data_imputed_output[[i]]$safe_mean)[,1]
  
  data_imputed_output[[i]]$age_scaled <- 
    scale(data_imputed_output[[i]]$age)[,1]
  
  data_imputed_output[[i]]$hapa3_scaled <- 
    scale(data_imputed_output[[i]]$hapa3)[,1]
  
  data_imputed_output[[i]]$hapa5_scaled <- 
    scale(data_imputed_output[[i]]$hapa5)[,1]
  
  data_imputed_output[[i]] <- as.data.frame(data_imputed_output[[i]])
  
  data_imputed_output[[i]]$.id <- data_imputed_output[[i]]$id
  
  data_imputed_output[[i]]$.imp <- i 
  
  data_imputed_output[[i]] <- data_imputed_output[[i]][, c(".imp", 
                                                           ".id", 
                                                           "id", 
                                                           "group", 
                                                           "time",
                                                           "age_scaled",
                                                           "education",
                                                           "fam_comp", 
                                                           "hapa2", 
                                                           "hapa3_scaled", 
                                                           "hapa4", 
                                                           "hapa5_scaled", 
                                                           "comm_mean_scaled", 
                                                           "safe_mean_scaled")]
  
}

