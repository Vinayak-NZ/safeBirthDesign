## ---- impute-data

pred <- make.predictorMatrix(app_v2)
meth <- make.method(app_v2)

pred[, "id"] <- 0
pred["id", ] <- 0
meth["id"]   <- ""

demo_vars <- c("group", "age", "education", "fam_comp")
meth[demo_vars] <- ""          
pred[demo_vars, ] <- 0         
pred[, demo_vars] <- 1         

data_imputed <- mice(
  app_v2, 
  m = 122, 
  maxit = 30, 
  seed = 555,
  predictorMatrix = pred,
  method = meth
)
