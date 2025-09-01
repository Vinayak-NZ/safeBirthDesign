## ---- tidy-input-data

# subset to key vars

app_v2 <- app_v2[, c(
  "UserCode", "group", "age", "education", "fam_comp",
  "hapa2_t0", "hapa3_t0", "hapa4_t0", "hapa5_t0",
  "safe1_t0", "safe2_t0", 
  "comm1_t0", "comm2_t0", "comm3_t0", "comm4_t0", 
  "comm5_t0", "comm6_t0", "comm7_t0",
  "hapa1_t1", "hapa1_t2", "hapa1_t3", "hapa1_t4",
  "hapa2_t1", "hapa2_t2", "hapa2_t3", "hapa2_t4",
  "hapa3_t1", "hapa3_t2", "hapa3_t3", "hapa3_t4",
  "hapa4_t1", "hapa4_t2", "hapa4_t3", "hapa4_t4",
  "hapa5_t1", "hapa5_t2", "hapa5_t3", "hapa5_t4",
  "safe1_t1", "safe2_t1", "safe1_t2", "safe2_t2", 
  "comm1_t1", "comm1_t2", "comm1_t3", "comm1_t4",
  "comm2_t1", "comm2_t2", "comm2_t3", "comm2_t4",
  "comm3_t1", "comm3_t2", "comm3_t3", "comm3_t4",
  "comm4_t1", "comm4_t2", "comm4_t3", "comm4_t4",
  "comm5_t1", "comm5_t2", "comm5_t3", "comm5_t4",
  "comm6_t1", "comm6_t2", "comm6_t3", "comm6_t4",
  "comm7_t1", "comm7_t2", "comm7_t3", "comm7_t4"
)]


names(app_v2)[names(app_v2) == "UserCode"] <- "id"

# remove test cases

app_v2 <- app_v2[
  !(grepl("TEST", app_v2$id, fixed = TRUE)),
]

# convert variable types

app_v2$group <- 
  factor(app_v2$group, 
         order = FALSE, 
         levels = c(0, 1))

app_v2$education <- 
  factor(app_v2$education, 
         order = TRUE, 
         levels = c(1, 2, 3, 4))

app_v2$fam_comp <- 
  factor(app_v2$fam_comp, 
         order = TRUE, 
         levels = c(1, 2, 3))