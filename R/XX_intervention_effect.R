# select observations relevant to v2

baseline <- 
  app_v1_v2_baseline_01[as.Date(app_v1_v2_baseline_01$datetime) > 
                          as.Date('2022-07-31'), ]

# select pregnant women
baseline_preg <- 
  baseline[baseline$Gruppe_Allgemein == 1, ]

# tidy up ID var
baseline_preg$UserCode <- ifelse(baseline_preg$UserCode_4_Sch_IG < 0 & 
                                           baseline_preg$Code_schw_KG_ges < 0, 
                                         NA, 
                                         ifelse(baseline_preg$UserCode_4_Sch_IG < 0 & 
                                                  baseline_preg$Code_schw_KG_ges > 0, 
                                                baseline_preg$Code_schw_KG_ges, 
                                                baseline_preg$UserCode_4_Sch_IG))

baseline_preg$UserCode <- toupper(baseline_preg$UserCode)

# tidy up randomisation var
baseline_preg$group <- ifelse(baseline_preg$c_0001 == 1, 1, 0)

# tidy up demographic vars
baseline_preg$age <- ifelse(baseline_preg$Alter_Sch_IG < 0 & 
                                        baseline_preg$Alter_Sch_KG < 0, 
                                      NA, 
                                      ifelse(baseline_preg$Alter_Sch_IG < 0 & 
                                               baseline_preg$Alter_Sch_KG > 0, 
                                             baseline_preg$Alter_Sch_KG, 
                                             baseline_preg$Alter_Sch_IG))

baseline_preg$education <- ifelse(baseline_preg$Ausbildung_Sch_IG < 0 & 
                                             baseline_preg$Ausbildung_Sch_KG< 0, 
                                           NA, 
                                           ifelse(baseline_preg$Ausbildung_Sch_IG < 0 & 
                                                    baseline_preg$Ausbildung_Sch_KG > 0, 
                                                  baseline_preg$Ausbildung_Sch_KG, 
                                                  baseline_preg$Ausbildung_Sch_IG))

baseline_preg$fam_comp <- ifelse(baseline_preg$Familie_Sch_IG < 0 & 
                                          baseline_preg$dupl1_Familie_Sch_IG < 0, 
                                        NA, 
                                        ifelse(baseline_preg$Familie_Sch_IG < 0 & 
                                                 baseline_preg$dupl1_Familie_Sch_IG > 0, 
                                               baseline_preg$dupl1_Familie_Sch_IG, 
                                               baseline_preg$Familie_Sch_IG))

baseline_preg$age <- ifelse(baseline_preg$age > 0, 
                                 baseline_preg$age, NA)

baseline_preg$education <- ifelse(baseline_preg$education > 0, 
                                 baseline_preg$education, NA)

baseline_preg$fam_comp <- ifelse(baseline_preg$fam_comp > 0, 
                                 baseline_preg$fam_comp, NA)

# tidy up HAPA vars
baseline_preg$hapa2_t0 <- ifelse(baseline_preg$OE2_Sch_IG < 0 & 
                                     baseline_preg$OE2_Schw_KG< 0, 
                                   NA, 
                                   ifelse(baseline_preg$OE2_Sch_IG < 0 & 
                                            baseline_preg$OE2_Schw_KG > 0, 
                                          baseline_preg$OE2_Schw_KG, 
                                          baseline_preg$OE2_Sch_IG))

baseline_preg$hapa3_t0 <- ifelse(baseline_preg$CSE2_Sch_IG < 0 & 
                                      baseline_preg$CSE2_Schw_KG< 0, 
                                    NA, 
                                    ifelse(baseline_preg$CSE2_Sch_IG < 0 & 
                                             baseline_preg$CSE2_Schw_KG > 0, 
                                           baseline_preg$CSE2_Schw_KG, 
                                           baseline_preg$CSE2_Sch_IG))

baseline_preg$hapa4_t0 <- ifelse(baseline_preg$Int_Sch_IG < 0 & 
                                      baseline_preg$INT_Schw_KG < 0, 
                                    NA, 
                                    ifelse(baseline_preg$Int_Sch_IG < 0 & 
                                             baseline_preg$INT_Schw_KG > 0, 
                                           baseline_preg$INT_Schw_KG, 
                                           baseline_preg$Int_Sch_IG))

baseline_preg$hapa5_t0 <- ifelse(baseline_preg$PL_Sch_IG < 0 & 
                                     baseline_preg$PL_Schw_KG < 0, 
                                   NA, 
                                   ifelse(baseline_preg$PL_Sch_IG < 0 & 
                                            baseline_preg$PL_Schw_KG > 0, 
                                          baseline_preg$PL_Schw_KG, 
                                          baseline_preg$PL_Sch_IG))

baseline_preg$safe1_t0 <- ifelse(baseline_preg$VUEIC1_Sch_IG < 0 & 
                                         baseline_preg$VUEIC1_Sch_KG < 0, 
                                       NA, 
                                       ifelse(baseline_preg$VUEIC1_Sch_IG < 0 & 
                                                baseline_preg$VUEIC1_Sch_KG > 0, 
                                              baseline_preg$VUEIC1_Sch_KG, 
                                              baseline_preg$VUEIC1_Sch_IG))

baseline_preg$safe2_t0 <- ifelse(baseline_preg$VUEIC2_Sch_IG < 0 & 
                                         baseline_preg$VUEIC2_Sch_KG < 0, 
                                       NA, 
                                       ifelse(baseline_preg$VUEIC2_Sch_IG < 0 & 
                                                baseline_preg$VUEIC2_Sch_KG > 0, 
                                              baseline_preg$VUEIC2_Sch_KG, 
                                              baseline_preg$VUEIC2_Sch_IG))


# tidy up comm vars
baseline_preg$comm1_t0 <- ifelse(baseline_preg$COMM1_Sch_IG < 0 & 
                                   baseline_preg$COMM1_Sch_KG < 0, 
                                   NA, 
                                   ifelse(baseline_preg$COMM1_Sch_IG < 0 & 
                                            baseline_preg$COMM1_Sch_KG > 0, 
                                          baseline_preg$COMM1_Sch_KG, 
                                          baseline_preg$COMM1_Sch_IG))

baseline_preg$comm2_t0 <- ifelse(baseline_preg$COMM2_Sch_IG < 0 & 
                                   baseline_preg$COMM2_Sch_KG < 0, 
                                 NA, 
                                 ifelse(baseline_preg$COMM2_Sch_IG < 0 & 
                                          baseline_preg$COMM2_Sch_KG > 0, 
                                        baseline_preg$COMM2_Sch_KG, 
                                        baseline_preg$COMM2_Sch_IG))

baseline_preg$comm3_t0 <- ifelse(baseline_preg$COMM3_Sch_IG < 0 & 
                                   baseline_preg$COMM3_Sch_KG < 0, 
                                 NA, 
                                 ifelse(baseline_preg$COMM3_Sch_IG < 0 & 
                                          baseline_preg$COMM3_Sch_KG > 0, 
                                        baseline_preg$COMM3_Sch_KG, 
                                        baseline_preg$COMM3_Sch_IG))

baseline_preg$comm4_t0 <- ifelse(baseline_preg$COMM4_Sch_IG < 0 & 
                                   baseline_preg$COMM4_Sch_KG < 0, 
                                 NA, 
                                 ifelse(baseline_preg$COMM4_Sch_IG < 0 & 
                                          baseline_preg$COMM4_Sch_KG > 0, 
                                        baseline_preg$COMM4_Sch_KG, 
                                        baseline_preg$COMM4_Sch_IG))

baseline_preg$comm5_t0 <- ifelse(baseline_preg$COMM5_Sch_IG < 0 & 
                                   baseline_preg$COMM5_Sch_KG < 0, 
                                 NA, 
                                 ifelse(baseline_preg$COMM5_Sch_IG < 0 & 
                                          baseline_preg$COMM5_Sch_KG > 0, 
                                        baseline_preg$COMM5_Sch_KG, 
                                        baseline_preg$COMM5_Sch_IG))

baseline_preg$comm6_t0 <- ifelse(baseline_preg$COMM6_Sch_IG < 0 & 
                                   baseline_preg$COMM6_Sch_KG < 0, 
                                 NA, 
                                 ifelse(baseline_preg$COMM6_Sch_IG < 0 & 
                                          baseline_preg$COMM6_Sch_KG > 0, 
                                        baseline_preg$COMM6_Sch_KG, 
                                        baseline_preg$COMM6_Sch_IG))

baseline_preg$comm7_t0 <- ifelse(baseline_preg$COMM7_Sch_IG < 0 & 
                                   baseline_preg$COMM7_Sch_KG < 0, 
                                 NA, 
                                 ifelse(baseline_preg$COMM7_Sch_IG < 0 & 
                                          baseline_preg$COMM7_Sch_KG > 0, 
                                        baseline_preg$COMM7_Sch_KG, 
                                        baseline_preg$COMM7_Sch_IG))

# Create a baseline dataset with ID vars, Group, COMM1-7

baseline_preg <- 
  baseline_preg[, c("UserCode", 
                    "group",
                    "age", 
                    "education", 
                    "fam_comp", 
                    "hapa2_t0", 
                    "hapa3_t0", 
                    "hapa4_t0", 
                    "hapa5_t0", 
                    "safe1_t0", 
                    "safe2_t0", 
                    "comm1_t0", 
                    "comm2_t0", 
                    "comm3_t0", 
                    "comm4_t0", 
                    "comm5_t0", 
                    "comm6_t0", 
                    "comm7_t0")]

# v2 active data

v2_comm_variable_list <- 
  c("id", 
    "^COMM1_v", 
    "^COMM2_v", 
    "^COMM3_v", 
    "^COMM4_v", 
    "^COMM5_v", 
    "^COMM6_v", 
    "^COMM7_v")

app_v2_active_subset_comm <- 
  app_v2_active_data[, grep(paste(v2_comm_variable_list, collapse="|"), 
                            names(app_v2_active_data), value = TRUE)]

names(app_v2_active_subset_comm)[2:ncol(app_v2_active_subset_comm)] <- 
  tolower(names(app_v2_active_subset_comm)[2:ncol(app_v2_active_subset_comm)])

## ---- rename-comm

app_v2_active_subset_comm <- comm_rename(app_v2_active_subset_comm)

## ---- remove-redundancies-comm

app_v2_comm_constructs <- remove_redundancy(app_v2_active_subset_comm, 
                                            comm_constructs)

## ---- adopt-most-recent-score-comm
comm_con_list <- c("comm1", 
                   "comm2", 
                   "comm3", 
                   "comm4", 
                   "comm5", 
                   "comm6", 
                   "comm7")

app_v2_comm_list <- lapply(comm_con_list, 
                           tidy_con, 
                           data = app_v2_comm_constructs, 
                           time = 4)

app_v2_comm_modified <- Reduce(function(x, y) merge(x, y, by = "id"), 
                               app_v2_comm_list) 

app_v2_comm_list_rev <- lapply(comm_con_list, 
                               score_edit_multiple, 
                               data = app_v2_comm_modified, 
                               time = 4)

app_v2_comm_clean <- Reduce(function(x, y) merge(x, y, by = "id"), 
                            app_v2_comm_list_rev)

# hapa-cleaning

v2_hapa_variable_list <- 
  c("id", 
    "^RiskPer_v", 
    "^OE_", 
    "^CSE_v", 
    "^Int_v", 
    "^PL_v")

app_v2_active_subset_hapa <- 
  app_v2_active_data[, grep(paste(v2_hapa_variable_list, collapse="|"), 
                            names(app_v2_active_data), value = TRUE)]

names(app_v2_active_subset_hapa)[2:ncol(app_v2_active_subset_hapa)] <- 
  tolower(names(app_v2_active_subset_hapa)[2:ncol(app_v2_active_subset_hapa)])

app_v2_active_subset_hapa <- hapa_rename(app_v2_active_subset_hapa)

app_v2_hapa_constructs <- remove_redundancy(app_v2_active_subset_hapa, 
                                            hapa_constructs)

hapa_con_list <- c("hapa1", "hapa2", "hapa3", "hapa4", "hapa5")

app_v2_hapa_list <- lapply(hapa_con_list, 
                           tidy_con, 
                           data = app_v2_hapa_constructs, 
                           time = 4)

app_v2_hapa_modified <- Reduce(function(x, y) merge(x, y, by = "id"), 
                               app_v2_hapa_list) 

app_v2_hapa_list_rev <- lapply(hapa_con_list, 
                               score_edit_multiple, 
                               data = app_v2_hapa_modified, 
                               time = 4)

app_v2_hapa_clean <- Reduce(function(x, y) merge(x, y, by = "id"), 
                            app_v2_hapa_list_rev)

# safety-cleaning

v2_safety_variable_list <- 
  c("id", 
    "^VUEIC1_v1", 
    "^VUEIC2_v1")

app_v2_active_subset_safe <- 
  app_v2_active_data[, grep(paste(v2_safety_variable_list, collapse="|"), 
                            names(app_v2_active_data), value = TRUE)]

names(app_v2_active_subset_safe)[2:ncol(app_v2_active_subset_safe)] <- 
  tolower(names(app_v2_active_subset_safe)[2:ncol(app_v2_active_subset_safe)])

app_v2_active_subset_safety <- safety_rename(app_v2_active_subset_safe)

app_v2_safety_constructs <- remove_redundancy(app_v2_active_subset_safety, 
                                              safey_constructs)

safety_con_list <- c("safe1", "safe2")

app_v2_safety_list <- lapply(safety_con_list, 
                             tidy_con,
                             data = app_v2_safety_constructs, 
                             time = 1)

app_v2_safety_modified <- Reduce(function(x, y) merge(x, y, by = "id"), 
                                 app_v2_safety_list)

app_v2_safety_list_rev <- lapply(safety_con_list, 
                                 score_edit_multiple, 
                                 data = app_v2_safety_modified, 
                                 time = 1)

app_v2_safety_clean <- Reduce(function(x, y) merge(x, y, by = "id"), 
                              app_v2_safety_list_rev) 


# merge comm-hapa-safety-vars

app_v2_hapa_safety <- merge(app_v2_hapa_clean, 
                            app_v2_safety_clean, 
                            by = "id")

app_v2_hapa_safety_comm <- merge(app_v2_hapa_safety, 
                                 app_v2_comm_clean, 
                              by = "id")

# Create in-app-v2 data

app_v2_core <- app_v2_active_data[, c("id", "UserCode")]

app_v2_complete_data <- merge(app_v2_hapa_safety_comm, 
                              app_v2_core, 
                              by = "id", 
                              all.x = TRUE)

# Merge in-app with baseline

app_v2 <- merge(baseline_preg, 
                app_v2_complete_data, 
                by = "UserCode")

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
  "safe1_t1", "safe2_t1",
  "comm1_t1", "comm1_t2", "comm1_t3", "comm1_t4",
  "comm2_t1", "comm2_t2", "comm2_t3", "comm2_t4",
  "comm3_t1", "comm3_t2", "comm3_t3", "comm3_t4",
  "comm4_t1", "comm4_t2", "comm4_t3", "comm4_t4",
  "comm5_t1", "comm5_t2", "comm5_t3", "comm5_t4",
  "comm6_t1", "comm6_t2", "comm6_t3", "comm6_t4",
  "comm7_t1", "comm7_t2", "comm7_t3", "comm7_t4"
)]


## ---- remove-test-cases
app_v2 <- app_v2[
  !(grepl("TEST", app_v2$UserCode, fixed = TRUE)),
]

app_v2_int <- app_v2[app_v2$group == 1, ]

app_v2_con <- app_v2[app_v2$group == 0, c("UserCode", 
                                          "age", 
                                          "education", 
                                          "fam_comp", 
                                          grep("(_t0$|_t1$)", 
                                               names(app_v2), value = TRUE))]


# Exclude first column (UserCode)
app_v2_int_mice <- app_v2_int[, -c(1, 2)]

app_v2_con_mice <- app_v2_con[, -1]

# Run MICE with default methods
imp_int <- mice(app_v2_int_mice, m = 5, maxit = 5, seed = 123)

imp_con <- mice(app_v2_con_mice, m = 5, maxit = 5, seed = 123)

# Get back one completed dataset
app_v2_completed_int <- complete(imp_int, 1)

app_v2_completed_con <- complete(imp_con, 1)

app_v2_completed_int$UserCode <- app_v2_int$UserCode

app_v2_completed_con$UserCode <- app_v2_con$UserCode

app_v2_completed_int$group <- 1

app_v2_completed_con$group <- 0

app_v2_completed_con <- app_v2_completed_con[, c(
  "UserCode", "group", "age", "education", "fam_comp",
  "hapa2_t0", "hapa3_t0", "hapa4_t0", "hapa5_t0",
  "safe1_t0", "safe2_t0",
  "comm1_t0", "comm2_t0", "comm3_t0", "comm4_t0", 
  "comm5_t0", "comm6_t0", "comm7_t0",
  "hapa1_t1", "hapa2_t1", "hapa3_t1", "hapa4_t1", "hapa5_t1",
  "safe1_t1", "safe2_t1",
  "comm1_t1", "comm2_t1", "comm3_t1", "comm4_t1", 
  "comm5_t1", "comm6_t1", "comm7_t1"
)]

app_v2_completed_int <- app_v2_completed_int[, c(
  "UserCode", "group", "age", "education", "fam_comp",
  "hapa2_t0", "hapa3_t0", "hapa4_t0", "hapa5_t0",
  "safe1_t0", "safe2_t0",
  "comm1_t0", "comm2_t0", "comm3_t0", "comm4_t0", 
  "comm5_t0", "comm6_t0", "comm7_t0",
  "hapa1_t4", "hapa2_t4", "hapa3_t4", "hapa4_t4", "hapa5_t4",
  "safe1_t1", "safe2_t1",
  "comm1_t4", "comm2_t4", "comm3_t4", "comm4_t4", 
  "comm5_t4", "comm6_t4", "comm7_t4"
)]

app_v2_completed_con$comm_mean_pre <- rowMeans(
  app_v2_completed_con[, paste0("comm", 1:7, "_t0")],
  na.rm = TRUE
)

app_v2_completed_con$comm_mean_post <- rowMeans(
  app_v2_completed_con[, paste0("comm", 1:7, "_t1")],
  na.rm = TRUE
)

app_v2_completed_int$comm_mean_pre <- rowMeans(
  app_v2_completed_int[, paste0("comm", 1:7, "_t0")],
  na.rm = TRUE
)

app_v2_completed_int$comm_mean_post <- rowMeans(
  app_v2_completed_int[, paste0("comm", 1:7, "_t4")],
  na.rm = TRUE
)

test_con <- app_v2_completed_con[, c("UserCode", "group", "comm_mean_pre", "comm_mean_post")]

test_int <- app_v2_completed_int[, c("UserCode", "group", "comm_mean_pre", "comm_mean_post")]

app_v2_imputed <- rbind(test_con, test_int)

app_v2_imputed$diff <- app_v2_imputed$comm_mean_post - app_v2_imputed$comm_mean_pre

t.test(diff ~ group, data = app_v2_imputed)

# rescale baseline comms to 0 to 100

# get average comm scores 