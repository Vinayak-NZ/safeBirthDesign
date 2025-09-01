## ---- process-in-app-data

# rename-variables-id

names(app_v2_active_data)[names(app_v2_active_data) == "UUID"] <- "id"

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



# rename-variables-comm

app_v2_active_subset_comm <- comm_rename(app_v2_active_subset_comm)


app_v2_comm_constructs <- remove_redundancy(app_v2_active_subset_comm, 
                                            comm_constructs)

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

# rename-variables-hapa

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

# rename-variables-safety

v2_safety_variable_list <- 
  c("id", 
    "^VUEIC1_v", 
    "^VUEIC2_v")

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
                             time = 2)

app_v2_safety_modified <- Reduce(function(x, y) merge(x, y, by = "id"), 
                                 app_v2_safety_list)

app_v2_safety_list_rev <- lapply(safety_con_list, 
                                 score_edit_multiple, 
                                 data = app_v2_safety_modified, 
                                 time = 2)

app_v2_safety_clean <- Reduce(function(x, y) merge(x, y, by = "id"), 
                              app_v2_safety_list_rev) 


