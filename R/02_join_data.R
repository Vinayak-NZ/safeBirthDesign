## ---- join-data

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

