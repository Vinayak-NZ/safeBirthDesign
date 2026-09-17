## ---- Derive-FMI

comm_fmi <- fmi_rubin_rule(data = data_imputed_output, 
                           outcome_var = "comm_mean")

safe_fmi <- fmi_rubin_rule(data = data_imputed_output, 
                           outcome_var = "safe_mean")

hapa2_fmi <- fmi_rubin_rule(data = data_imputed_output, 
                           outcome_var = "hapa2")

hapa3_fmi <- fmi_rubin_rule(data = data_imputed_output, 
                           outcome_var = "hapa3")

hapa5_fmi <- fmi_rubin_rule(data = data_imputed_output, 
                           outcome_var = "hapa5")

