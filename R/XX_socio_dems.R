## ---- socio-demographic-group

app_v2_dems <- app_v2

table(app_v2_dems$group)

## ---- socio-demographic-age

app_v2_dems$age_group <- 
  ifelse(is.na(app_v2_dems$age), NA, 
         ifelse(app_v2_dems$age < 20, 1, 
                ifelse(app_v2_dems$age >= 20 & app_v2_dems$age < 30, 2, 
                       ifelse(app_v2_dems$age >= 30 & app_v2_dems$age < 40, 3, 4))))

table(app_v2_dems$group, 
      app_v2_dems$age_group)

## ---- socio-demographic-fam-comp

table(app_v2_dems$group, 
      app_v2_dems$fam_comp)

## ---- socio-demographic-qual

table(app_v2_dems$group, 
      app_v2_dems$education)
