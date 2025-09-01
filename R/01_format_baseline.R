## ---- subset-data

# select observations relevant to v2

baseline <- 
  app_v1_v2_baseline_01[as.Date(app_v1_v2_baseline_01$datetime) > 
                          as.Date('2022-07-31'), ]

# select pregnant women

baseline_preg <- 
  baseline[baseline$Gruppe_Allgemein == 1, ]

## ---- tidy-variables

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

# Create a baseline data set with ID vars, Group, COMM1-7

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



