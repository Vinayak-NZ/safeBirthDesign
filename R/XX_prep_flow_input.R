## ---- participant-flow-chart

# create-tidy-baseline-data

baseline_input_flow <- app_v1_v2_baseline_01

baseline_input_flow <- 
  baseline_input_flow[as.Date(app_v1_v2_baseline_01$datetime) > 
                        as.Date('2022-07-31'), ]


baseline_input_flow$UserCode <- ifelse(baseline_input_flow$UserCode_4_Sch_IG < 0 & 
                                   baseline_input_flow$Code_schw_KG_ges < 0, 
                                 NA, 
                                 ifelse(baseline_input_flow$UserCode_4_Sch_IG < 0 & 
                                          baseline_input_flow$Code_schw_KG_ges > 0, 
                                        baseline_input_flow$Code_schw_KG_ges, 
                                        baseline_input_flow$UserCode_4_Sch_IG))

baseline_input_flow$UserCode <- toupper(baseline_input_flow$UserCode)

baseline_input_flow <- baseline_input_flow[
  !(grepl("TEST", baseline_input_flow$UserCode, fixed = TRUE)),
]

c2 <- nrow(baseline_input_flow)

baseline_input_flow$randomisation_group <- 
  as.factor(ifelse(baseline_input_flow$c_0001 == 1, 1, 
         ifelse(baseline_input_flow$c_0001 == 2, 0, NA)))
