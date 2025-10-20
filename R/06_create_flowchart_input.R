## ---- participant-flow-chart-input

# filer to relevant set

baseline_input_flow <- app_v1_v2_baseline_01

baseline_input_flow <- 
  baseline_input_flow[as.Date(app_v1_v2_baseline_01$datetime) > 
                        as.Date('2022-07-31'), ]

# create user code

baseline_input_flow$UserCode <- 
  ifelse(baseline_input_flow$UserCode_4_Sch_IG < 0 & 
         baseline_input_flow$Code_schw_KG_ges < 0, 
       NA, 
       ifelse(baseline_input_flow$UserCode_4_Sch_IG < 0 & 
                baseline_input_flow$Code_schw_KG_ges > 0, 
              baseline_input_flow$Code_schw_KG_ges, 
              baseline_input_flow$UserCode_4_Sch_IG))

baseline_input_flow$UserCode <- 
  toupper(baseline_input_flow$UserCode)

# filter out TEST cases

baseline_input_flow <- baseline_input_flow[
  !(grepl("TEST", baseline_input_flow$UserCode, fixed = TRUE)),
]

# edit randomisation group variable

baseline_input_flow$randomisation_group <- 
  as.factor(ifelse(baseline_input_flow$c_0001 == 1, 1, 
         ifelse(baseline_input_flow$c_0001 == 2, 0, NA)))

## ---- flowchart-tibble

# total-consent-form

total_control <- 
  table(baseline_input_flow$randomisation_group)[[1]]

total_intervention <- 
  table(baseline_input_flow$randomisation_group)[[2]]

total <- 
  total_control + total_intervention

# app-registrations

app_baseline_control <- table(app_v2$group)[[1]]

app_baseline_int <- table(app_v2$group)[[2]]

# module-01-completed

mod_01_control <- nrow(
  app_v2[app_v2$group == 0 &
           !is.na(app_v2$comm1_t2) & 
           !is.na(app_v2$comm2_t2) & 
           !is.na(app_v2$comm3_t2) & 
           !is.na(app_v2$comm4_t2) & 
           !is.na(app_v2$comm5_t2) & 
           !is.na(app_v2$comm6_t2) & 
           !is.na(app_v2$comm7_t2), ])

mod_01_int <- nrow(
  app_v2[app_v2$group == 1 &
           !is.na(app_v2$comm1_t2) & 
           !is.na(app_v2$comm2_t2) & 
           !is.na(app_v2$comm3_t2) & 
           !is.na(app_v2$comm4_t2) & 
           !is.na(app_v2$comm5_t2) & 
           !is.na(app_v2$comm6_t2) & 
           !is.na(app_v2$comm7_t2), ])

# module-02-completed

mod_02_control <- nrow(
  app_v2[app_v2$group == 0 &
           !is.na(app_v2$comm1_t3) & 
           !is.na(app_v2$comm2_t3) & 
           !is.na(app_v2$comm3_t3) & 
           !is.na(app_v2$comm4_t3) & 
           !is.na(app_v2$comm5_t3) & 
           !is.na(app_v2$comm6_t3) & 
           !is.na(app_v2$comm7_t3), ])

mod_02_int <- nrow(
  app_v2[app_v2$group == 1 &
           !is.na(app_v2$comm1_t3) & 
           !is.na(app_v2$comm2_t3) & 
           !is.na(app_v2$comm3_t3) & 
           !is.na(app_v2$comm4_t3) & 
           !is.na(app_v2$comm5_t3) & 
           !is.na(app_v2$comm6_t3) & 
           !is.na(app_v2$comm7_t3), ])

# module-03-completed

mod_03_control <- nrow(
  app_v2[app_v2$group == 0 &
           !is.na(app_v2$comm1_t4) & 
           !is.na(app_v2$comm2_t4) & 
           !is.na(app_v2$comm3_t4) & 
           !is.na(app_v2$comm4_t4) & 
           !is.na(app_v2$comm5_t4) & 
           !is.na(app_v2$comm6_t4) & 
           !is.na(app_v2$comm7_t4), ])

mod_03_int <- nrow(
  app_v2[app_v2$group == 1 &
           !is.na(app_v2$comm1_t4) & 
           !is.na(app_v2$comm2_t4) & 
           !is.na(app_v2$comm3_t4) & 
           !is.na(app_v2$comm4_t4) & 
           !is.na(app_v2$comm5_t4) & 
           !is.na(app_v2$comm6_t4) & 
           !is.na(app_v2$comm7_t4), ])

# create-tibble

tibble::tibble(
  id = 1:total,
  group = c(rep("Control", total_control), 
            rep("Intervention", total_intervention)),
  app_baseline = c(
    rep("Yes", app_baseline_control), 
    rep("No", total_control - app_baseline_control),
    rep("Yes", app_baseline_int), 
    rep("No", total_intervention - app_baseline_int)
  ),
  app_mod_01 = c(
    rep("Yes", mod_01_control), 
    rep("No", total_control - mod_01_control), 
    rep("Yes", mod_01_int),
    rep("No", total_intervention - mod_01_int)
  ),
  app_mod_02 = c(
    rep("Yes", mod_02_control), 
    rep("No", total_control - mod_02_control), 
    rep("Yes", mod_02_int),
    rep("No", total_intervention - mod_02_int)
  ),
  app_mod_03 = c(
    rep("Yes", mod_03_control), 
    rep("No", total_control - mod_03_control), 
    rep("Yes", mod_03_int),
    rep("No", total_intervention - mod_03_int)
  )
) -> teambaby_webapp

teambaby_webapp |>
  mutate(group = factor(group,
                        ordered = TRUE,
                        levels = c("Control", "Intervention")
  )) -> teambaby_webapp