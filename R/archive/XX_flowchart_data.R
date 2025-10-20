library(flowchart)
library(tidyverse)

total_control <- table(baseline_input_flow$randomisation_group)[[1]]

total_intervention <- table(baseline_input_flow$randomisation_group)[[2]]

total <- total_control + total_intervention

app_baseline_control <- table(app_v2$group)[[1]]

app_baseline_int <- table(app_v2$group)[[2]]

mod_01_control <- nrow(
  app_v2[app_v2$group == 0 &
        !is.na(app_v2$comm1_t2) & 
        !is.na(app_v2$comm2_t2) & 
        !is.na(app_v2$comm3_t2) & 
        !is.na(app_v2$comm4_t2) & 
        !is.na(app_v2$comm5_t2) & 
        !is.na(app_v2$comm6_t2) & 
        !is.na(app_v2$comm7_t2), ])

mod_02_control <- nrow(
  app_v2[app_v2$group == 0 &
        !is.na(app_v2$comm1_t3) & 
        !is.na(app_v2$comm2_t3) & 
        !is.na(app_v2$comm3_t3) & 
        !is.na(app_v2$comm4_t3) & 
        !is.na(app_v2$comm5_t3) & 
        !is.na(app_v2$comm6_t3) & 
        !is.na(app_v2$comm7_t3), ])

mod_03_control <- nrow(
  app_v2[app_v2$group == 0 &
        !is.na(app_v2$comm1_t4) & 
        !is.na(app_v2$comm2_t4) & 
        !is.na(app_v2$comm3_t4) & 
        !is.na(app_v2$comm4_t4) & 
        !is.na(app_v2$comm5_t4) & 
        !is.na(app_v2$comm6_t4) & 
        !is.na(app_v2$comm7_t4), ])

mod_01_int <- nrow(
  app_v2[app_v2$group == 1 &
           !is.na(app_v2$comm1_t2) & 
           !is.na(app_v2$comm2_t2) & 
           !is.na(app_v2$comm3_t2) & 
           !is.na(app_v2$comm4_t2) & 
           !is.na(app_v2$comm5_t2) & 
           !is.na(app_v2$comm6_t2) & 
           !is.na(app_v2$comm7_t2), ])

mod_02_int <- nrow(
  app_v2[app_v2$group == 1 &
           !is.na(app_v2$comm1_t3) & 
           !is.na(app_v2$comm2_t3) & 
           !is.na(app_v2$comm3_t3) & 
           !is.na(app_v2$comm4_t3) & 
           !is.na(app_v2$comm5_t3) & 
           !is.na(app_v2$comm6_t3) & 
           !is.na(app_v2$comm7_t3), ])

mod_03_int <- nrow(
  app_v2[app_v2$group == 1 &
           !is.na(app_v2$comm1_t4) & 
           !is.na(app_v2$comm2_t4) & 
           !is.na(app_v2$comm3_t4) & 
           !is.na(app_v2$comm4_t4) & 
           !is.na(app_v2$comm5_t4) & 
           !is.na(app_v2$comm6_t4) & 
           !is.na(app_v2$comm7_t4), ])

# Simulate the data
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

teambaby_webapp_fc <- teambaby_webapp |>
  as_fc(label = "Collection of informed consent") |>
  fc_filter(!is.na(group), label = "Randomised participants") |>
  fc_split(group) |>
  fc_filter(app_baseline == "Yes",
            label = "App registration completed") |>
  fc_filter(app_mod_01 == "Yes",
            label = "Module 1 completed") |>
  fc_filter(app_mod_02 == "Yes",
            label = "Module 2 completed") |>
  fc_filter(app_mod_03 == "Yes",
            label = "Module 3 completed")

teambaby_webapp_fc |>
  fc_modify(
    ~ . |>
      dplyr::mutate(
        bg_fill = ifelse(id == 5, "violet", bg_fill),
        bg_fill = ifelse(id == 12, "violet", bg_fill), 
        text = ifelse(id == 3, "Waitlist Control\n (4 week wait to get access to web-app)\n 324 (49.77%)", text),
        text = ifelse(id == 4, "Intervention\n (Immediate access to web-app)\n 327 (50.23%)", text),
        text = ifelse(id == 5, "App registration completed\n (Used as post-measure)\n 45 (13.89%)", text),
        text = ifelse(id == 12, "Module 3 completed\n (Used as post-measure)\n 13 (65.00%)", text)
      )
  ) |>
  fc_draw()

teambaby_webapp_fc |>
  fc_draw()
