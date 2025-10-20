## ---- create-flow-chart

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