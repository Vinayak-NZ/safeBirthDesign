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
        text = ifelse(id == 3, "t0\n Waitlist Control\n (4 week wait to get access to web-app)\n N = 324 (49.77%)", text),
        text = ifelse(id == 4, "t0\n Intervention\n (Immediate access to web-app)\n N = 327 (50.23%)", text),
        text = ifelse(id == 5, "t1\n App registration completed\n (Used as post-measure)\n n = 45 (13.89%)", text),
        text = ifelse(id == 6, "t1\n App registration completed\n n = 88 (26.91%)", text),
        text = ifelse(id == 7, "t2\n Module 1 completed\n n = 25 (55.56%)", text),
        text = ifelse(id == 8, "t2\n Module 1 completed\n n = 38 (43.18%)", text),
        text = ifelse(id == 9, "t3\n Module 2 completed\n n = 11 (44.00%)", text),
        text = ifelse(id == 10, "t3\n Module 2 completed\n n = 20 (52.63%)", text),
        text = ifelse(id == 11, "t4\n Module 3 completed\n n = 11 (100.00%)", text),
        text = ifelse(id == 12, "t4\n Module 3 completed\n (Used as post-measure)\n n = 13 (65.00%)", text)
      )
  ) |>
  fc_draw()