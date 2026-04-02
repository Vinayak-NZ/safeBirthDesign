## ---- sensitivity-checks-compile

extract_gxt <- function(fit) {
  s <- posterior_summary(
    fit,
    variable = "b_group1:time2",
    probs = c(0.025, 0.975)
  )
  
  data.frame(
    estimate = s[1, "Estimate"],
    l95 = s[1, "Q2.5"],
    u95 = s[1, "Q97.5"]
  )
}

comm_sensitivity_table <- do.call(
  rbind,
  lapply(names(sensitivity_comms), function(nm) {
    cbind(scenario = nm, extract_gxt(sensitivity_comms[[nm]]))
  })
)

safe_sensitivity_table <- do.call(
  rbind,
  lapply(names(sensitivity_safe), function(nm) {
    cbind(scenario = nm, extract_gxt(sensitivity_safe[[nm]]))
  })
)

hapa2_sensitivity_table <- do.call(
  rbind,
  lapply(names(sensitivity_hapa2), function(nm) {
    cbind(scenario = nm, extract_gxt(sensitivity_hapa2[[nm]]))
  })
)

hapa3_sensitivity_table <- do.call(
  rbind,
  lapply(names(sensitivity_hapa3), function(nm) {
    cbind(scenario = nm, extract_gxt(sensitivity_hapa3[[nm]]))
  })
)

hapa5_sensitivity_table <- do.call(
  rbind,
  lapply(names(sensitivity_hapa5), function(nm) {
    cbind(scenario = nm, extract_gxt(sensitivity_hapa5[[nm]]))
  })
)

comm_sensitivity_table

safe_sensitivity_table

hapa2_sensitivity_table

hapa3_sensitivity_table

hapa5_sensitivity_table
