
fit_lmer_safety <- with(
  imp2,
  lm(safe_mean_scaled ~ group*time + age_scaled + education + fam_comp)
)

pooled_safety <- pool(fit_lmer_safety)

fit_lmer_comms <- with(
  imp2,
  lm(comm_mean_scaled ~ group*time + age_scaled + education + fam_comp)
)

pooled_comms <- pool(fit_lmer_comms)

fit_lmer_hapa2 <- with(
  imp2,
  lm(hapa2_scaled ~ group*time + age_scaled + education + fam_comp)
)

pooled_hapa2 <- pool(fit_lmer_hapa2)

fit_lmer_hapa3 <- with(
  imp2,
  lm(hapa3_scaled ~ group*time + age_scaled + education + fam_comp)
)

pooled_hapa3 <- pool(fit_lmer_hapa3)

fit_lmer_hapa5 <- with(
  imp2,
  lm(hapa5_scaled ~ group*time + age_scaled + education + fam_comp)
)

pooled_hapa5 <- pool(fit_lmer_hapa5)


summary(pooled_safety, type = "all")

summary(pooled_comms, type = "all")

summary(pooled_hapa2, type = "all")

summary(pooled_hapa3, type = "all")

summary(pooled_hapa5, type = "all")


# library(semTools)
# library(lavaan.mi)
# 
# fmi_vars <- c(
#   "comm_mean_scaled",
#   "safe_mean_scaled",
#   "hapa2_scaled",
#   "hapa3_scaled",
#   "hapa5_scaled",
#   "age_scaled"
# )
# 
# fmi_out <- semTools::fmi(
#   data_imputed_output,
#   method = "saturated",
#   varnames = fmi_vars
# )
# 
# fmi_out




