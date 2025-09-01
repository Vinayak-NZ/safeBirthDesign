library(miceadds)  # for datalist2mids

# You already built: data_imputed_output <- list(df1, df2, ..., dfm)
imp2 <- miceadds::datalist2mids(data_imputed_output)

fit_lmer <- with(
  imp2,
  lm(comm_mean_scaled ~ group*time + age_scaled + education + fam_comp)
)

pooled <- pool(fit_lmer)
summ   <- summary(pooled)
summ$fmi

how_many_imputations(fit_lmer)
how_many_imputations(fit_lmer, cv = .01)
