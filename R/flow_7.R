model_input <- readRDS("output/bayesian_model_hapa5_2026-09-08.rds")

library(posterior)

# Number of imputations
m <- 122

# Convert brm_multiple object to posterior draws
draws <- as_draws_array(model_input)

# 4 chains per imputed dataset
nc <- nchains(model_input) / m

nc

# Split posterior draws into the 122 imputed datasets
draws_per_dat <- lapply(
  1:m,
  \(i) subset_draws(
    draws,
    chain = ((i - 1) * nc + 1):(i * nc)
  )
)

diagnostics_per_dat <- lapply(
  draws_per_dat,
  summarise_draws,
  default_convergence_measures()
)

saveRDS(diagnostics_per_dat, "output/model_output/bayesian_model_hapa5_output.rds")

# diagnostics_per_dat[[1]]
# 
# library(dplyr)
# 
# diagnostics_all <- bind_rows(
#   lapply(
#     seq_along(diagnostics_per_dat),
#     \(i) diagnostics_per_dat[[i]] |>
#       mutate(imputation = i)
#   )
# )
# 
# diagnostics_all |>
#   summarise(
#     max_Rhat = max(rhat, na.rm = TRUE),
#     min_bulk_ESS = min(ess_bulk, na.rm = TRUE),
#     min_tail_ESS = min(ess_tail, na.rm = TRUE)
#   )
# 
# diagnostics_all |>
#   summarise(
#     min_bulk_ESS = min(ess_bulk, na.rm = TRUE),
#     median_bulk_ESS = median(ess_bulk, na.rm = TRUE),
#     min_tail_ESS = min(ess_tail, na.rm = TRUE),
#     median_tail_ESS = median(ess_tail, na.rm = TRUE)
#   )



