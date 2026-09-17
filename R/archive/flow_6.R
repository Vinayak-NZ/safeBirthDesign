## ---- bayes-prior-sensitivity ----
##
## Prior sensitivity analysis
##
## Purpose:
## Assess whether posterior estimates for the intervention effect
## (group1:time2) are robust to alternative prior specifications.
##
## Only the prior for group1:time2 is changed.
## Model formula, imputed datasets, sampling settings, and controls
## remain identical to the primary analysis.
##

# ------------------------------------------------------------
# 1. Define sensitivity priors
# ------------------------------------------------------------

# Weakly informative priors
# Same neutral centre, substantially wider distribution
prior_wide <- brms::prior(
  "normal(0, 0.50)",
  class = "b",
  coef = "group1:time2"
)


# Very weakly informative / approximately neutral priors
prior_very_wide <- brms::prior(
  "normal(0, 1.00)",
  class = "b",
  coef = "group1:time2"
)


# ------------------------------------------------------------
# 2. Create output directory if necessary
# ------------------------------------------------------------

if (!dir.exists("output/prior_sensitivity")) {
  dir.create("output/prior_sensitivity", recursive = TRUE)
}


# ------------------------------------------------------------
# 3. Function to run sensitivity models
# ------------------------------------------------------------

run_sensitivity_model <- function(formula,
                                  prior,
                                  outcome_name,
                                  prior_name) {

  message(
    "Running: ",
    outcome_name,
    " | prior = ",
    prior_name
  )

  model <- brms::brm_multiple(
    formula,
    data = data_imputed_output,
    chains = 4,
    cores = 4,
    iter = 4000,
    warmup = 500,
    backend = "cmdstanr",
    control = list(
      adapt_delta = 0.95,
      max_treedepth = 15
    ),
    prior = prior
  )

  # Filename
  model_file <- paste0(
    "output/prior_sensitivity/",
    "bayesian_model_",
    outcome_name,
    "_",
    prior_name,
    "_",
    Sys.Date(),
    ".rds"
  )

  summary_file <- paste0(
    "output/prior_sensitivity/",
    "bayesian_model_",
    outcome_name,
    "_",
    prior_name,
    "_",
    Sys.Date(),
    ".txt"
  )

  # Save model
  saveRDS(model, file = model_file)

  # Save summary
  sink(summary_file)
  print(summary(model))
  sink()

  return(model)
}


# ------------------------------------------------------------
# 4. Model formulas
# ------------------------------------------------------------

formula_comm <-
  comm_mean_scaled ~ group*time +
  age_scaled + education + fam_comp + (1 | id)

formula_safe <-
  safe_mean_scaled ~ group*time +
  age_scaled + education + fam_comp + (1 | id)

formula_hapa2 <-
  hapa2_scaled ~ group*time +
  age_scaled + education + fam_comp + (1 | id)

formula_hapa3 <-
  hapa3_scaled ~ group*time +
  age_scaled + education + fam_comp + (1 | id)

formula_hapa5 <-
  hapa5_scaled ~ group*time +
  age_scaled + education + fam_comp + (1 | id)


# # ------------------------------------------------------------
# # 5. Run sensitivity models
# # ------------------------------------------------------------
# 
# # ---- Communication ----
# 
bayesian_model_comm_wide <- run_sensitivity_model(
  formula = formula_comm,
  prior = prior_wide,
  outcome_name = "comm",
  prior_name = "wide"
)

bayesian_model_comm_very_wide <- run_sensitivity_model(
  formula = formula_comm,
  prior = prior_very_wide,
  outcome_name = "comm",
  prior_name = "very_wide"
)


# ---- Safety ----

bayesian_model_safe_wide <- run_sensitivity_model(
  formula = formula_safe,
  prior = prior_wide,
  outcome_name = "safe",
  prior_name = "wide"
)

bayesian_model_safe_very_wide <- run_sensitivity_model(
  formula = formula_safe,
  prior = prior_very_wide,
  outcome_name = "safe",
  prior_name = "very_wide"
)


# ---- HAPA 2 ----

bayesian_model_hapa2_wide <- run_sensitivity_model(
  formula = formula_hapa2,
  prior = prior_wide,
  outcome_name = "hapa2",
  prior_name = "wide"
)

bayesian_model_hapa2_very_wide <- run_sensitivity_model(
  formula = formula_hapa2,
  prior = prior_very_wide,
  outcome_name = "hapa2",
  prior_name = "very_wide"
)


# ---- HAPA 3 ----

bayesian_model_hapa3_wide <- run_sensitivity_model(
  formula = formula_hapa3,
  prior = prior_wide,
  outcome_name = "hapa3",
  prior_name = "wide"
)

bayesian_model_hapa3_very_wide <- run_sensitivity_model(
  formula = formula_hapa3,
  prior = prior_very_wide,
  outcome_name = "hapa3",
  prior_name = "very_wide"
)


# ---- HAPA 5 ----

bayesian_model_hapa5_wide <- run_sensitivity_model(
  formula = formula_hapa5,
  prior = prior_wide,
  outcome_name = "hapa5",
  prior_name = "wide"
)

bayesian_model_hapa5_very_wide <- run_sensitivity_model(
  formula = formula_hapa5,
  prior = prior_very_wide,
  outcome_name = "hapa5",
  prior_name = "very_wide"
)


# ------------------------------------------------------------
# 6. Save session information
# ------------------------------------------------------------

writeLines(
  capture.output(sessionInfo()),
  con = paste0(
    "output/prior_sensitivity/sessionInfo_",
    Sys.Date(),
    ".txt"
  )
)