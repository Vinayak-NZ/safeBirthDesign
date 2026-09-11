# Load model
model_input <- readRDS("output/bayesian_model_hapa5_2026-09-08.rds")

# Packages
library(posterior)
library(brms)

# ---------------------------------------------------------
# Settings
# ---------------------------------------------------------

# ROPE limits
rope_lower <- -0.10
rope_upper <-  0.10

# Parameter of interest
parameter <- "b_group1:time2"


# ---------------------------------------------------------
# Extract posterior draws
# ---------------------------------------------------------

draws <- as_draws_matrix(model_input)

beta <- draws[, parameter]


# ---------------------------------------------------------
# Probability of Direction (PD)
# ---------------------------------------------------------

pd <- max(
  mean(beta > 0),
  mean(beta < 0)
)


# ---------------------------------------------------------
# Probability of ROPE (P(ROPE))
# ---------------------------------------------------------

p_rope <- mean(
  beta >= rope_lower &
    beta <= rope_upper
)


# ---------------------------------------------------------
# Bayesian R-squared
# ---------------------------------------------------------

r2_draws <- bayes_R2(
  model_input,
  summary = FALSE
)

r2_median <- median(r2_draws)
r2_lower  <- quantile(r2_draws, 0.025)
r2_upper  <- quantile(r2_draws, 0.975)


# ---------------------------------------------------------
# Print results
# ---------------------------------------------------------

cat("\nTreatment effect:", parameter, "\n")
cat("PD:", round(pd, 3), "\n")
cat("P(ROPE):", round(p_rope, 3), "\n")
cat("\nBayesian R²:\n")
cat("Median:", round(r2_median, 3), "\n")
cat("95% CrI:", round(r2_lower, 3), "to", round(r2_upper, 3), "\n")