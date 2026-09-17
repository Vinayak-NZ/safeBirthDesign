model_input <- readRDS("output/bayesian_model_hapa5_2026-09-08.rds")

library(posterior)
library(brms)

rope_lower <- -0.10

rope_upper <-  0.10

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