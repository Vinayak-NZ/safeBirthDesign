## ---- prior-sensitivity-tests

bayesian_model_comm_wide <- prior_sensitivity_test(
  data = data_imputed_output, 
  outcome = "comm_mean", 
  prior_type = "wide"
)

bayesian_model_comm_very_wide <- prior_sensitivity_test(
  data = data_imputed_output, 
  outcome = "comm_mean", 
  prior_type = "very wide"
)

bayesian_model_safe_wide <- prior_sensitivity_test(
  data = data_imputed_output, 
  outcome = "safe_mean", 
  prior_type = "wide"
)

bayesian_model_safe_very_wide <- prior_sensitivity_test(
  data = data_imputed_output, 
  outcome = "safe_mean", 
  prior_type = "very wide"
)

bayesian_model_hapa2_wide <- prior_sensitivity_test(
  data = data_imputed_output, 
  outcome = "hapa2", 
  prior_type = "wide"
)

bayesian_model_hapa2_very_wide <- prior_sensitivity_test(
  data = data_imputed_output, 
  outcome = "hapa2", 
  prior_type = "very wide"
)

bayesian_model_hapa3_wide <- prior_sensitivity_test(
  data = data_imputed_output, 
  outcome = "hapa3", 
  prior_type = "wide"
)

bayesian_model_hapa3_very_wide <- prior_sensitivity_test(
  data = data_imputed_output, 
  outcome = "hapa3", 
  prior_type = "very wide"
)

bayesian_model_hapa5_wide <- prior_sensitivity_test(
  data = data_imputed_output, 
  outcome = "hapa5", 
  prior_type = "wide"
)

bayesian_model_hapa5_very_wide <- prior_sensitivity_test(
  data = data_imputed_output, 
  outcome = "hapa5", 
  prior_type = "very wide"
)