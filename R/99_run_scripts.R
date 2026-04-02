## ---- prep-data
source("R/00_load_data.R")
source("R/00_load_models.R")
source("R/00_load_functions.R")
source("R/00_load_package.R")
source("R/01_format_baseline.R")
source("R/01_format_data_in_app.R")
source("R/02_join_data.R")
source("R/02_joined_data_prep.R")
source("R/02_non_imputed_analytic_dataset.R")

## ---- imputation
source("R/02_pre_impute_check.R")
source("R/03_impute_data.R")

## ---- analyse-data
source("R/04_post_impute_edits.R")
source("R/05_build_bayes_models.R")
source("R/05_interpret_bayes_models.R")

## ---- visualise-data
source("R/06_socio_dems.R")
source("R/06_create_flowchart_input.R")
source("R/06_output_flowchart.R")
source("R/06_visualise_bayes_output.R")

## ---- sensitivity-check
source("R/07_sensitivity_check_01.R")
source("R/07_sensitivity_check_02.R")
source("R/07_sensitivity_check_03.R")
