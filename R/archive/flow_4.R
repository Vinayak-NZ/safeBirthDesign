get_mi_diagnostics <- function(data_list, outcome) {
  
  fits <- lapply(
    data_list,
    function(dat) {
      lm(
        as.formula(
          paste0(
            outcome,
            " ~ group*time + age_scaled + education + fam_comp"
          )
        ),
        data = dat
      )
    }
  )
  
  estimates <- sapply(
    fits,
    function(fit) coef(fit)["group1:time2"]
  )
  
  ses <- sapply(
    fits,
    function(fit) {
      summary(fit)$coefficients[
        "group1:time2",
        "Std. Error"
      ]
    }
  )
  
  M <- length(estimates)
  Q_bar <- mean(estimates)
  U_bar <- mean(ses^2)
  B <- var(estimates)
  T <- U_bar + (1 + 1/M) * B
  
  FMI <- ((1 + 1/M) * B) / T
  RIV <- ((1 + 1/M) * B) / U_bar
  
  data.frame(
    outcome = outcome,
    M = M,
    estimate = Q_bar,
    within_variance = U_bar,
    between_variance = B,
    total_variance = T,
    FMI = FMI,
    RIV = RIV
  )
}

fmi_diagnostics <- rbind(
  get_mi_diagnostics(data_imputed_output, "comm_mean_scaled"),
  get_mi_diagnostics(data_imputed_output, "safe_mean_scaled"),
  get_mi_diagnostics(data_imputed_output, "hapa2_scaled"),
  get_mi_diagnostics(data_imputed_output, "hapa3_scaled"),
  get_mi_diagnostics(data_imputed_output, "hapa5_scaled")
)

fmi_diagnostics
