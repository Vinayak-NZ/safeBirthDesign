## ---- check variation across imputations

sapply(
  data_imputed_output,
  function(x) mean(x$hapa2_scaled, na.rm = TRUE)
) |> summary()

sapply(
  data_imputed_output,
  function(x) mean(x$hapa3_scaled, na.rm = TRUE)
) |> summary()

sapply(
  data_imputed_output,
  function(x) mean(x$hapa5_scaled, na.rm = TRUE)
) |> summary()
