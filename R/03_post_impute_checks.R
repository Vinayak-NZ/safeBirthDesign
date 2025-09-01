## ---- post-imputation-check

# check-percent-imputed

imputation_table <- data.frame(
  variable    = colnames(data_imputed$data),
  n           = nrow(data_imputed$data),
  n_missing   = colSums(data_imputed$where),
  pct_missing = colMeans(data_imputed$where) * 100
)

imputation_table[order(-imputation_table$pct_missing), ]
