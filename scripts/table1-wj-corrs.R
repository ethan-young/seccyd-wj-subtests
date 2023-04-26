table1 <- 
  dvs_analysis_long |> 
  select(id, wj_subtest, mean_score) |> 
  pivot_wider(names_from = "wj_subtest", values_from = "mean_score") |> 
  select(-id) |> 
  corr_table(
    numbered = T,
    stats = c("n", "mean", "sd", "min", "median", "max"),
    c.names = wj_labels,
    sample_size = F
  )
