table2 <- 
  primary_data |>
  select(
    id,
    family_unp,
    incnt_mean,
    incnt_sd,
    neigh_harsh,
    neigh_unp,
  ) |> 
  distinct() |> 
  select(-id) |> 
  corr_table(
    numbered = T,
    stats = c("n", "mean", "sd", "min", "median", "max"),
    c.names = ivs_label |> 
      str_subset("Residual|Percent", T) |> 
      str_replace("Standard Deviation", "Family Income Variability") |> 
      str_replace("\\n", " "),
    sample_size = F
  )
