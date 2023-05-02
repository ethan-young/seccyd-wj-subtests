table2 <- 
  ivs_analysis |> 
  mutate(
    incnt_mean = incnt_mean * -1
  ) |> 
  select(
    family_unp,
    incnt_mean,
    incnt_sd,
    neigh_harsh,
    neigh_unp,
  ) |> 
  corr_table(
    numbered = T,
    stats = c("n", "mean", "sd", "min", "median", "max"),
    c.names = ivs_label |> str_subset("Residual|Percent", T),
    sample_size = F
  )
