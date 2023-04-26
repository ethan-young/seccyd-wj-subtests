table2 <- 
  ivs_analysis |> 
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
    c.names = ivs_label,
    sample_size = F
  )
