table3 <- 
  secondary_data |> 
  select(id, incnt_sd, incnt_sigma, incnt_pc) |> 
  distinct() |> 
  left_join(primary_data |> select(id, incnt_mean) |> distinct()) |> 
  select(incnt_mean, incnt_sd, incnt_sigma, incnt_pc) |> 
  corr_table(
    numbered = T,
    c.names = c("Mean", "Standard Deviation", "Residual Standard Deviation", "Average Percent Change"),
    stats = c("n", "mean", "sd", "min", "median", "max")
  )
