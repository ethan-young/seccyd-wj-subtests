table3 <- 
  secondary_data2 |> 
  select(id, incnt_sd, incnt_sigma, incnt_pc, incnt_cv) |> 
  distinct() |> 
  left_join(primary_data |> select(id, incnt_mean) |> distinct()) |> 
  select(incnt_mean, incnt_sd, incnt_sigma, incnt_pc, incnt_cv) |> 
  corr_table(
    numbered = T,
    c.names = 
      c(
        "Mean",
        "Standard Deviation", 
        "Residual Standard Deviation", 
        "Average Percent Change",
        "Coefficient of Variation"
      ),
    stats = c("n", "mean", "sd", "min", "median", "max")
  )
