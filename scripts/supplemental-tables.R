stable_ivs1 <- 
  left_join(
    primary_data |> distinct(id),
    ivs_analysis
  ) |> 
  mutate(incnt_mean = incnt_mean * -1) |> 
  select(
    incnt_mean,
    neigh_harsh, matches("cen(.*)mean$"),
  ) |> 
  corr_table(
    numbered = T,
    stats = c("n", "mean", "sd", "min", "median", "max"),
    c.names = c("Family Income Disadvantage", "Neigh. Socioeconomic Disadvantage", 
                "Neigh. % Poverty", "Neigh. HH Income", "Neigh. Gini",
                "Neigh. % Renting", "Neigh. % Unemploy", 
                "Neigh. % No Degree")
  )

stable_ivs2 <- 
  left_join(
    secondary_data2 |> distinct(id, incnt_sd, incnt_sigma, incnt_pc, incnt_cv),
    ivs_analysis
  ) |> 
  select(
    family_unp, partner_changes, moves_n, job_changes_mean,
    incnt_sd, incnt_sigma, incnt_pc, incnt_cv,
    neigh_unp, matches("cen(.*)sd$")
  ) |> 
  corr_table(
    numbered = T,
    stats = c("n", "mean", "sd", "min", "median", "max"),
    c.names = c("Fam. Transitions", "Partner Changes", 
                "Job Changes", "Res. Moves",
                "Income (SD)", "Income (Residual SD)",
                "Income (% Change)",
                "Income (CV)",
                "Neigh. SE (SD)", "Neigh. % Poverty (SD)",
                "Neigh. HH Income (SD)",  "Neigh. Gini (SD)",
                "Neigh. % Renting (SD)", "Neigh. % Unemploy (SD)", 
                "Neigh. % No Degree (SD)"
    )
  )

stables_primary <- 
  primary_results_adjusted |> 
  filter(dvs == "mean_score", ivs != "z_incnt_sd") |> 
  group_split(ivs) |> 
  map(function(x){
    x |> 
      mutate(
        across(.cols = matches("coefficient|^slope$"), ~formatC(.x, 2, 5, format = "f")),
        across(.cols = matches("^se$"), ~formatC(.x, 2, 4, format = "f")),
        across(.cols = matches("^ci|slope_(h|l)"), ~formatC(.x, 2, 5, format = "f")),
        across(.cols = matches("p$"), ~formatC(.x, 3, 5, format = "f"))
      ) |> 
      transmute(
        parameter = parameter,
        b = glue::glue("{coefficient} ({se})"),
        B = glue::glue("{std_coefficient} [{ci_low}, {ci_high}]"),
        p = glue::glue("{p}"),
        rope_p = glue::glue("{equiv_main_p}"),
        slope = glue::glue("{slope} [{slope_low}, {slope_high}]"),
        slope_p = glue::glue("{slope_p}"),
        rope_slope_p = glue::glue("{equiv_slope_p}")
      ) |> 
      mutate(
        parameter = factor(parameter, c(ivs_order, wj_order), c(ivs_labels, wj_labels)),
        slope = ifelse(str_detect(slope, "NA"), "", slope),
        rope_slope_p = ifelse(str_detect(rope_slope_p, "NA"), "", rope_slope_p),
      ) |> 
      mutate(across(everything(), ~as.character(.)))
    
  })

stables_secondary1 <- 
  secondary_results_adjusted1 |> 
  filter(dvs == "mean_score") |> 
  group_split(ivs) |> 
  map(function(x){
    x |> 
      mutate(
        across(.cols = matches("coefficient|^slope$"), ~formatC(.x, 2, 5, format = "f")),
        across(.cols = matches("^se$"), ~formatC(.x, 2, 4, format = "f")),
        across(.cols = matches("^ci|slope_(h|l)"), ~formatC(.x, 2, 5, format = "f")),
        across(.cols = matches("p$"), ~formatC(.x, 3, 5, format = "f"))
      ) |> 
      transmute(
        parameter = parameter,
        b = glue::glue("{coefficient} ({se})"),
        B = glue::glue("{std_coefficient} [{ci_low}, {ci_high}]"),
        p = glue::glue("{p}"),
        rope_p = glue::glue("{equiv_main_p}"),
        slope = glue::glue("{slope} [{slope_low}, {slope_high}]"),
        slope_p = glue::glue("{slope_p}"),
        rope_slope_p = glue::glue("{equiv_slope_p}")
      ) |> 
      mutate(
        parameter = factor(parameter, c(ivs_order, wj_order), c(ivs_labels, wj_labels)),
        slope = ifelse(str_detect(slope, "NA"), "", slope),
        rope_slope_p = ifelse(str_detect(rope_slope_p, "NA"), "", rope_slope_p),
      )
  })

stables_secondary2 <- 
  secondary_results_adjusted2 |> 
  filter(dvs == "mean_score") |> 
  group_split(ivs, model_meta) |> 
  map(function(x){
    x |> 
      mutate(
        across(.cols = matches("coefficient|^slope$"), ~formatC(.x, 2, 5, format = "f")),
        across(.cols = matches("^se$"), ~formatC(.x, 2, 4, format = "f")),
        across(.cols = matches("^ci|slope_(h|l)"), ~formatC(.x, 2, 5, format = "f")),
        across(.cols = matches("p$"), ~formatC(.x, 3, 5, format = "f"))
      ) |> 
      transmute(
        model_meta = model_meta,
        parameter = parameter,
        b = glue::glue("{coefficient} ({se})"),
        B = glue::glue("{std_coefficient} [{ci_low}, {ci_high}]"),
        p = glue::glue("{p}"),
        rope_p = glue::glue("{equiv_main_p}"),
        slope = glue::glue("{slope} [{slope_low}, {slope_high}]"),
        slope_p = glue::glue("{slope_p}"),
        rope_slope_p = glue::glue("{equiv_slope_p}")
      ) |> 
      mutate(
        parameter = factor(parameter, c(ivs_order, wj_order), c(ivs_labels, wj_labels)),
        slope = ifelse(str_detect(slope, "NA"), "", slope),
        rope_slope_p = ifelse(str_detect(rope_slope_p, "NA"), "", rope_slope_p),
      )
  })
