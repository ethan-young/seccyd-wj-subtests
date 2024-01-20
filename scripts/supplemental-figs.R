# Supplemental Figure 1 ---------------------------------------------------
## Plotting Data ----
### Data from models
wj_plotting_data1 <- 
  primary_results |> 
  reveal(predicted_vals_fitted, predicted_vals_full, "wide") |> 
  filter(contrast == "wj_subtest_con1") |> 
  select(ivs, dvs, x, predicted, conf.low, conf.high, group)

### Data for plotting equivalence info
equivalence_data1 <- 
  primary_results_adjusted |> 
  mutate(
    main_effect = ifelse(ivs == parameter, coefficient, NA),
    main_effect_txt = ifelse(ivs == parameter, str_pad(sprintf("%.2f", main_effect), 8), NA),
    main_effect_txt = case_when(ivs == parameter & p < .001 ~ paste0(main_effect_txt, "***"),
                                ivs == parameter & p < .01  ~ paste0(main_effect_txt, "**"),
                                ivs == parameter & p < .05  ~ paste0(main_effect_txt, "*"),
                                T ~ main_effect_txt)
  ) |> 
  fill(main_effect, main_effect_txt) |> 
  filter(parameter %in% wj_order, dvs == "mean_score") |> 
  mutate(
    parameter = factor(parameter, wj_order, wj_labels),
    parameter_num = as.numeric(parameter),
    ivs = factor(ivs, ivs_order, ivs_labels),
    sig_pos = ifelse(coefficient < 0, slope_low -.5, slope_high +.5),
    sig_star = case_when(p < .001 ~ "***",
                         p < .01  ~ "**",
                         p < .05  ~ "*",
                         T ~ ""),
    sim_sig_pos = ifelse(slope < 0, slope_low -.5, slope_high +.5),
    sim_sig_star = case_when(slope_p < .001 ~ "***",
                             slope_p < .01  ~ "**",
                             slope_p < .05  ~ "*",
                             T ~ "")
  )

## Figure ----
sfig1 <- 
  wj_plotting_data1 |> 
  filter(dvs == "mean_score", ivs != "z_incnt_sd") |> 
  mutate(
    test = factor(group, wj_order, wj_labels),
    adversity = x,
    score = predicted
  ) |> 
  mutate(
    score_overall = mean(score),
    .by = c(ivs, adversity)
  ) |>
  left_join(
    primary_results_adjusted,
    by = c("ivs", "dvs", "group" = "parameter")
  ) |> 
  mutate(
    ivs = factor(ivs, ivs_order, ivs_labels)
  ) |> 
  filter(adversity != 0) |>
  ggplot(
    aes(
      x = adversity , 
      y = score, 
      group = test,
      color = test, 
      alpha = equiv_main,
      shape = equiv_main
    )
  ) +
  geom_line(
    aes(
      x = ifelse(adversity > 0, 1.5, -1.5),
      y = score_overall, group = ivs),
    color = "black",
    linewidth = 1,
    inherit.aes = F
  ) +
  geom_line() +
  geom_point(fill = "white", size = 2, stroke = 1) +
  geom_text(
    data = equivalence_data1 |> 
      distinct(ivs, main_effect, main_effect_txt) |> 
      filter(ivs != "Family Income\nVariability"),
    aes(
      x = -1.5, 
      y = 92, 
      label = paste("Overall Effect: ", main_effect_txt)
    ),
    size = 3,
    hjust = 0,
    inherit.aes = F
  ) +
  scale_y_continuous("WJ Score", expand = c(.2,.2)) +
  scale_x_continuous("", breaks = c(-1,1), labels = c("Low\nDisadvantage","High\nDisadvantage")) +
  scale_color_manual("WJ Subtest", values = wj_palette) + 
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_shape_manual(values = c(16,21)) +
  scale_alpha_manual(values = c(.1, 1)) +
  guides(
    color = guide_legend(ncol = 3, byrow = T),
    fill = "none",
    shape = "none", 
    alpha = "none"
  ) +
  ggtitle("Primary Analysis Slopes\n(uncentered)") +
  facet_wrap(~ivs, ncol = 2) +
  theme(
    axis.ticks = element_blank(),
    axis.line.x = element_blank(),
    legend.justification = "center",
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.text = element_text(hjust = 0.5, size = rel(.85)),
    panel.spacing = unit(1, "lines")
  )

# Supplemental Figure 2 ---------------------------------------------------
## Plotting Data ----
### Data from model
wj_plotting_data2 <- 
  secondary_results |> 
  reveal(predicted_vals_fitted, predicted_vals_full, "wide") |> 
  filter(contrast == "wj_subtest_con1") |> 
  select(ivs, dvs, x, predicted, conf.low, conf.high, group)

### Data for plotting equivalence info
equivalence_data2 <- 
  secondary_results_adjusted |> 
  mutate(
    main_effect = ifelse(ivs == parameter, coefficient, NA),
    main_effect_txt = ifelse(ivs == parameter, str_pad(sprintf("%.2f", main_effect), 8), NA),
    main_effect_txt = case_when(ivs == parameter & p < .001 ~ paste0(main_effect_txt, "***"),
                                ivs == parameter & p < .01  ~ paste0(main_effect_txt, "**"),
                                ivs == parameter & p < .05  ~ paste0(main_effect_txt, "*"),
                                T ~ main_effect_txt)
  ) |> 
  fill(main_effect, main_effect_txt) |> 
  filter(parameter %in% wj_order, dvs == "mean_score") |> 
  mutate(
    parameter = factor(parameter, wj_order, wj_labels),
    parameter_num = as.numeric(parameter),
    sig_pos = ifelse(coefficient < 0, slope_low -.5, slope_high +.5),
    sig_star = case_when(p < .001 ~ "***",
                         p < .01  ~ "**",
                         p < .05  ~ "*",
                         T ~ ""),
    sim_sig_pos = ifelse(slope < 0, slope_low -.5, slope_high +.5),
    sim_sig_star = case_when(slope_p < .001 ~ "***",
                             slope_p < .01  ~ "**",
                             slope_p < .05  ~ "*",
                             T ~ "")
  )

## Figure ----
sfig2 <- 
  wj_plotting_data2 |> 
  filter(dvs == "mean_score", ivs %in% c("z_incnt_sd", "z_incnt_sigma", "z_incnt_pc")) |> 
  mutate(
    test = factor(group, wj_order, wj_labels),
    adversity = x,
    score = predicted
  ) |> 
  mutate(
    score_overall = mean(score),
    .by = c(ivs, adversity)
  ) |>
  left_join(
    secondary_results_adjusted,
    by = c("ivs", "dvs", "group" = "parameter")
  ) |> 
  mutate(
    ivs = factor(ivs, ivs_order, ivs_labels |> str_replace("Family Income\nVariability","Standard Deviation")),
  ) |> 
  filter(adversity != 0) |>
  ggplot(
    aes(
      x = adversity , 
      y = score, 
      group = test,
      color = test, 
      alpha = equiv_main,
      shape = equiv_main
    )
  ) +
  geom_line(
    aes(
      x = ifelse(adversity > 0, 1.5, -1.5),
      y = score_overall, group = ivs),
    color = "black",
    linewidth = 1,
    inherit.aes = F
  ) +
  geom_line() +
  geom_point(fill = "white", size = 2, stroke = 1) +
  geom_text(
    data = equivalence_data2 |> 
      distinct(ivs, main_effect, main_effect_txt) |> 
      filter(ivs %in% c("z_incnt_sd", "z_incnt_sigma", "z_incnt_pc")) |> 
      mutate(ivs = factor(ivs, ivs_order, ivs_labels |> str_replace("Family Income\nVariability","Standard Deviation"))),
    aes(
      x = -1.5, 
      y = 92, 
      label = paste("Overall Effect\n", main_effect_txt)
    ),
    size = 3,
    hjust = 0,
    inherit.aes = F
  ) +
  scale_y_continuous("WJ Score", expand = c(.15,.15)) +
  scale_x_continuous("", breaks = c(-1,1), labels = c("Low","High")) +
  scale_color_manual("WJ Subtest", values = wj_palette) + 
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_shape_manual(values = c(16,21)) +
  scale_alpha_manual(values = c(.1, 1)) +
  guides(
    color = guide_legend(ncol = 1, byrow = T),
    fill = "none",
    shape = "none", 
    alpha = "none"
  ) +
  labs(
    title = "Secondary Analyses",
    subtitle = "Family Income Variability\nPerformance Slopes"
  ) +
  facet_wrap(~ivs, ncol = 1) +
  theme(
    axis.ticks = element_blank(),
    axis.line.x = element_blank(),
    strip.text = element_text(hjust = 0.5, size = rel(.85)),
    panel.spacing = unit(1, "lines"), 
    plot.subtitle = element_text(hjust = .5)
  )

