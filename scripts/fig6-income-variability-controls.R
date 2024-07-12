wj_plotting_data3 <- 
  secondary_results2 |> 
  reveal(predicted_vals_fitted, predicted_vals_full, .unpack_specs = "wide") |> 
  filter(contrast == "wj_subtest_con1") |> 
  select(ivs, dvs, x, model_meta, predicted, conf.low, conf.high, group)

### Data for plotting equivalence info
equivalence_data3 <- 
  secondary_results_adjusted2 |> 
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

# Standard interaction plots ----
fig6a <- 
  wj_plotting_data3 |> 
  filter(dvs == "z_mean_score", ivs %in% c("z_incnt_sd", "z_incnt_sigma")) |> 
  mutate(
    test = factor(group, wj_order, wj_labels),
    adversity = x,
    score = predicted
  ) |> 
  mutate(
    score_overall = mean(score),
    .by = c(ivs, adversity, model_meta)
  ) |>
  left_join(
    secondary_results_adjusted2,
    by = c("ivs", "dvs", "group" = "parameter", "model_meta")
  ) |> 
  mutate(
    analysis_order = 
      case_when(
        model_meta == "within_model" & ivs == "z_incnt_sd" ~ 1,
        model_meta == "cov_main"     & ivs == "z_incnt_sd" ~ 2,
        model_meta == "cov_int"      & ivs == "z_incnt_sd" ~ 3,
        model_meta == "within_model" & ivs == "z_incnt_sigma" ~ 4,
        model_meta == "cov_main"     & ivs == "z_incnt_sigma" ~ 5,
        model_meta == "cov_int"      & ivs == "z_incnt_sigma" ~ 6,
      ),
    ivs = factor(ivs, ivs_order, ivs_labels),
    control_type = 
      case_when(
        model_meta == "within_model" ~ "No Control",
        model_meta == "cov_main"     ~ "Main Effect Control",
        model_meta == "cov_int"      ~ "Main + Simle Effect Control"
      ),
    analysis_type = glue::glue("{ivs}\n{control_type}"),
    analysis_type = fct_reorder(analysis_type, analysis_order)
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
    data = equivalence_data3 |> 
      distinct(ivs, main_effect, main_effect_txt, model_meta) |> 
      mutate(
        analysis_order = 
          case_when(
            model_meta == "within_model" & as.numeric(ivs) == 7 ~ 1,
            model_meta == "cov_main"     & as.numeric(ivs) == 7 ~ 2,
            model_meta == "cov_int"      & as.numeric(ivs) == 7 ~ 3,
            model_meta == "within_model" & as.numeric(ivs) == 9 ~ 4,
            model_meta == "cov_main"     & as.numeric(ivs) == 9 ~ 5,
            model_meta == "cov_int"      & as.numeric(ivs) == 9 ~ 6,
          ),
        control_type = 
          case_when(
            model_meta == "within_model" ~ "No Control",
            model_meta == "cov_main"     ~ "Main Effect Control",
            model_meta == "cov_int"      ~ "Main + Simle Effect Control"
          ),
        analysis_type = glue::glue("{ivs}\n{control_type}"),
        analysis_type = fct_reorder(analysis_type, analysis_order)
      ) |> 
      filter(ivs %in% c("Standard Deviation", "Residual\nStandard Deviation")),
    aes(
      x = -1.5, 
      y = -Inf, 
      label = glue::glue("Overall = {str_trim(main_effect_txt)}")
    ),
    size = 3,
    hjust = 0,
    vjust = -.5,
    inherit.aes = F
  ) +
  scale_y_continuous("Centered WJ Score", expand = c(.15,.15)) +
  scale_x_continuous("", breaks = c(-1,1), labels = c("Low","High")) +
  scale_color_manual("WJ Subtest", values = wj_palette) + 
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_shape_manual(values = c(16,21)) +
  scale_alpha_manual(values = c(.1, 1)) +
  guides(
    color = guide_legend(ncol = 2, byrow = T),
    fill = "none",
    shape = "none", 
    alpha = "none"
  ) +
  ggtitle("Performance Slopes") +
  facet_wrap(~analysis_type, ncol = 1,strip.position = "left") +
  theme(
    axis.ticks = element_blank(),
    axis.line.x = element_blank(),
    axis.title.y = element_blank(),
    strip.text.y.left = element_text(hjust = 0.5, size = rel(1), angle = 0),
    panel.spacing = unit(1, "lines"),
    legend.position = "none"
  )

# Equivalence - Interaction Term ------------------------------------------
fig6b <- 
  equivalence_data3 |> 
  filter(ivs %in% c("Standard Deviation", "Residual\nStandard Deviation", "Average Percent Change", "Coefficient of Variation")) |> 
  mutate(
    analysis_order = 
      case_when(
        model_meta == "within_model" & as.numeric(ivs) == 7 ~ 1,
        model_meta == "cov_main"     & as.numeric(ivs) == 7 ~ 2,
        model_meta == "cov_int"      & as.numeric(ivs) == 7 ~ 3,
        model_meta == "within_model" & as.numeric(ivs) == 9 ~ 4,
        model_meta == "cov_main"     & as.numeric(ivs) == 9 ~ 5,
        model_meta == "cov_int"      & as.numeric(ivs) == 9 ~ 6,
      ),
    control_type = 
      case_when(
        model_meta == "within_model" ~ "No Control",
        model_meta == "cov_main"     ~ "Main Effect Control",
        model_meta == "cov_int"      ~ "Main + Simle Effect Control"
      ),
    analysis_type = glue::glue("{ivs}\n{control_type}"),
    analysis_type = fct_reorder(analysis_type, analysis_order)
  ) |> 
  ggplot(aes(color = parameter)) +
  geom_rect(
    aes(
      xmin = 0, xmax = 11,  ymin = main_effect - 1.5, ymax = main_effect + 1.5
    ),
    fill = "#F2F3F5",
    inherit.aes = F
  ) +
  geom_segment(
    aes(x = 0, xend = 11, y = main_effect, yend = main_effect),
    color = "black",
  ) +
  geom_segment(
    aes(x = parameter_num, xend = parameter_num, y = slope_low, yend = slope_high),
    linewidth = 1
  ) +
  geom_point(
    aes(x = parameter_num, y = slope, shape = equiv_main),
    size = 2,
    stroke = 1,
    fill = "white"
  ) +
  geom_text(
    aes(x = parameter_num, y = sig_pos, label = sig_star),
    color = "black"
  ) +
  scale_x_continuous("", breaks = 1:10, labels = levels(equivalence_data3$parameter),position = "top") +
  scale_y_continuous("", breaks = seq(-5, 5, by = 1)) +
  scale_color_manual(values = wj_palette) + 
  scale_shape_manual(values = c(16,21)) +
  facet_wrap(~analysis_type, ncol = 1) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle("Interaction Effects") +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.spacing = unit(1, "lines"),
    strip.text = element_blank()
  )

# Equivalence - Simple Slopes ---------------------------------------------
fig6c <- 
  equivalence_data3 |> 
  filter(ivs %in% c("Standard Deviation", "Residual\nStandard Deviation", "Average Percent Change", "Coefficient of Variation")) |> 
  mutate(
    analysis_order = 
      case_when(
        model_meta == "within_model" & as.numeric(ivs) == 7 ~ 1,
        model_meta == "cov_main"     & as.numeric(ivs) == 7 ~ 2,
        model_meta == "cov_int"      & as.numeric(ivs) == 7 ~ 3,
        model_meta == "within_model" & as.numeric(ivs) == 9 ~ 4,
        model_meta == "cov_main"     & as.numeric(ivs) == 9 ~ 5,
        model_meta == "cov_int"      & as.numeric(ivs) == 9 ~ 6,
      ),
    control_type = 
      case_when(
        model_meta == "within_model" ~ "No Control",
        model_meta == "cov_main"     ~ "Main Effect Control",
        model_meta == "cov_int"      ~ "Main + Simle Effect Control"
      ),
    analysis_type = glue::glue("{ivs}\n{control_type}"),
    analysis_type = fct_reorder(analysis_type, analysis_order)
  ) |> 
  ggplot(aes(color = parameter)) +
  geom_rect(
    aes(
      xmin = 0, xmax = 11,  ymin = 0 - 1.5, ymax = 0 + 1.5
    ),
    fill = "#F2F3F5",
    inherit.aes = F
  ) +
  geom_segment(
    aes(x = 0, xend = 11, y = 0, yend = 0),
    color = "black"
  ) +
  geom_segment(
    aes(x = parameter_num, xend = parameter_num, y = slope_low, yend = slope_high),
    linewidth = 1
  ) +
  geom_point(
    aes(x = parameter_num, y = slope, shape = equiv_slope),
    size = 2,
    stroke = 1,
    fill = "white"
  ) +
  geom_text(
    aes(x = parameter_num, y = sim_sig_pos, label = sim_sig_star),
    color = "black"
  ) +
  scale_x_continuous("", breaks = 1:10, labels = levels(equivalence_data3$parameter)) +
  scale_y_continuous("Coefficient\n", breaks = seq(-5, 5, by = 1), position = "right") +
  scale_color_manual(values = wj_palette) + 
  scale_shape_manual(values = c(16,21)) +
  facet_wrap(~analysis_type, ncol = 1) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle("Simple Effects") +
  theme(
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(hjust = 1),
    axis.ticks = element_blank(),
    panel.spacing = unit(1, "lines"),
    strip.text = element_blank()
  )

# Stitch Together Plots ---------------------------------------------------
fig6 <- 
  (fig6a + fig6b + fig6c + plot_layout(guides = 'collect')) &
  theme(legend.position = "bottom", legend.title = element_blank())

