# Standard interaction plots ----
fig5a <- 
  wj_plotting_data2 |> 
  filter(dvs == "z_mean_score", ivs %in% c("z_incnt_sd", "z_incnt_sigma", "z_incnt_pc")) |> 
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
    ivs = factor(ivs, ivs_order, ivs_label)
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
      filter(ivs %in% c("Standard Deviation", "Residual\nStandard Deviation", "Average Percent Change")),
    aes(
      x = -1.5, 
      y = -2, 
      label = paste("Overall Effect\n", main_effect_txt)
    ),
    size = 3,
    hjust = 0,
    inherit.aes = F
  ) +
  scale_y_continuous("Centered WJ Score", expand = c(.15,.15)) +
  scale_x_continuous("", breaks = c(-1,1), labels = c("Low","High")) +
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
  ggtitle("Performance Slopes") +
  facet_wrap(~ivs, ncol = 1) +
  theme(
    axis.ticks = element_blank(),
    axis.line.x = element_blank(),
    strip.text = element_text(hjust = 0.5, size = rel(.85)),
    panel.spacing = unit(1, "lines")
  )

# Equivalence - Interaction Term ------------------------------------------
fig5b <- 
  equivalence_data2 |> 
  filter(ivs %in% c("Standard Deviation", "Residual\nStandard Deviation", "Average Percent Change")) |> 
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
  scale_x_continuous("", breaks = 1:10, labels = levels(equivalence_data2$parameter),position = "top") +
  scale_y_continuous("", breaks = seq(-5, 5, by = 1)) +
  scale_color_manual(values = wj_palette) + 
  scale_shape_manual(values = c(16,21)) +
  facet_wrap(~ivs, ncol = 1) +
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
fig5c <- 
  equivalence_data2 |> 
  filter(ivs %in% c("Standard Deviation", "Residual\nStandard Deviation", "Average Percent Change")) |> 
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
  scale_x_continuous("", breaks = 1:10, labels = levels(equivalence_data2$parameter)) +
  scale_y_continuous("Coefficient\n", breaks = seq(-5, 5, by = 1), position = "right") +
  scale_color_manual(values = wj_palette) + 
  scale_shape_manual(values = c(16,21)) +
  facet_wrap(~ivs, ncol = 1) +
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
fig5 <- 
  (fig5a + fig5b + fig5c + plot_layout(guides = 'collect')) &
  theme(legend.position = "bottom", legend.title = element_blank())

