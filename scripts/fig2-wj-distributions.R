# WJ distribution figure --------------------------------------------------
## Prep underying data
fig2a_data <- 
  dvs_analysis_long |> 
  bind_rows(
    dvs_analysis_long |> 
      summarize(
        mean_score = mean(mean_score, na.r = T),
        wj_subtest = "wj_overall_mean",
        .by = id
      )
  ) |> 
  arrange(id, wj_subtest) |> 
  mutate(
    wj_test_type = case_when(str_detect(wj_subtest, "(memna|memse|vrba|incom|picvo)_mean$") ~ "Cognitive Battery",
                             str_detect(wj_subtest, "(pscmp|appld|lwid|wrdat|calc)_mean$") ~ "Achievement Battery",
                             T~"Overall Performance"),
    wj_test_order = factor(wj_subtest, levels = wj_order, labels = wj_labels) |> fct_rev(),
    side = ifelse(wj_subtest == "wj_overall_mean", "top", "bottom")
  )

## Make the figure
fig2a <- 
  fig2a_data |> 
  filter(wj_subtest != "wj_overall_mean") |> 
  ggplot(
    aes(
      x = wj_test_order, 
      y = mean_score, 
      fill = wj_test_order, 
      color = wj_test_order
    )
  ) +
  geom_hline(
    aes(yintercept = mean(mean_score, na.rm = T)), 
    color = "#d2d6de"
  ) +
  geom_dotsinterval(side = "bottom", scale = .5, show.legend = F, color = NA) +
  stat_slabinterval(
    side = "top", 
    scale = .5, 
    point_interval = "mean_qi", 
    color = "black", 
    show.legend = F
  ) +
  scale_x_discrete("") +
  scale_y_continuous("Standard Score") +
  scale_fill_manual(values = wj_palette) +
  coord_flip() +
  ggtitle("Score Distributions") +
  facet_wrap(~wj_test_order, scales = "free_y", nrow = 10) +
  theme(
    strip.text = element_blank(),
    panel.spacing.y = unit(0, "cm"),
    axis.line.y = element_blank(), 
    axis.ticks.y = element_blank()
  )

# WJ Trajectory figure --------------------------------------------------
## Prep underlying data
fig2b_data <- 
  dvs_analysis_wide |> 
  pivot_longer(
    c(-id, -assessment, -assessment_order), 
    names_to = "wj_subtest", 
    values_to = "score"
  ) |> 
  mutate(
    wj_test_order = factor(wj_subtest, levels = wj_order, labels = ifelse(str_length(wj_labels) > 15, str_replace(wj_labels, " ", "\n"), wj_labels)),
    wj_test_type = case_when(str_detect(wj_subtest, "(memna|memse|vrba|incom|picvo)") ~ "Cognitive Battery",
                             str_detect(wj_subtest, "(pscmp|appld|lwid|wrdat|calc)") ~ "Achievement Battery"),
    assessment_order = 1:n(),
    .by = c(id, wj_subtest)
  ) |> 
  mutate(
    wj_test_type_order = 1:n(),
    .by = c(id, assessment, wj_test_type)) |> 
  arrange(id, assessment_order)

fig2b <- 
  fig2b_data |>
  ggplot(
    aes(
      x = assessment_order, 
      y = score, 
      group = wj_test_order, 
      color = wj_test_order,
      fill = wj_test_order
    )
  ) +
  geom_hline(
    aes(yintercept = mean(score, na.rm = T)), 
    color = "#d2d6de"
  ) +
  stat_summary(
    geom = "line",
    fun.data = mean_se
  ) +
  stat_summary(
    geom = "linerange",
    fun.data = mean_sdl,
    linewidth = .5,
  ) +
  stat_summary(
    geom = "point", 
    fun.data = mean_se,
    color = "white", 
    shape = 21, 
    size = 2.25,
    stroke = 1
  ) +
  geom_dots(aes(x = 5.75)) +
  stat_summary(
    geom = "point", 
    aes(x = 5.75),
    fun.data = mean_se,
    fill = "white",
    shape = 21, 
    size = 2.25,
    stroke = 1,
    show.legend = F
  ) +
  scale_x_continuous(
    "\nAssessment",
    breaks = unique(fig2b_data$assessment_order),
    labels = unique(fig2b_data$assessment)
  ) +
  scale_y_continuous(
    "Standard Score\n",
  ) +
  scale_color_manual(values = wj_palette) +
  scale_fill_manual(values = wj_palette) +
  guides(color = "none", fill = "none") +
  facet_wrap(~wj_test_order, ncol = 2)

# Combined Figure ---------------------------------------------------------
fig2 <- fig2b

