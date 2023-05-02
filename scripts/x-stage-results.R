# Setup -------------------------------------------------------------------
## Libraries----
library(tidyverse)
library(ggdist)
library(patchwork)
library(multitool)

## Custom functions----
source("scripts/0-corr_table.R")

## Load data ----
walk(
  list.files("data", "^analysis|results", full.names = T), 
  function(x) load(x, envir =.GlobalEnv)
)

## ggplot2 theme ----
theme_set(
  theme_bw() +
    theme(
      axis.line.y       = element_line(),
      axis.text.y       = element_text(size = rel(1.1)),
      axis.title.y      = element_text(size = rel(1.25), margin = margin(1,0,0,0,"lines")),
      axis.ticks.y      = element_line(),
      axis.text.x       = element_text(size = rel(1.1)),
      axis.title.x      = element_text(size = rel(1.25), margin = margin(1,0,0,0,"lines")),
      axis.line.x       = element_line(),
      panel.border      = element_blank(), 
      panel.spacing.y   = unit(0.5, "lines"),
      plot.margin       = margin(.25,.25,.25,.25,"lines"),
      plot.background   = element_rect(color = NA),
      plot.title        = element_text(size = rel(1.25), hjust = 0, margin = margin(0,0,.5,0, "lines")),
      plot.subtitle     = element_blank(),
      panel.grid        = element_line(color = NA),
      strip.background  = element_blank(), 
      strip.placement   = "outside",
      strip.text        = element_text(size = rel(1), angle = 0)
    )
)

## Colors for WJ-test ----
wj_palette <- ggsci::pal_cosmic("hallmarks_light")(10)

## WJ factor levels and labels ----
wj_order <- 
  c(
    "wj_pscmp",
    "wj_picvo",
    "wj_calc",
    "wj_vrba",
    "wj_lwid",
    "wj_memse",
    "wj_appld",
    "wj_incom",
    "wj_wrdat",
    "wj_memna"
  )

wj_labels <- 
  c(
    "Passage Comprehension",
    "Picture Vocab",
    "Calculations",
    "Verbal Analogies",
    "Letter-Word Pronunciation",
    "Short-Term Memory",
    "Applied Problems",
    "Auditory Processing",
    "Unfamilar Words",
    "Auditory-Visual Associations"
  )

## IV labels
ivs_order <- 
  c(
    "z_family_unp",
    "z_incnt_mean",
    "z_incnt_sd",
    "z_neigh_harsh",
    "z_neigh_unp",
    "z_incnt_sigma",
    "z_incnt_pc"
  )

ivs_label <- 
  c(
    "Family Transitions",
    "Family Poverty (Mean)",
    "Standard Deviation",
    "Neighborhood Poverty (Mean)",
    "Neighborhood Poverty (SD)",
    "Residual Variance",
    "Average Percent Change"
  )

# Plotting Data -----------------------------------------------------------
## Primary Results ----
### Predicted performance by adversity level and subtest
wj_plotting_data1 <- 
  primary_results |> 
  reveal(predicted_vals_fitted, ggpredict_full, T) |> 
  filter(contrast == "wj_subtest_con1") |> 
  select(ivs, dvs, x, predicted, conf.low, conf.high, group)

### Data for plotting equivalence info
equivalence_data1 <- 
  primary_results_stats |> 
  mutate(
    main_effect = ifelse(ivs == parameter, coefficient, NA),
    main_effect_txt = ifelse(ivs == parameter, str_pad(sprintf("%.2f", main_effect), 8), NA),
    main_effect_txt = case_when(ivs == parameter & p < .001 ~ paste0(main_effect_txt, "***"),
                                ivs == parameter & p < .01  ~ paste0(main_effect_txt, "**"),
                                ivs == parameter & p < .01  ~ paste0(main_effect_txt, "*"),
                                T ~ main_effect_txt)
  ) |> 
  fill(main_effect, main_effect_txt) |> 
  filter(parameter %in% wj_order, dvs == "mean_score") |> 
  mutate(
    parameter = factor(parameter, wj_order, wj_labels),
    parameter_num = as.numeric(parameter),
    ivs = factor(ivs, ivs_order, ivs_label),
    sig_pos = ifelse(coefficient < 0, slope_low -.5, slope_high +.5),
    sig_star = case_when(p < .001 ~ "***",
                         p < .01  ~ "**",
                         p < .01  ~ "*",
                         T ~ ""),
    sim_sig_pos = ifelse(slope < 0, slope_low -.5, slope_high +.5),
    sim_sig_star = case_when(slope_p < .001 ~ "***",
                             slope_p < .01  ~ "**",
                             slope_p < .01  ~ "*",
                             T ~ "")
  )

## Secondary Results----
### Predicted performance by adversity level and subtest
wj_plotting_data2 <- 
  secondary_results |> 
  reveal(predicted_vals_fitted, ggpredict_full, T) |> 
  filter(contrast == "wj_subtest_con1") |> 
  select(ivs, dvs, x, predicted, conf.low, conf.high, group)

### Data for plotting equivalence info
equivalence_data2 <- 
  secondary_results_stats |> 
  mutate(
    main_effect = ifelse(ivs == parameter, coefficient, NA),
    main_effect_txt = ifelse(ivs == parameter, str_pad(sprintf("%.2f", main_effect), 8), NA),
    main_effect_txt = case_when(ivs == parameter & p < .001 ~ paste0(main_effect_txt, "***"),
                                ivs == parameter & p < .01  ~ paste0(main_effect_txt, "**"),
                                ivs == parameter & p < .01  ~ paste0(main_effect_txt, "*"),
                                T ~ main_effect_txt)
  ) |> 
  fill(main_effect, main_effect_txt) |> 
  filter(parameter %in% wj_order, dvs == "mean_score") |> 
  mutate(
    parameter = factor(parameter, wj_order, wj_labels),
    parameter_num = as.numeric(parameter),
    ivs = factor(ivs, ivs_order, ivs_label),
    sig_pos = ifelse(coefficient < 0, slope_low -.5, slope_high +.5),
    sig_star = case_when(p < .001 ~ "***",
                         p < .01  ~ "**",
                         p < .01  ~ "*",
                         T ~ ""),
    sim_sig_pos = ifelse(slope < 0, slope_low -.5, slope_high +.5),
    sim_sig_star = case_when(slope_p < .001 ~ "***",
                             slope_p < .01  ~ "**",
                             slope_p < .01  ~ "*",
                             T ~ "")
  )

# Source Scripts ----------------------------------------------------------
source("scripts/fig2-wj-distributions.R")

# Set ggplot2 theme ----
theme_set(
  theme_light() +
    theme(
      text = element_text(size = 11),
      title = element_text(size = 11, hjust = .5),
      axis.line = element_line(),
      panel.border = element_blank(),
      panel.background = element_rect(color = NA),
      panel.grid = element_blank(),
      plot.background = element_rect(color = NA),
      plot.title = element_text(hjust = .5, face = "bold"),
      strip.background = element_rect(color = NA, fill = NA),
      strip.text = element_text(color = "black", hjust = 0.5, face = "bold.italic")
    )
)

source("scripts/fig3-harshness.R")
source("scripts/fig4-unpredictability.R")
source("scripts/fig5-income-variability.R")
source("scripts/table1-wj-corrs.R")
source("scripts/table2-adversity-corrs.R")

# Save output -------------------------------------------------------------
save(
  dvs_analysis_long,
  dvs_analysis_wide,
  fig2,
  fig3,
  fig4,
  fig5,
  table1,
  table2,
  file = "manuscript/r-objects.Rdata"
)

# Write figures -----------------------------------------------------------
ggsave("manuscript/figures/fig2-wj-distributions.pdf", fig2, height = 8, width = 4.5)
ggsave("manuscript/figures/fig3-harshness.pdf", fig3, height = 6, width = 8)
ggsave("manuscript/figures/fig4-unpredictability.pdf", fig4, height = 6, width = 8)
ggsave("manuscript/figures/fig5-income-variability.pdf", fig5, height = 8, width = 8)

