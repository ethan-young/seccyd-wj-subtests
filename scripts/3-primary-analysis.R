# Set up ------------------------------------------------------------------
## Libraries
library(tidyverse)
library(lme4)
library(ggeffects)
library(parameters)
library(multitool)
library(patchwork)

## Custom functions 
source("scripts/0-create_codebook.R")
source("scripts/0-corr_table.R")

## Load data
walk(
  list.files("data", "^analysis", full.names = T), 
  function(x) load(x, envir =.GlobalEnv)
)

# Merge ivs and dvs -------------------------------------------------------
primary_data <- 
  ivs_analysis |> 
  mutate(
    incnt_mean = incnt_mean * -1,
    across(
      c(family_unp, incnt_mean, incnt_sd, neigh_harsh, neigh_unp, meduc),
      ~scale(.x) |> as.numeric(), .names = "z_{.col}"
    ),
  ) |> 
  right_join(
    dvs_analysis_long,
    by = "id"
  ) |> 
  mutate(
    z_mean_score = scale(mean_score, center = T, scale = F) |> as.numeric(),
    .by = wj_subtest
  ) |> 
  mutate(
    wj_subtest_fct  = factor(wj_subtest),
    wj_subtest_con1 = faux::contr_code_sum(wj_subtest),
    wj_subtest_con2 = faux::contr_code_sum(wj_subtest, omit = 1),
    sex = ifelse(csex == 1, -1, 1),
    race = ifelse(crace == 4, -1, 1)
  )

# Analysis grid -----------------------------------------------------------
## Create a pipeline
primary_grid <- 
  primary_data |> 
  add_variables(
    "ivs",
    z_family_unp, 
    z_incnt_mean, 
    z_incnt_sd, 
    z_neigh_harsh, 
    z_neigh_unp
  ) |> 
  add_variables("dvs", mean_score, z_mean_score) |> 
  add_variables("contrast", wj_subtest_con1, wj_subtest_con2) |> 
  add_model(
    "within_model", 
    lmer({dvs} ~ sex + race + z_meduc + {ivs}*{contrast} + (1|id))
  ) |> 
  add_postprocess("betas_unstd", model_parameters()) |> 
  add_postprocess("betas_std", standardize_parameters()) |> 
  add_postprocess(
    "betas_slopes", 
    hypothesis_test(
      c("{ivs}", "{contrast}"), 
      re.form = NA, 
      test = NULL
    )
  ) |> 
  add_postprocess("eq_betas", equivalence_test(range = c(-1.5, 1.5))) |> 
  add_postprocess(
    "eq_slopes", 
    hypothesis_test(
      c("{ivs}", "{contrast}"), 
      re.form = NA, 
      test= NULL, 
      equivalence = c(-1.5, 1.5)
    )
  ) |> 
  add_postprocess(
    "predicted_vals", 
    ggpredict(c("{ivs} [-1,0,1]", "{contrast}"))
  )

## Expand pipeline into a grid
primary_grid_expanded <- expand_decisions(primary_grid)

# Conduct analyses and extract info ---------------------------------------
## Execute analysis pipeline grid
primary_results <- run_multiverse(primary_grid_expanded)

## Extract results data
primary_results_stats <-
  primary_results |> 
  reveal(betas_unstd_fitted, betas_unstd_full, .unpack_specs = "wide") |> 
  rename_with(tolower) |> 
  filter(contrast == "wj_subtest_con1" | (contrast == "wj_subtest_con2" & str_detect(parameter, "wj_wrdat"))) |> 
  select(ivs, dvs, parameter, coefficient, se, p) |> 
  left_join(
    primary_results |> 
      reveal(betas_std_fitted, betas_std_full, .unpack_specs = "wide") |> 
      rename_with(tolower) |> 
      filter(contrast == "wj_subtest_con1" | (contrast == "wj_subtest_con2" & str_detect(parameter, "wj_wrdat"))) |> 
      select(ivs, dvs, parameter, std_coefficient, ci_low, ci_high)
  ) |> 
  left_join(
    primary_results |> 
      reveal(eq_betas_fitted, eq_betas_full, .unpack_specs = "wide") |> 
      rename_with(tolower) |> 
      filter(contrast == "wj_subtest_con1" | (contrast == "wj_subtest_con2" & str_detect(parameter, "wj_wrdat"))) |> 
      select(ivs, dvs, parameter, p, ci_low, ci_high) |> 
      rename(equiv_main_p = p, equiv_main_low = ci_low, equiv_main_high = ci_high) |> 
      mutate(equiv_main = ifelse(equiv_main_p < .05, "Accepted", "Rejected"))
  ) |> 
  filter(parameter %in% c("sex", "race", "z_meduc") | parameter == ivs | str_detect(parameter, ":")) |> 
  mutate(parameter = ifelse(str_detect(parameter, ":"), str_extract(parameter, "(\\.)(wj_.....)", 2) |> str_remove("-$"), parameter)) |> 
  left_join(
    primary_results |> 
      reveal(betas_slopes_fitted, betas_slopes_full, .unpack_specs = "wide") |> 
      filter(contrast == "wj_subtest_con1") |> 
      select(ivs, dvs, wj_subtest_con1, Slope, p.value, conf.low, conf.high) |> 
      rename(parameter = wj_subtest_con1, slope_p = p.value, slope = Slope, slope_low = conf.low, slope_high = conf.high)
  ) |> 
  left_join(
    primary_results |> 
      reveal(eq_slopes_fitted, eq_slopes_full, .unpack_specs = "wide") |> 
      filter(contrast == "wj_subtest_con1") |> 
      rename(equiv_slope_p = p.value) |> 
      mutate(equiv_slope = ifelse(equiv_slope_p < .05, "Accepted", "Rejected")) |> 
      select(ivs, dvs, wj_subtest_con1, equiv_slope, equiv_slope_p) |> 
      rename(parameter = wj_subtest_con1)
  )


# Adjust p-values using Benjamini & Hochberg ------------------------------
primary_results_adjusted <- 
  primary_results_stats |> 
  mutate(
    adjust = ifelse(str_detect(parameter, "^wj"), 1, 0)
  ) |> 
  mutate(
    p_int_adjusted = p.adjust(p, method = "BH"),
    p_slope_adjusted = p.adjust(slope_p, method = "BH"),
    .by = c(ivs, dvs, adjust)
  ) |> 
  mutate(
    p = ifelse(adjust == 1, p_int_adjusted, p),
    slope_p = ifelse(adjust == 1, p_slope_adjusted, slope_p)
  )

# Save Results ------------------------------------------------------------
save(
  primary_data,
  primary_grid,
  primary_grid_expanded,
  primary_results,
  primary_results_stats,
  primary_results_adjusted,
  file = "data/primary-results.Rdata"
)

