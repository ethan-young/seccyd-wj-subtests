# Set up ------------------------------------------------------------------
## Libraries
library(tidyverse)
library(lme4)
library(ggeffects)
library(parameters)
library(broom)
library(multitool)
library(patchwork)

## Custom functions 
source("scripts/0-create_codebook.R")
source("scripts/0-corr_table.R")

# Linear model function
incnt_linear <- function(df){
  lm(incnt ~ as.numeric(assessment), data=df)
}

## Load data
walk(
  list.files("data", "^analysis", full.names = T), 
  function(x) load(x, envir =.GlobalEnv)
)

# Recompute Income-to-Needs Variability -----------------------------------
## Grab only income-to-needs scores
new_incnt_data1 <- 
  ivs_data2 |> 
  filter(
    assessment %in% c("01", "06", "15", "24", "36", "54")
  ) |> 
  select(id, assessment, incnt)

## Nest data to apply linear function to each observation
new_incnt_data2 <- 
  new_incnt_data1 |> 
  drop_na(incnt) |> 
  nest(.by = id) |> 
  mutate(
    incnt_lm        = map(data, incnt_linear),
    incnt_mean      = map_dbl(data,\(x) mean(x$incnt, na.rm = T)),
    incnt_sd        = map_dbl(data,\(x) sd(x$incnt, na.rm = T)),
    incnt_coefs     = map(incnt_lm, tidy),
    incnt_stats     = map(incnt_lm, glance),
    incnt_intercept = map_dbl(incnt_coefs, \(x) unlist(x[1,"estimate"])),
    incnt_slope     = map_dbl(incnt_coefs, \(x) unlist(x[2,"estimate"])),
    incnt_deviance  = incnt_stats |> map_dbl("deviance"),
    incnt_sigma     = incnt_stats |> map_dbl("sigma"),
    incnt_pc        = map_dbl(data,\(x) mean(abs(x$incnt - lag(x$incnt))/lag(x$incnt), na.rm = T)),
    incnt_n         = map_dbl(data, nrow),
    incnt_cv        = incnt_sd/incnt_mean,
    .by = id
  )

# Merge new Variability Scores with Analysis Data -------------------------
secondary_data2 <- 
  ivs_analysis |> 
  select(id, meduc, csex, crace) |> 
  left_join(
    new_incnt_data2 |> 
      select(id, incnt_mean, incnt_sd, incnt_sigma, incnt_pc, incnt_cv)
  ) |> 
  mutate(
    incnt_mean = incnt_mean * -1,
    across(
      c(incnt_mean, incnt_sd, incnt_sigma, meduc),
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

# New Analysis Grid -------------------------------------------------------
secondary_grid2 <- 
  secondary_data2 |> 
  add_variables(
    "ivs",
    z_incnt_sd,
    z_incnt_sigma
  ) |> 
  add_variables("dvs", mean_score, z_mean_score) |> 
  add_variables("contrast", wj_subtest_con1, wj_subtest_con2) |> 
  add_model("within_model", lmer({dvs} ~ sex + race + z_meduc + {ivs}*{contrast} + (1|id))) |> 
  add_model("cov_main", lmer({dvs} ~ sex + race + z_meduc + z_incnt_mean + {ivs}*{contrast} + (1|id))) |> 
  add_model("cov_int", lmer({dvs} ~ sex + race + z_meduc + z_incnt_mean*{contrast} + {ivs}*{contrast} + (1|id))) |> 
  add_postprocess("betas_unstd", model_parameters()) |> 
  add_postprocess("betas_std", standardize_parameters()) |> 
  add_postprocess("betas_slopes", hypothesis_test(c("{ivs}", "{contrast}"), re.form = NA, test = NULL)) |> 
  add_postprocess("eq_betas", equivalence_test(range = c(-1.5, 1.5))) |> 
  add_postprocess("eq_slopes", hypothesis_test(c("{ivs}", "{contrast}"), re.form = NA, test= NULL, equivalence = c(-1.5, 1.5))) |> 
  add_postprocess("predicted_vals", ggpredict(c("{ivs} [-1,0,1]", "{contrast}")))

## Expand pipeline into a grid
secondary_grid_expanded2 <- expand_decisions(secondary_grid2)

# Conduct analyses and extract info ---------------------------------------
## Execute analysis pipeline grid
secondary_results2 <- run_multiverse(secondary_grid_expanded2)

## Extract results data
secondary_results_stats2 <-
  secondary_results2 |> 
  reveal(betas_unstd_fitted, betas_unstd_full, .unpack_specs = "wide") |> 
  rename_with(tolower) |> 
  filter(contrast == "wj_subtest_con1" | (contrast == "wj_subtest_con2" & str_detect(parameter, "wj_wrdat"))) |> 
  select(ivs, dvs, model_meta, parameter, coefficient, se, p) |> 
  left_join(
    secondary_results2 |> 
      reveal(betas_std_fitted, betas_std_full, .unpack_specs = "wide") |> 
      rename_with(tolower) |> 
      filter(contrast == "wj_subtest_con1" | (contrast == "wj_subtest_con2" & str_detect(parameter, "wj_wrdat"))) |> 
      select(ivs, dvs, model_meta, parameter, std_coefficient, ci_low, ci_high)
  ) |> 
  left_join(
    secondary_results2 |> 
      reveal(eq_betas_fitted, eq_betas_full, .unpack_specs = "wide") |> 
      rename_with(tolower) |> 
      filter(contrast == "wj_subtest_con1" | (contrast == "wj_subtest_con2" & str_detect(parameter, "wj_wrdat"))) |> 
      select(ivs, dvs, model_meta, parameter, p, ci_low, ci_high) |> 
      rename(equiv_main_p = p, equiv_main_low = ci_low, equiv_main_high = ci_high) |> 
      mutate(equiv_main = ifelse(equiv_main_p < .05, "Accepted", "Rejected"))
  ) |> 
  filter(parameter %in% c("sex", "race", "z_meduc") | parameter == ivs | str_detect(parameter, ":")) |> 
  filter(!str_detect(parameter, "incnt_mean")) |> 
  mutate(parameter = ifelse(str_detect(parameter, ":"), str_extract(parameter, "(\\.)(wj_.....)", 2) |> str_remove("-$"), parameter)) |> 
  left_join(
    secondary_results2 |> 
      reveal(betas_slopes_fitted, betas_slopes_full, .unpack_specs = "wide") |> 
      filter(contrast == "wj_subtest_con1") |> 
      select(ivs, dvs, model_meta, wj_subtest_con1, Slope, p.value, conf.low, conf.high) |> 
      rename(parameter = wj_subtest_con1, slope_p = p.value, slope = Slope, slope_low = conf.low, slope_high = conf.high)
  ) |> 
  left_join(
    secondary_results2 |> 
      reveal(eq_slopes_fitted, eq_slopes_full, .unpack_specs = "wide") |> 
      filter(contrast == "wj_subtest_con1") |> 
      rename(equiv_slope_p = p.value) |> 
      mutate(equiv_slope = ifelse(equiv_slope_p < .05, "Accepted", "Rejected")) |> 
      select(ivs, dvs, model_meta, wj_subtest_con1, equiv_slope, equiv_slope_p) |> 
      rename(parameter = wj_subtest_con1)
  )

# Adjust p-values using Benjamini & Hochberg ------------------------------
secondary_results_adjusted2 <- 
  secondary_results_stats2 |> 
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
  secondary_data2,
  secondary_grid2,
  secondary_grid_expanded2,
  secondary_results2,
  secondary_results_stats2,
  secondary_results_adjusted2,
  file = "data/secondary-results-2.Rdata"
)
