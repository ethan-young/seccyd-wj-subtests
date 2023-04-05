# Setup -------------------------------------------------------------------
## Libraries ----
library(tidyverse)
library(lubridate)
library(sjlabelled)

## Custom functions ----
source("scripts/0-create_codebook.R")
source("scripts/0-corr_table.R")

## Load Data ----
load("data/seccyd-dvs-wj.Rdata")

# Aggregate DVs -----------------------------------------------------------
## Look up table for missingness ----
seccyd_wj_n_assessments <- 
  seccyd_dvs_wj_data2 |> 
  summarize(
    across(starts_with("wj"), ~sum(!is.na(.x))),
    .by = assessment
  ) |> 
  summarize(across(starts_with("wj"), ~sum(.x > 0))) |> 
  pivot_longer(everything(), names_to = "wj_subtest", values_to = "n_assessments")

## Average score and number of scores ----
dvs_analysis_long <- 
  seccyd_dvs_wj_data2 |> 
  group_by(id) |> 
  summarize(
    across(
      starts_with("wj_"), 
      list(
        mean_score = ~ mean(.x, na.rm = T), 
        n_scores = ~sum(!is.na(.x))
      ),
      .names = "{.col}.{.fn}"
    )
  ) |> 
  mutate(across(everything(), ~ifelse(is.nan(.x), NA, .x))) |> 
  pivot_longer(
    c(-id), 
    names_sep = "\\.", 
    names_to = c("wj_subtest", "var"),
    values_to = c("value")
  ) |> 
  pivot_wider(names_from = "var", values_from = "value") |> 
  left_join(
    seccyd_wj_n_assessments,
    by = "wj_subtest"
  ) |> 
  mutate(
    n_missing = n_assessments - n_scores
  )

dvs_analysis_wide <- seccyd_dvs_wj_data2

# Save data ---------------------------------------------------------------
save(
  dvs_analysis_wide,
  dvs_analysis_long,
  file = "data/analysis-dvs.Rdata"
)
