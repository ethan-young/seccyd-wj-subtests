# Setup -------------------------------------------------------------------
## Libraries
library(tidyverse)
library(flextable)
library(gt)

## Load data
load("data/seccyd_measures.Rdata")

table_wj_measures <- 
  seccyd_codebook |> 
  filter(
    str_detect(instrument, "Woodcock-Johnson (Achievement|Cognitive) Test")
  ) |> 
  ungroup() |> 
  transmute(
    Battery = str_extract(instrument, "(Achievement|Cognitive)"),
    Subtest = str_remove(construct, "^.*Test \\d\\d |^.*Test \\d "),
    Ability = str_remove(key, "(Achievement|Cognitive)-"),
    Assessments = str_count(ages, ",") + 1,
    Ages = ages
  ) |> 
  select(-Battery) |> 
  add_row(.before = 1, Subtest = "Achievement") |> 
  add_row(.before = 7, Subtest = "Cognitive") |> 
  flextable() |> 
  bold(i = c(1,7), j = 1) |> 
  padding(i = c(2:6, 8:12), j = 1, padding.left = 15) |> 
  valign(valign = "top") |> 
  set_table_properties(width = 1, layout = "autofit")
