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
    "Unfamiliar Words",
    "Auditory-Visual Associations"
  )

## IV labels
ivs_order <- 
  c(
    "sex",
    "race",
    "z_meduc",
    "z_incnt_mean",
    "z_neigh_harsh",
    "z_family_unp",
    "z_incnt_sd",
    "z_neigh_unp",
    "z_incnt_sigma",
    "z_incnt_pc"
  )

ivs_labels <- 
  c(
    "Sex Assigned at Birth",
    "Child Race Ethnicity",
    "Maternal Education",
    "Family Income\nDisadvantage",
    "Neigh. Socioeconomic\nDisadvantage",
    "Family Transitions",
    "Family Income\nVariability",
    "Neigh. Socioeconomic\nVariabibiliy",
    "Residual\nStandard Deviation",
    "Average Percent Change"
  )

# Source files ------------------------------------------------------------
source("scripts/supplemental-figs.R")
source("scripts/supplemental-tables.R")

# Save output -------------------------------------------------------------
save(
  sfig1,
  sfig2,
  stable_ivs1,
  stable_ivs2,
  stables_primary,
  stables_secondary,
  file = "manuscript/supplement/r-objects.Rdata"
)
