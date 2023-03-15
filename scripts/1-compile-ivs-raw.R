# Setup -------------------------------------------------------------------
## Libraries ----
library(tidyverse)
library(haven)
library(sjlabelled)
library(lubridate)

## Custom functions ----
source("scripts/0-create_codebook.R")

# Read data ---------------------------------------------------------------
## Get file paths ----
seccyd_ivs_changes_files <- 
  list.files(
    "..", 
    pattern = "^f\\d\\d(a|A).*.sas7bdat", 
    full.names = T, 
    recursive = T
  )

seccyd_changes_names <- 
  seccyd_ivs_changes_files |> str_extract("(.*)(\\d\\d)", 2) |> 
  paste0("hh_comp", ... = _)

## Read data into a list ----
seccyd_ivs_changes_list <-
  map(seccyd_ivs_changes_files, function(x){
    
    data <- read_sas(x)
    codebook <- create_codebook(data)
    
    list(
      data = data,
      codebook = codebook
    )
  }) |> 
  set_names(seccyd_changes_names)



seccyd_assessments <- 
  c(
    "01", "03", "06", "09", "12", "15", 
    "18", "21", "24", "27", "30", "33",
    "36", "42", "46", "50", "54", "60", 
    "kf", "ks", "1f", "1s", "2f", "2s",
    "g3", "g4", "g5", "g6", "x4", "x5"
  )

seccyd_ivs_changes_data1 <- 
  map(seccyd_ivs_changes_list, function(x){
    x[[1]] %>% 
      rename_with(tolower) |> 
      select(
        id, 
        any_of("brthdats"),
        matches("^r(.*)_(\\d*)"))
  }) %>% 
  reduce(full_join, by = "id")

seccyd_ivs_changes_data2 <- 
  seccyd_ivs_changes_data1 |> 
  pivot_longer(c(-id, -brthdats), names_sep = "_", names_to = c("var","number"), values_to = "value") |> 
  mutate(var = str_replace(var, "^(r(b|m|ag|sx|ch))(.*)$", "\\1_\\3")) |> 
  separate(var, into = c("var", "assessment"), sep = "_") |> 
  pivot_wider(names_from = "var", values_from = "value") |> 
  filter(!(is.na(rb) & is.na(rm) & is.na(rag) & is.na(rsx) & is.na(rch)))

seccyd_ivs_changes_data2$assessment |> unique() |> as.numeric() |> sort()

seccyd_ivs_changes_data2 |> 
  mutate(
    move_out           = rch == 2,
    move_in            = rch == 3,
    paternal_fig       = rb %in% c(1,2),
    adult              = rb %in% c(1:5) | rag > 18,
    adult_unrel        = rb == 5 | (rb %in% c(6:9) & rag > 18),
    change_paternal    = if_else((move_out|move_in) & paternal_fig, 1, 0),
    change_adult       = if_else((move_out|move_in) & adult, 1, 0),
    change_unrel_adult = if_else((move_out|move_in) & adult, 1, 0)
  ) |> 
  summarize(across(starts_with("change"), ~sum(.x, na.rm = T)), .by = c(id, assessment)) |> 
  
  summarize(across(starts_with("change"), ~sum(.x, na.rm = T)), .by = id) |> 
  filter(change_paternal >= 1)
