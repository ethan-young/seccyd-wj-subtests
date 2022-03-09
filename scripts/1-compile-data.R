# Setup -------------------------------------------------------------------
## Libraries ----
library(tidyverse)
library(haven)

## Load Functions ----
source("scripts/0-create_codebook.R")
source("scripts/0-corr_table.R")


# Gather Woodcock Johnson Data -------------------------------------------------------------
## Compile File Names ----
wj_files <- 
  list.files(path = "..", recursive = T, full.names = T) %>% 
  str_subset("Woodcock.*sav$")

wj_years <-
  wj_files %>% 
  str_split("(/)| ") %>% 
  map(function(x) paste(x[3], x[2], sep = "_")) %>% 
  unlist %>% 
  str_remove("(st|rd|th)$") %>% 
  tolower()

## Create Data List ----
wj_list <- 
  map(wj_files, function(x){
    
    data <- read_sav(x)
    codebook <- create_codebook(data)
    
    list(
      data = data,
      codebook = codebook
    )
  }) %>% 
  set_names(wj_years)

## Compile Standard Scores ----
wj_data <- 
  map2(names(wj_list), wj_list, function(x,y){
    assessment <- x %>%
      str_split("_") %>% 
      map(function(z) paste0(z[2], z[1])) %>% 
      unlist %>% 
      str_replace_all(c("year" = "y", "grade" = "g", "month" = "m"))
    
    y[[1]] %>% 
      select(ID, matches("_SS$")) %>% 
      rename_with(.cols = c(-ID), ~paste(.x, assessment, sep = "_")) %>% 
      rename_with(tolower)
  }) %>% 
  reduce(full_join, by = "id")

## Stack Variables ----
wj_data_long <- 
  pivot_longer(wj_data, cols = c(-id), names_to = "subtest", values_to = "std_score") %>% 
  mutate(subtest = str_remove(subtest,"_ss")) %>% 
  separate(subtest, c("subtest","assessment"), sep = "_") %>% 
  pivot_wider(names_from = subtest, values_from = std_score) %>% 
  mutate(assessment_order = case_when(assessment == "54m" ~ 1, 
                                      assessment == "1g"  ~ 2, 
                                      assessment == "3g"  ~ 3, 
                                      assessment == "4g"  ~ 4,
                                      assessment == "5g"  ~ 5,
                                      assessment == "15y" ~ 6)) %>% 
           arrange(id, assessment_order) %>% 
  select(id, assessment, assessment_order, everything())

# Gather Continuous Performance Data -----------------------------------------
## Compile File Names ----
cp_files <- 
  list.files(path = "..", recursive = T, full.names = T) %>% 
  str_subset("Continuous.*sav$")

cp_years <-
  cp_files %>% 
  str_split("(/)| ") %>% 
  map(function(x) paste(x[3], x[2], sep = "_")) %>% 
  unlist %>% 
  str_remove("(st|rd|th)$") %>% 
  tolower()

## Create Data List ----
cp_list <- 
  map(cp_files, function(x){
    
    data <- read_sav(x)
    codebook <- create_codebook(data)
    
    list(
      data = data,
      codebook = codebook
    )
  }) %>% 
  set_names(cp_years)

## Compile Error Scores ----
cp_data <- 
  map2(names(cp_list), cp_list, function(x,y){
    assessment <- x %>%
      str_split("_") %>% 
      map(function(z) paste0(z[2], z[1])) %>% 
      unlist %>% 
      str_replace_all(c("year" = "y", "grade" = "g", "month" = "m"))
    
    y[[1]] %>% 
      select(ID, matches("OMIS|INCR")) %>% 
      rename_with(.cols = c(-ID), ~paste(.x, assessment, sep = "_")) %>% 
      rename_with(tolower)
  }) %>% 
  reduce(full_join, by = "id")

## Stack Variables ----
cp_data_long <- 
  pivot_longer(cp_data, cols = c(-id), names_to = "variable", values_to = "score") %>% 
  separate(variable, c("variable","assessment"), sep = "_") %>% 
  pivot_wider(names_from = variable, values_from = score) %>% 
  mutate(assessment_order = case_when(assessment == "54m" ~ 1, 
                                      assessment == "1g"  ~ 2, 
                                      assessment == "3g"  ~ 3, 
                                      assessment == "4g"  ~ 4,
                                      assessment == "5g"  ~ 5,
                                      assessment == "15y" ~ 6)) %>% 
  arrange(id, assessment_order) %>% 
  select(id, assessment, assessment_order, everything())

# Gather Tower Activity ---------------------------------------------------
## Compile File Names ----
ta_files <- 
  list.files(path = "..", recursive = T, full.names = T) %>% 
  str_subset("Tower.*sav$")

ta_years <-
  ta_files %>% 
  str_split("(/)| ") %>% 
  map(function(x) paste(x[3], x[2], sep = "_")) %>% 
  unlist %>% 
  str_remove("(st|rd|th)$") %>% 
  tolower()

## Create Data List ----
ta_list <- 
  map(ta_files, function(x){
    
    data <- read_sav(x)
    codebook <- create_codebook(data)
    
    list(
      data = data,
      codebook = codebook
    )
  }) %>% 
  set_names(ta_years)

## Planning Scores ----
ta_data <- 
  map2(names(ta_list), ta_list, function(x,y){
    assessment <- x %>%
      str_split("_") %>% 
      map(function(z) paste0(z[2], z[1])) %>% 
      unlist %>% 
      str_replace_all(c("year" = "y", "grade" = "g", "month" = "m"))
    
    y[[1]] %>% 
      select(ID, matches("TOTSCORE|TOT(PP)")) %>% 
      rename_with(.cols = c(-ID), ~paste(.x, assessment, sep = "_")) %>% 
      rename_with(tolower)
  }) %>% 
  reduce(full_join, by = "id") %>% 
  rename(totscore_15y = totppsl_15y) %>% 
  mutate(across(c(-id), ~scale(.x) %>% as.numeric))

## Stack Variables ----
ta_data_long <- 
  pivot_longer(ta_data, cols = c(-id), names_to = "variable", values_to = "score") %>% 
  separate(variable, c("variable","assessment"), sep = "_") %>% 
  pivot_wider(names_from = variable, values_from = score) %>% 
  mutate(assessment_order = case_when(assessment == "54m" ~ 1, 
                                      assessment == "1g"  ~ 2, 
                                      assessment == "3g"  ~ 3, 
                                      assessment == "4g"  ~ 4,
                                      assessment == "5g"  ~ 5,
                                      assessment == "15y" ~ 6)) %>% 
  arrange(id, assessment_order) %>% 
  select(id, assessment, assessment_order, everything())

# Stroop ------------------------------------------------------------------
## Compile File Names ----
sp_files <- 
  list.files(path = "..", recursive = T, full.names = T) %>% 
  str_subset("Stroop.*sav$")

sp_years <-
  sp_files %>% 
  str_split("(/)| ") %>% 
  map(function(x) paste(x[3], x[2], sep = "_")) %>% 
  unlist %>% 
  str_remove("(st|rd|th)$") %>% 
  tolower()

## Create Data List ----
sp_list <- 
  map(sp_files, function(x){
    
    data <- read_sav(x)
    codebook <- create_codebook(data)
    
    list(
      data = data,
      codebook = codebook
    )
  }) %>% 
  set_names(sp_years)

## Planning Scores ----
sp_data <- 
  map2(names(sp_list), ta_list, function(x,y){
    assessment <- x %>%
      str_split("_") %>% 
      map(function(z) paste0(z[2], z[1])) %>% 
      unlist %>% 
      str_replace_all(c("year" = "y", "grade" = "g", "month" = "m"))
    
    y[[1]] %>% 
      select(ID, matches("NTRFT")) %>% 
      rename_with(.cols = c(-ID), ~paste(.x, assessment, sep = "_")) %>% 
      rename_with(tolower)
  }) %>% 
  reduce(full_join, by = "id") %>% 
  mutate(across(c(-id), ~scale(.x) %>% as.numeric))

## Stack Variables ----
ta_data_long <- 
  pivot_longer(ta_data, cols = c(-id), names_to = "variable", values_to = "score") %>% 
  separate(variable, c("variable","assessment"), sep = "_") %>% 
  pivot_wider(names_from = variable, values_from = score) %>% 
  mutate(assessment_order = case_when(assessment == "54m" ~ 1, 
                                      assessment == "1g"  ~ 2, 
                                      assessment == "3g"  ~ 3, 
                                      assessment == "4g"  ~ 4,
                                      assessment == "5g"  ~ 5,
                                      assessment == "15y" ~ 6)) %>% 
  arrange(id, assessment_order) %>% 
  select(id, assessment, assessment_order, everything())
