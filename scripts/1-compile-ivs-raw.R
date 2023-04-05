# Setup -------------------------------------------------------------------
## Libraries ----
library(tidyverse)
library(haven)
library(sjlabelled)
library(lubridate)

## Custom functions ----
source("scripts/0-create_codebook.R")

## Load phone data ----
load("data/seccyd-ivs-phone.Rdata")
load("data/seccyd-ivs-demo.Rdata")

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
  paste0("hh_comp", ... = _) |> 
  str_replace("02$","01")

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

## Assessment factor levels ----
seccyd_assessments <- 
  c(
    "01", "03", "06", "09", "12", "15", 
    "18", "21", "24", "27", "30", "33",
    "36", "42", "46", "50", "54", "60",
    "kf", "ks", "1f", "1s", "2f", "2s",
    "g3", "g4", "g5", "g6", "x4", "x5"
  )

# Rebuild phone analysis data ---------------------------------------------
## put all data into one data.frame ----
seccyd_ivs_changes_data1 <- 
  map2(seccyd_ivs_changes_list, names(seccyd_ivs_changes_list), function(x, y){
    if(y != "hh_comp40"){
      x[[1]] %>% 
        rename_with(tolower) |> 
        select(
          id, 
          any_of(c("brthdats", "rel")),
          matches("^r(.*)_(\\d*)|hhchng\\d\\d")
        ) |> 
        rename_with(.cols = any_of("rel"), ~paste0("rel_", y))
    } else{
      
    }
  }) |> 
  compact() |> 
  reduce(full_join, by = "id")

## Stack data per person, assessment, and change ----
seccyd_ivs_changes_data2 <- 
  seccyd_ivs_changes_data1 |> 
  as_tibble() |> 
  rename_with(.cols = matches("^rel_hh_comp"), ~str_remove(.x, "_hh_comp")) |>
  pivot_longer(c(matches("^r|hh")), names_to = "var", values_to = "value") |> 
  mutate(
    assessment = str_replace(var, "^(r(b|m|ag|sx|ch|el)|hhchng)(\\d\\d)(.*)$", "\\3"),
    var = str_remove(var, "\\d\\d")
  ) |> 
  pivot_wider(names_from = var, values_from = value) |> 
  select(id, brthdats, assessment, hhchng, rel, everything()) |> 
  pivot_longer(c(matches("^r(b|m|ag|sx|ch)")), names_sep = "_", names_to = c("var","number"), values_to = "value") |> 
  pivot_wider(names_from = "var", values_from = "value")

## Recompute partner and father in the home codes ----
raw_phone <- 
  seccyd_ivs_changes_data2 |> 
  mutate(
    assessment = factor(assessment, seccyd_assessments),
    no_change = rch == 1, 
    move_out  = rch == 2,
    move_in   = rch == 3,
    adlt      = if_else(rb %in% c(1, 2, 3, 4, 5), 1, NA),
    fhome     = case_when(rb %in% c(1, 10) & (move_in|no_change|is.na(rch)) ~ 1, 
                          rb %in% c(1, 10) & !(move_in|no_change) ~ 0,
                          T ~ NA),
    phome     = case_when((rm %in% c(1, 2) | rb %in% c(2)) & (move_in|no_change|is.na(rch)) ~ 1, 
                          rm %in% c(1, 2) & !(move_in|no_change) ~ 0, 
                          T ~ NA),
    person    = if_else((!is.na(rb) | !is.na(rm) | !is.na(rsx) | !is.na(rag) | !is.na(rch)) & !move_out, 1, 0)
  ) |> 
  summarize(
    hhchng    = unique(hhchng),
    person_in = mean(move_in, na.rm = T) * sum(!is.na(move_in)),
    no_change = mean(no_change, na.rm = T) * sum(!is.na(no_change)),
    person_out = mean(move_out, na.rm = T) * sum(!is.na(move_out)),
    persons = mean(person, na.rm = T) * sum(!is.na(person)),
    thhsz = persons + 2,
    fhome = mean(fhome, na.rm = T) * sum(!is.na(fhome)),
    phome = mean(phome, na.rm = T) * sum(!is.na(phome)),
    .by = c(id, assessment, rel)
  ) |> 
  arrange(id, assessment) |> 
  mutate(
    across(c(-id,-assessment), ~ifelse(is.nan(.x), NA, .x)),
    fhome = ifelse(hhchng == 0 & is.na(fhome), lag(fhome), fhome),
    phome = ifelse(hhchng == 0 & is.na(phome), lag(phome), phome),
    thhsz = ifelse((!is.na(fhome)|!is.na(phome)) & is.na(thhsz), persons, thhsz),
    across(c(-id,-assessment), ~ifelse(is.na(rel), NA, .x)),
  ) |> 
  mutate(
    fhome = ifelse(hhchng == 0 & is.na(fhome), lag(fhome), fhome),
    phome = ifelse(hhchng == 0 & is.na(phome), lag(phome), phome),
    fhome = ifelse(!(is.na(rel) | is.na(hhchng)) & is.na(fhome), 0, fhome),
    phome = ifelse(!(is.na(rel) | is.na(hhchng)) & is.na(phome), 0, phome),
  )

# Extract employment data -------------------------------------------------
seccyd_ivs_jobs <- 
  seccyd_ivs_changes_list |> 
  map(function(x){
    x$data |> 
      rename_with(tolower) |> 
      select(id, matches("^empld")) 
  }) |> 
  compact() |> 
  reduce(full_join, by = "id") |> 
  select(id, matches("(18|21|27|30|33)$")) |> 
  rename_with(.cols = matches("\\d$"), ~str_replace(.x, "(.*)(\\d\\d)$", "\\1_\\2")) |> 
  pivot_longer(c(-id), names_to = c("var", "assessment"), names_sep = "_", values_to = "value") |> 
  pivot_wider(names_from = "var", values_from = "value") |> 
  rename(memps = empld, pemps = empldp) |> 
  mutate(assessment = factor(assessment, seccyd_assessments))

# Merge Demo and phone ----------------------------------------------------
## Put them all together ----
ivs_data1 <- 
  bind_rows(
    seccyd_ivs_demo_data2 |> 
      select(-c(csex, crace, mrace, frace,  mage, meduc)),
    seccyd_ivs_phone_data2,
    .id = "source"
  ) |> 
  mutate(
    assessment = factor(assessment, levels = seccyd_assessments),
    source     = ifelse(source == 1, "demo", "phone")
  ) |> 
  bind_rows(
    raw_phone |> 
      select(id, assessment, phome, fhome) |> 
      filter(assessment %in% c("18", "21", "27", "30","33")) |> 
      mutate(source = "phone") |> 
      left_join(
        seccyd_ivs_jobs,
        by = c("id", "assessment")
      )
  ) |> 
  left_join(
    seccyd_ivs_demo_data2 |> 
      select(c(id, csex, crace, mrace, frace,  mage, meduc)) |> 
      distinct(),
    by = "id"
  ) |> 
  select(
    id, assessment, source, 
    csex, crace, mrace, frace,  mage, meduc, 
    everything()
  ) |> 
  var_labels(
    id = "site/loc/subject ID",
    assessment = "assessment when information was collected",
    source     = "Phone or in person interview",
    phome      = "Husband/partner lives in household (1 = yes, 0 = no)",
    fhome      = "Father lives in household (1 = yes, 0 = no)",
    hhtyp      = "Household type",
    adlts      = "Number of adults living in home (rel/unrel)",
    thhsz      = "Total household size (children + adults)",
    mstat      = "Mother's marital status",
    incnt      = "Income to needs ratio",
    memps      = "Mother's employment status",
    pemps      = "Partner's employment status",
    moccu      = "Occupation of mother",
    poccu      = "Occupation of partner",
    mhrw       = "hrs/wk mother works-all jobs", 
    phrw       = "hrs/wk partner works"
  ) |> 
  arrange(id, assessment)

# Save data objects -------------------------------------------------------
## All data steps
save(
  ivs_data1,
  file = "data/ivs-data1.Rdata"
)

## Just the IVs wrangled above
write_csv(ivs_data1 |> create_codebook(), "codebooks/ivs_data1.csv")
