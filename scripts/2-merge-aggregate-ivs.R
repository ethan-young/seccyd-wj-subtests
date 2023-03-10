# Setup -------------------------------------------------------------------
## Libraries ----
library(tidyverse)
library(lubridate)
library(sjlabelled)

## Custom functions ----
source("scripts/0-create_codebook.R")
source("scripts/0-corr_table.R")

## Load Data ----
### Get date when first wj assessment took place
first_wj_date <- 
  list.files("..", pattern = "f55l54_a.sav", full.names = T, recursive = T) |> 
  haven::read_sav() |> 
  rename_with(tolower) |> 
  select(id, intdt55l)

### Get all compiled iv data
walk(
  list.files("data", "ivs", full.names = T), 
  function(x) load(x, envir =.GlobalEnv)
)

# Combine demo, family, and phone data  -----------------------------------
## Order levels for assessments ----
### Missing these phone assessments
#### 18 
#### 21, 
#### 30, 
#### 33 
seccyd_assessments <- 
  c(
    "01", "03", "06", "09", "12", "15", 
    "21", "24", "30", "33", "36", "42", 
    "46", "50", "54", "60", "kf", "ks", 
    "1f", "1s", "2f", "2s", "g3", "g4", 
    "g5", "g6", "x4", "x5"
  )

## Combine phone and demo ----
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
  )

## Merge family data with demo & phone ----
ivs_data2 <- 
  ivs_data1 |> 
  left_join(
    seccyd_ivs_fam_data2 |> 
      mutate(
        assessment = factor(assessment, levels = seccyd_assessments)
      ),
    by = c("id", "assessment")
  ) |> 
  arrange(id, assessment) |> 
  filter(as.numeric(assessment) <= 12)


# Compute aggregated variables --------------------------------------------
ivs_unp_home <- 
  ivs_data2 |> 
  mutate(
    n_missing = sum(is.na(phome)) + sum(is.na(fhome)),
    lag  = accumulate(is.na(phome), ~ (.x + .y) * .y),
    lag1 = is.na(lag(phome)),
    lag2 = is.na(lag(phome, 2)),
    lag3 = is.na(lag(phome, 3)),
    lag4 = is.na(lag(phome, 4)),
    lag5 = is.na(lag(phome, 5)),
    lag6 = is.na(lag(phome, 6)),
    partner_transition = ifelse(lag(phome) == phome & lag(phome) == phome, 0, 1),
    partner_transition = ifelse(lag1 & lag(phome, 2) != phome, 1, partner_transition),
    partner_transition = ifelse(lag1 & lag2 & lag(phome, 3) != phome, 1, partner_transition),
    partner_transition = ifelse(lag1 & lag2 & lag3 & lag(phome, 4) != phome, 1, partner_transition),
    partner_transition = ifelse(lag1 & lag2 & lag3 & lag4 & lag(phome, 5) != phome, 1, partner_transition),
    partner_transition = ifelse(lag1 & lag2 & lag3 & lag4 & lag5 & lag(phome, 6) != phome, 1, partner_transition),
    lag1 = is.na(lag(fhome)),
    lag2 = is.na(lag(fhome, 2)),
    lag3 = is.na(lag(fhome, 3)),
    lag4 = is.na(lag(fhome, 4)),
    lag5 = is.na(lag(fhome, 5)),
    lag6 = is.na(lag(fhome, 6)),
    father_transition = ifelse(lag(fhome) == fhome & lag(fhome) == fhome, 0, 1),
    father_transition = ifelse(lag1 & lag(fhome, 2) != fhome, 1, father_transition),
    father_transition = ifelse(lag1 & lag2 & lag(fhome, 3) != fhome, 1, father_transition),
    father_transition = ifelse(lag1 & lag2 & lag3 & lag(fhome, 4) != fhome, 1, father_transition),
    father_transition = ifelse(lag1 & lag2 & lag3 & lag4 & lag(fhome, 5) != fhome, 1, father_transition),
    father_transition = ifelse(lag1 & lag2 & lag3 & lag4 & lag5 & lag(fhome, 6) != fhome, 1, father_transition),
    transitions = across(c(partner_transition, father_transition)) |> rowSums(na.rm = T),
    transitions = ifelse(transitions == 2 & phome == fhome & lag(phome) == lag(fhome), 1, transitions),
    mjob_change = ifelse(lag(memps) == memps, 0, 1),
    pjob_change = ifelse(lag(pemps) == pemps, 0, 1),
    pjob_change = ifelse(transitions > 0, NA, pjob_change),
    .by = id
  ) |> 
  summarize(
    across(c(-id, -assessment), ~sum(is.na)),
    transitions_n    = mean(transitions, na.rm = T) * sum(!is.na(transitions)),
    mjob_changes_n   = mean(mjob_change, na.rm = T) * sum(!is.na(mjob_change)),
    pjob_changes_n   = mean(pjob_change, na.rm = T) * sum(!is.na(pjob_change)),
    job_changes_mean = mean(c(mjob_changes_n, pjob_changes_n), na.rm = T),
    mhrs_mean        = mean(mhrw, na.rm = T),
    mhrs_sd          = sd(mhrw, na.rm = T),
    phrs_mean        = mean(phrw, na.rm = T),
    phrs_sd          = sd(phrw, na.rm = T),
    adults_mean      = mean(adlts, na.rm = T),
    aduts_sd         = sd(adlts, na.rm = T),
    incnt_mean       = mean(incnt, na.rm = T),
    incnt_sd         = sd(incnt, na.rm = T),
    madep_mean       = mean(madep, na.rm = T),
    madep_sd         = sd(madep, na.rm = T),
    .by = id
  )

ivs_unp_home |> select(-id) |> corr_table(numbered = T, stats = c("mean","sd", "min","max", "skew","kurtosis"))

# Compute Residential moves data ------------------------------------------
seccyd_ivs_census1990 |>
  filter(
    strtdate <= ymd("1996-12-31"),
    enddate <= ymd("1996-12-31") | is.na(enddate),
    id == 12
  ) |
  summarize(
    n_homes = n_distinct(strtdate),
    n_moves = n_distinct(enddate, na.rm = T),
    .by = id
  )
