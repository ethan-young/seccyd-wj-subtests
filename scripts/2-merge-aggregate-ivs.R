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
  select(id, intdt55l) |> 
  rename(wj_date = intdt55l)

### Get all compiled iv data
walk(
  list.files("data", "ivs", full.names = T), 
  function(x) load(x, envir =.GlobalEnv)
)

# Combine demo, family, and phone data  -----------------------------------
## Order levels for assessments ----
seccyd_assessments <- 
  c(
    "01", "03", "06", "09", "12", "15", 
    "18", "21", "24", "27", "30", "33",
    "36", "42", "46", "50", "54", "60", 
    "kf", "ks", "1f", "1s", "2f", "2s",
    "g3", "g4", "g5", "g6", "x4", "x5"
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
  arrange(id, assessment)

# Family/Home aggregated variables ----------------------------------------
## Partner transitions ----
ivs_partner_transitions <- 
  ivs_data2 |> 
  select(id, assessment, mstat, adlts, hhtyp, phome, fhome, memps, pemps) |> 
  filter(assessment %in% seccyd_assessments[1:17]) |> 
  mutate(
    f_lag1  = is.na(lag(fhome)),
    f_lag2 = f_lag1 & is.na(lag(fhome, 2)),
    f_lag3 = f_lag1 & f_lag2 & is.na(lag(fhome, 3)),
    f_lag4 = f_lag1 & f_lag2 & f_lag3 & is.na(lag(fhome, 4)),
    f_lag5 = f_lag1 & f_lag2 & f_lag3 & f_lag4 & is.na(lag(fhome, 5)),
    p_lag1  = is.na(lag(phome)),
    p_lag2 = p_lag1 & is.na(lag(phome, 2)),
    p_lag3 = p_lag1 & p_lag2 & is.na(lag(phome, 3)),
    p_lag4 = p_lag1 & p_lag2 & p_lag3 & is.na(lag(phome, 4)),
    p_lag5 = p_lag1 & p_lag2 & p_lag3 & p_lag4 & is.na(lag(phome, 5)),
    fchange = ifelse(fhome != lag(fhome), 1, 0),
    fchange = ifelse(f_lag1 & lag(fhome, 2) != fhome, 1, fchange),
    fchange = ifelse(f_lag1 & f_lag2 & lag(fhome, 3) != fhome, 1, fchange),
    fchange = ifelse(f_lag1 & f_lag2 & f_lag3 & lag(fhome, 4) != fhome, 1, fchange),
    fchange = ifelse(f_lag1 & f_lag2 & f_lag3 & f_lag4 & lag(fhome, 5) != fhome, 1, fchange),
    fchange = ifelse(f_lag1 & f_lag2 & f_lag3 & f_lag4 & f_lag5 & lag(fhome, 6) != fhome, 1, fchange),
    pchange = ifelse(phome != lag(phome), 1, 0),
    pchange = ifelse(p_lag1 & lag(phome, 2) != phome, 1, pchange),
    pchange = ifelse(p_lag1 & p_lag2 & lag(phome, 3) != phome, 1, pchange),
    pchange = ifelse(p_lag1 & p_lag2 & p_lag3 & lag(phome, 4) != phome, 1, pchange),
    pchange = ifelse(p_lag1 & p_lag2 & p_lag3 & p_lag4 & lag(phome, 5) != phome, 1, pchange),
    pchange = ifelse(p_lag1 & p_lag2 & p_lag3 & p_lag4 & p_lag5 & lag(phome, 6) != phome, 1, pchange),
    partner_na     = across(c(pchange, fchange), ~!is.na(.x)) |> rowSums(),
    partner_change = across(c(pchange, fchange)) |> rowMeans(na.rm = T) * partner_na,
    partner_change = ifelse(partner_change == 2 & phome == fhome & lag(phome) == lag(fhome), 1, partner_change),
    me_lag1  = is.na(lag(memps)),
    me_lag2 = me_lag1 & is.na(lag(memps, 2)),
    me_lag3 = me_lag1 & me_lag2 & is.na(lag(memps, 3)),
    me_lag4 = me_lag1 & me_lag2 & me_lag3 & is.na(lag(memps, 4)),
    me_lag5 = me_lag1 & me_lag2 & me_lag3 & me_lag4 & is.na(lag(memps, 5)),
    pe_lag1  = is.na(lag(pemps)),
    pe_lag2 = pe_lag1 & is.na(lag(pemps, 2)),
    pe_lag3 = pe_lag1 & pe_lag2 & is.na(lag(pemps, 3)),
    pe_lag4 = pe_lag1 & pe_lag2 & pe_lag3 & is.na(lag(pemps, 4)),
    pe_lag5 = pe_lag1 & pe_lag2 & pe_lag3 & pe_lag4 & is.na(lag(pemps, 5)),
    mjchange = ifelse(memps != lag(memps), 1, 0),
    mjchange = ifelse(me_lag1 & lag(memps, 2) != memps, 1, mjchange),
    mjchange = ifelse(me_lag1 & me_lag2 & lag(memps, 3) != memps, 1, mjchange),
    mjchange = ifelse(me_lag1 & me_lag2 & me_lag3 & lag(memps, 4) != memps, 1, mjchange),
    mjchange = ifelse(me_lag1 & me_lag2 & me_lag3 & me_lag4 & lag(memps, 5) != memps, 1, mjchange),
    mjchange = ifelse(me_lag1 & me_lag2 & me_lag3 & me_lag4 & me_lag5 & lag(memps, 6) != memps, 1, mjchange),
    pjchange = ifelse(pemps != lag(pemps), 1, 0),
    pjchange = ifelse(pe_lag1 & lag(pemps, 2) != pemps, 1, pjchange),
    pjchange = ifelse(pe_lag1 & pe_lag2 & lag(pemps, 3) != pemps, 1, pjchange),
    pjchange = ifelse(pe_lag1 & pe_lag2 & pe_lag3 & lag(pemps, 4) != pemps, 1, pjchange),
    pjchange = ifelse(pe_lag1 & pe_lag2 & pe_lag3 & pe_lag4 & lag(pemps, 5) != pemps, 1, pjchange),
    pjchange = ifelse(pe_lag1 & pe_lag2 & pe_lag3 & pe_lag4 & pe_lag5 & lag(pemps, 6) != pemps, 1, pjchange),
    pjchange = ifelse(partner_change > 0, NA, pjchange),
    .by = id
  ) |> 
  summarize(
    partner_changes_n  = sum(!is.na(partner_change)),
    mjob_changes_n     = sum(!is.na(mjchange)),
    pjob_changes_n     = sum(!is.na(pjchange)),
    mjob_changes       = mean(mjchange, na.rm = T) * mjob_changes_n,
    pjob_changes       = mean(pjchange, na.rm = T) * pjob_changes_n,
    partner_changes    = mean(partner_change, na.rm = T) * partner_changes_n,
    partner_changes_na = sum(is.na(partner_change)),
    mjob_changes_na    = sum(is.na(mjchange)),
    pjob_changes_na    = sum(is.na(pjchange)),
    job_changes_mean   = mean(c(mjob_changes, pjob_changes), na.rm = T),
    .by = id
  ) |> 
  mutate(
    across(everything(), ~ifelse(is.nan(.x), NA, .x))
  )

## Income to needs ----
ivs_incnt <- 
  ivs_data2 |> 
  filter(
    assessment %in% c("01", "06", "15", "24", "36", "54")
  ) |> 
  summarize(
    incnt_mean = mean(incnt, na.rm = T),
    incnt_sd   = sd(incnt, na.rm = T),
    incnt_na   = sum(is.na(incnt)),
    .by = id
  ) |> 
  select(
    id, starts_with("incnt")
  )

## Maternal Depression ----
ivs_madep <- 
  ivs_data2 |> 
  filter(
    assessment %in% c("01", "15", "36", "54"),
  ) |> 
  summarize(
    madep_mean = mean(madep, na.rm = T),
    madep_sd   = sd(madep, na.rm = T),
    madep_na  = sum(is.na(madep)),
    .by = id
  ) |> 
  select(
    id, starts_with("madep")
  )

# Residential and Census aggregation --------------------------------------
## limit data to before the first WJ assessment ----
ivs_census <- 
  seccyd_ivs_census1990 |>
  full_join(
    first_wj_date,
    by = "id"
  ) |> 
  mutate(
    wj_date = if_else(is.na(wj_date), max(wj_date, na.rm = T), wj_date)
  ) 

## Compute neigborhood variables ----
ivs_neigh <- 
  ivs_census |> 
  filter(
    strtdate <= wj_date
  ) |> 
  mutate(
    enddate = if_else(enddate > max(wj_date, na.rm = T), NA, enddate)
  ) |> 
  summarize(
    homes_n            = n_distinct(strtdate, na.rm = T),
    moves_n            = n_distinct(enddate, na.rm = T),
    cen_block_n        = n_distinct(blkgrpid, na.rm = T),
    cen_pov_mean       = mean(cen1990_pov, na.rm = T),
    cen_inc_mean       = mean(cen1990_med_income91, na.rm = T) * -1,
    cen_gini_mean      = mean(cen1990_gini, na.rm = T),
    cen_rent_mean      = mean(cen1990_rent, na.rm = T),
    cen_unem_mean      = mean(cen1990_unemploy, na.rm = T),
    cen_no_degree_mean = mean(cen1990_no_degree, na.rm = T),
    cen_pov_sd         = sd(cen1990_pov, na.rm = T),
    cen_inc_sd         = sd(cen1990_med_income91, na.rm = T),
    cen_gini_sd        = sd(cen1990_gini, na.rm = T),
    cen_rent_sd        = sd(cen1990_rent, na.rm = T),
    cen_unem_sd        = sd(cen1990_unemploy, na.rm = T),
    cen_no_degree_sd   = sd(cen1990_no_degree, na.rm = T),
    .by = id
  ) |> 
  mutate(
    across(ends_with("sd"), ~ifelse(homes_n == 1, 0, .x)),
    neigh_harsh = across(ends_with("mean"), ~scale(.x)) |> rowMeans(na.rm = T),
    neigh_unp   = across(ends_with("sd"), ~scale(.x)) |> rowMeans(na.rm = T)
  ) |> 
  select(
    id, homes_n, moves_n, starts_with("cen_"), neigh_harsh, neigh_unp
  )

# Put together all ivs ----------------------------------------------------
ivs_analysis <- 
  list(
    ivs_data2 |> 
      select(id, 4:9) |> 
      distinct(),
    ivs_partner_transitions |> 
      select(id, ends_with("changes"), job_changes_mean, ends_with("na")),
    ivs_madep, 
    ivs_incnt, 
    ivs_neigh
  ) |> 
  reduce(full_join, by = "id") |> 
  mutate(
    family_unp = across(c(partner_changes, moves_n, job_changes_mean), ~scale(.x)) |> rowMeans(na.rm = T)
  ) |>
  relocate(family_unp, .after = pjob_changes_na) |> 
  var_labels(
    id                 = "Child identification number",
    mjob_changes       = "Number of mother job transitions ",
    pjob_changes       = "Number of father/partner job transitions ",
    partner_changes    = "Number of partners moving in and out",
    partner_changes_na = "Number of missing partner change scores",
    mjob_changes_na    = "Number of missing mother job scores",
    pjob_changes_na    = "Number of missing partnner job scores",
    job_changes_mean   = "Mean of mother and partner job changes",
    family_unp         = "Mean of job_changes_mean, partner_changes, and moves_n (all z-scored before averaging)",
    madep_mean         = "Mean maternal depression score",
    madep_sd           = "Standard deviation of maternal depression",
    madep_na           = "Number of missing depression scores",
    incnt_mean         = "Mean income-to-needs ratio",
    incnt_sd           = "Standard deviation of income-to-needs ratio",
    incnt_na           = "Number of missing income-to-needs scores",
    homes_n            = "Number of unique homes lived in",
    moves_n            = "Number of moves to a new home",
    cen_block_n        = "Number of different census blocks lived in",
    cen_pov_mean       = "Mean percent of people living in poverty across census blocks",
    cen_inc_mean       = "Mean median household income in 1991 across census blocks",
    cen_gini_mean      = "Mean Gini coefficient for census tract based income frequencies across census income categories",
    cen_rent_mean      = "Mean Percent of occupied houses that are renter occupied",
    cen_unem_mean      = "Mean Percent of people 16 and over who are unemployed",
    cen_no_degree_mean = "Mean Percent of people 25 and older who have no diploma or GED",
    cen_pov_sd         = "Standard deviation of the percent of people living in poverty across census blocks",
    cen_inc_sd         = "Standard deviation of the median household income in 1991 across census blocks",
    cen_gini_sd        = "Standard deviation of the Gini coefficient for census tract based income frequencies across census income categories",
    cen_rent_sd        = "Standard deviation of the Percent of occupied houses that are renter occupied",
    cen_unem_sd        = "Standard deviation of the Percent of people 16 and over who are unemployed",
    cen_no_degree_sd   = "Standard deviation of the Percent of people 25 and older who have no diploma or GED",
    neigh_harsh        = "The mean of cen_pov_mean, cen_inc_mean (reversed), cen_gini_mean, cen_rent_mean, cen_unem_mean, and cen_no_degree_mean",
    neigh_unp          = "The mean of cen_pov_sd, cen_inc_sd, cen_gini_sd, cen_rent_sd, cen_unem_sd, and cen_no_degree_sd",
  )

# Save data ---------------------------------------------------------------
save(
  ivs_analysis,
  ivs_data1,
  ivs_data2,
  ivs_partner_transitions,
  ivs_census,
  ivs_incnt,
  ivs_madep,
  ivs_neigh,
  file = "data/analysis-ivs.Rdata"
)

## Just the IVs wrangled above
write_csv(ivs_analysis |> create_codebook(), "codebooks/analysis-ivs.csv")

