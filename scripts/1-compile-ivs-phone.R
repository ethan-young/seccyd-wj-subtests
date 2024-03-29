# Setup -------------------------------------------------------------------
## Libraries ----
library(tidyverse)
library(haven)
library(sjlabelled)

## Custom Functions ----
source("scripts/0-create_codebook.R")

# Read Data ---------------------------------------------------------------
## Get file paths ----
seccyd_ivs_phone_files <- 
  list.files(
    "..", 
    pattern = "^phon(.*).sas7bdat$", 
    full.names = T, 
    recursive = T
  )

## Generate times points from file paths ----
seccyd_ivs_phone_years <-
  seccyd_ivs_phone_files %>% 
  str_split("(/)| ") %>% 
  map(function(x) paste(x[6])) %>% 
  unlist %>% 
  str_remove("\\..*$") %>% 
  tolower()

## Read data into a list ----
seccyd_ivs_phone_list <-
  map(seccyd_ivs_phone_files, function(x){
    
    data <- read_sas(x)
    codebook <- create_codebook(data)
    
    list(
      data = data,
      codebook = codebook
    )
  }) |> 
  set_names(seccyd_ivs_phone_years)

# Select Relevant variables -----------------------------------------------
## Loop through each assesment and grab assessment and merge ----
seccyd_ivs_phone_data1 <- 
  map(seccyd_ivs_phone_list, function(x){
    x[[1]] %>% 
      rename_with(~str_remove(.x, "_") |>  tolower()) |> 
      select(
        id, 
        matches("^(mstat|meduc|fhome|phome|memp(s|)|pemp(s|)|moccu|poccu|mhrw|phrw|incnt|hhtyp|adlts|thhsz)m")
      ) |> 
      rename_with(.cols = any_of(matches("pempm")), ~str_replace(.x, "pemp(.*)", "pemps\\1"))
  }) %>% 
  reduce(full_join, by = "id")

## Stack the data per id and assessment ----
seccyd_ivs_phone_data2 <- 
  seccyd_ivs_phone_data1 |> 
  pivot_longer(
    c(-id), 
    names_pattern = "(.*)m(..)",
    names_to = c("variable","assessment")
  ) |> 
  pivot_wider(names_from =  "variable", values_from = "value") |> 
  var_labels(
    id = "site/loc/subject ID",
    assessment = "assessment when information was collected",
    phome = "Husband/partner lives in household (1 = yes, 0 = no)",
    fhome = "Father lives in household (1 = yes, 0 = no)",
    hhtyp = "Household type",
    adlts = "Number of adults living in homw (rel/unrel)",
    thhsz = "Total household size (children + adults)",
    mstat = "Mother's marital status",
    memps = "Mother's employment status",
    pemps = "partner's employment status",
    moccu = "Occupation of mother",
    poccu = "Occupation of partner",
    mhrw = "hrs/wk mother works-all jobs", 
    phrw = "hrs/wk partner works"
  )

# Save data objects -------------------------------------------------------
## All data steps
save(
  seccyd_ivs_phone_list,
  seccyd_ivs_phone_data1, 
  seccyd_ivs_phone_data2, 
  file = "data/seccyd-ivs-phone.Rdata"
)

## Just the IVs wrangled above
write_csv(seccyd_ivs_phone_data2 |> create_codebook(), "codebooks/seccyd-ivs-phone.csv")
