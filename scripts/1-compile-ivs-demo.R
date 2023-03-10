# Setup -------------------------------------------------------------------
## Libraries ----
library(tidyverse)
library(haven)
library(sjlabelled)

## Custom Functions ----
source("scripts/0-create_codebook.R")

# Read Data ---------------------------------------------------------------
## Get file paths ----
seccyd_ivs_demo_files <- 
  list.files(
    "..", 
    pattern = "^demo(.*).sas7bdat$", 
    full.names = T, 
    recursive = T
  )

## Generate times points from file paths ----
seccyd_ivs_demo_years <-
  seccyd_ivs_demo_files %>% 
  str_split("(/)| ") %>% 
  map(function(x) paste(x[5])) %>% 
  unlist %>% 
  str_remove("\\..*$") %>% 
  tolower()

## Read data into a list ----
seccyd_ivs_demo_list <-
  map(seccyd_ivs_demo_files, function(x){
    
    data <- read_sas(x)
    codebook <- create_codebook(data)
    
    list(
      data = data,
      codebook = codebook
    )
  }) |> 
  set_names(seccyd_ivs_demo_years)

# Select Relevant variables -----------------------------------------------
## Loop through each assesment and grab assessment and merge ----
seccyd_ivs_demo_data1 <- 
  map(seccyd_ivs_demo_list, function(x){
    x[[1]] %>% 
      rename_with(~str_remove(.x, "_") |>  tolower()) |> 
      select(
        id, 
        matches("^(csex|crace|mrace|frace|mage|mstat|meduc|fhome|phome|memps|pemps|moccu|poccu|mhrw|phrw|incnt|hhtyp|adlts|thhsz)m"))
  }) %>% 
  reduce(full_join, by = "id")

## Stack the data per id and assessment ----
seccyd_ivs_demo_data2 <- 
  seccyd_ivs_demo_data1 |> 
  rename_with(.cols = matches("sex|race|edu|age"), ~str_remove(.x, "m01$")) |> 
  pivot_longer(
    c(-id, -matches("sex|race|edu|age")), 
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
    incnt = "Income to needs ratio",
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
  seccyd_ivs_demo_list,
  seccyd_ivs_demo_data1, 
  seccyd_ivs_demo_data2, 
  file = "data/seccyd-ivs-demo.Rdata"
)

## Just the DVs wrangled above
write_csv(seccyd_ivs_demo_data2 |> create_codebook(), "codebooks/seccyd-ivs-demo.csv")
