# Setup -------------------------------------------------------------------
## Libraries ----
library(tidyverse)
library(haven)
library(sjlabelled)

## Custom Functions ----
source("scripts/0-create_codebook.R")

# Read Data ---------------------------------------------------------------
## Get file paths ----
seccyd_ivs_fam_files <- 
  list.files(
    "..", 
    pattern = "^fam(.*).sas7bdat$", 
    full.names = T, 
    recursive = T
  )

## Generate times points from file paths ----
seccyd_ivs_fam_years <-
  seccyd_ivs_fam_files %>% 
  str_split("(/)| ") %>% 
  map(function(x) paste(x[5])) %>% 
  unlist %>% 
  str_remove("\\..*$") %>% 
  tolower()

## Read data into a list ----
seccyd_ivs_fam_list <-
  map(seccyd_ivs_fam_files, function(x){
    
    data <- read_sas(x)
    codebook <- create_codebook(data)
    
    list(
      data = data,
      codebook = codebook
    )
  }) |> 
  set_names(seccyd_ivs_fam_years)

# Select Relevant variables -----------------------------------------------
## Loop through each assesment and grab assessment and merge ----
seccyd_ivs_fam_data1 <- 
  map(seccyd_ivs_fam_list, function(x){
    x[[1]] %>% 
      rename_with(~str_remove(.x, "_") |>  tolower()) |> 
      select(
        id, 
        matches("^(lfstr|parex|madep|hlthm|negle|neger|posle|poser|hhtot)m"))
  }) %>% 
  reduce(full_join, by = "id")

## Stack the data per id and assessment ----
seccyd_ivs_fam_data2 <- 
  seccyd_ivs_fam_data1 |> 
  pivot_longer(
    c(-id), 
    names_pattern = "(.*)m(..)",
    names_to = c("variable","assessment")
  ) |> 
  pivot_wider(names_from =  "variable", values_from = "value") |> 
  var_labels(
    assessment = "assessment when information was collected",
    madep      = "Mother depression score from CESD",
    hlthm      = "Health of mother",
    hhtot      = "H.O.M.E. total score",
    parex      = "Parenting stress",
    lfstr      = "Total life stress",
    neger      = "Sum of negative event ratings",
    negle      = "Number of negative life events ",
    poser      = "Sum of positive event ratings",
    posle      = "Number of positive life events "
  )

# Save data objects -------------------------------------------------------
## All data steps
save(
  seccyd_ivs_fam_list,
  seccyd_ivs_fam_data1, 
  seccyd_ivs_fam_data2, 
  file = "data/seccyd-ivs-family.Rdata"
)

## Just the IVs wrangled above
write_csv(seccyd_ivs_fam_data2 |> create_codebook(), "codebooks/seccyd-ivs-family.csv")
