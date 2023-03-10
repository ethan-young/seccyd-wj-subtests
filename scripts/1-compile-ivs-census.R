# Setup -------------------------------------------------------------------
## Libraries ----
library(tidyverse)
library(haven)
library(sjlabelled)
library(lubridate)

## Custom Functions ----
source("scripts/0-create_codebook.R")

## Look ups for Gini coef in 1990 ----
income1990_lookup <- 
  pivot_longer(
    tibble(
      P0800001 = median(c(   0 ,    5000)),
      P0800002 = median(c(5000 ,    9999)),
      P0800003 = median(c(10000,   12499)),
      P0800004 = median(c(12500,   14999)),
      P0800005 = median(c(15000,   17499)),
      P0800006 = median(c(17500,   19999)),
      P0800007 = median(c(20000,   22499)),
      P0800008 = median(c(22500,   24999)),
      P0800009 = median(c(25000,   27499)),
      P0800010 = median(c(27500,   29999)),
      P0800011 = median(c(30000,   32499)),
      P0800012 = median(c(32500,   34999)),
      P0800013 = median(c(35000,   37499)),
      P0800014 = median(c(37500,   39999)),
      P0800015 = median(c(40000,   42499)),
      P0800016 = median(c(42500,   44999)),
      P0800017 = median(c(45000,   47499)),
      P0800018 = median(c(47500,   49999)),
      P0800019 = median(c(50000,   54999)),
      P0800020 = median(c(55000,   59999)),
      P0800021 = median(c(60000,   74999)),
      P0800022 = median(c(75000,   99999)),
      P0800023 = median(c(100000, 124999)),
      P0800024 = median(c(125000, 149999)),
      P0800025 = median(c(150000        ))
    ),
    everything(),
    names_to = "income_var",
    values_to = "income_cat"
  )

# Read Data ---------------------------------------------------------------
## Get file paths ----
seccyd_ivs_census_files <- 
  list.files(
    "..", 
    pattern = "^(cens|res).*.sas7bdat", 
    full.names = T, 
    recursive = T
  )

## Read data into a list ----
seccyd_ivs_census_list <-
  map(seccyd_ivs_census_files, function(x){
    
    data <- read_sas(x)
    codebook <- create_codebook(data)
    
    list(
      data = data,
      codebook = codebook
    )
  }) |> 
  set_names(c("census_1990","census_2000","res_grid"))

# Select Variables --------------------------------------------------------
## 1990 Census ----
seccyd_ivs_census1990 <- 
  seccyd_ivs_census_list[["census_1990"]]$data |> 
  as_tibble() |> 
  mutate(
    cen1990_pov       = across(matches("P121000(1|2|3|4)$")) |> rowSums(na.rm=T),
    cen1990_pov       = (cen1990_pov / P0010001) * 100,
    cen1990_rent      = ( H0080002 / H0040001 ) * 100,
    cen1990_assist    =  ( P0950001 / P0010001 ) * 100,
    cen1990_no_degree = across(matches("P057")) |> rowSums(na.rm=T),
    cen1990_no_degree = ((P0570001 + P0570002)/cen1990_no_degree) * 100,
    cen1990_unemploy  = across(matches("P07000")) |> rowSums(na.rm = T),
    cen1990_unemploy  = ((P0700003 + P0700007)/cen1990_unemploy) * 100
  ) |> 
  rename(
    cen1990_pop          = P0010001,
    cen1990_households   = P0050001,
    cen1990_med_income89 = P107A001,
    cen1990_med_income91 = P080A001,
  ) |> 
  pivot_longer(
    matches("P0800"), 
    names_to = "income_var", 
    values_to = "income_n"
  ) |> 
  left_join(
    income1990_lookup,
    by = "income_var"
  ) |> 
  mutate(
    cen1990_gini = DescTools::Gini(income_cat, income_n), .by = c(ID, BLKGRPID)
  ) |> 
  rename_with(tolower) |> 
  select(ends_with("id"), ends_with("date"), starts_with("cen")) |> 
  distinct(id, blkgrpid, strtdate, .keep_all = TRUE) |> 
  var_labels(
    cen1990_pop          = "Total number of people living in the census block",
    cen1990_households   = "Total number of households in the census block",
    cen1990_med_income89 = "Median household income in 1989",
    cen1990_med_income91 = "Median household income in 1991",
    cen1990_gini         = "Gini coefficient for census tract based income frequencies across census income categories",
    cen1990_pov          = "Percent of people living at (1.00-1.24) or below poverty line",
    cen1990_rent         = "Percent of occupied houses that are renter occupied",
    cen1990_assist       = "Percent of people recieving public assistance",
    cen1990_no_degree    = "Percent of people 25 and older who have no diploma or GED",
    cen1990_unemploy     = "Percent of people 16 and over who are unemployed"
  )

# Save data objects -------------------------------------------------------
## All data steps
save(
  seccyd_ivs_census_list,
  seccyd_ivs_census1990, 
  file = "data/seccyd-ivs-census.Rdata"
)

## Just the IVs wrangled above
write_csv(seccyd_ivs_census1990 |> create_codebook(), "codebooks/seccyd-ivs-census.csv")