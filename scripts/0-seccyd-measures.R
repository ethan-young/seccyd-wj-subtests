# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)

# Read raw SECCYD codebook ------------------------------------------------
seccyd_codebook <- 
  read_excel("archive/seccyd-docs/21940-Documentation-Measures_Chart_Phase1-4.xls") |> 
  pivot_longer(-c(1:3), names_to = "age", values_to = "informant") |> 
  rename_with(~c("instrument", "construct", "key","age","informant")) |> 
  filter(!is.na(informant)) |> 
  group_by(instrument, construct, key) |> 
  summarize(
    ages = paste(age, collapse = ", "),
    informant = unique(informant)
  )

# Write the codebook ------------------------------------------------------
save(seccyd_codebook, file = "data/seccyd-full-measure-list.Rdata")
write_csv(seccyd_codebook, "codebooks/seccyd-full-measure-list.csv")
