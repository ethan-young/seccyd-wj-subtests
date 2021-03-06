# Setup -------------------------------------------------------------------
## Libraries ----
library(tidyverse)
library(haven)
library(sjlabelled)

## Load Functions ----
source("scripts/0-create_codebook.R")
source("scripts/0-corr_table.R")

# Gather Woodcock Johnson data -------------------------------------------------------------
## Compile file names ----
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

## Create data list ----
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

## Compile standard scores ----
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

## Stack and label variables ----
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
  select(id, assessment, assessment_order, everything(), -math, -read) %>% 
  var_labels(
    assessment = "Assessment when test was administered",
    assessment_order = "Numeric assessment label from earliest to latest.",
    picvo = "Picture Vocabulary standard score. 
             Measures the ability to recognize or to name pictured objects. 
             Higher scores = more verbal comprehension/crystallized knowledge.",
    vrba  = "Verbal Analogies standard score. 
             Measures the ability to complete phrases with words that indicate appropriate analogies. 
             Higher scores = more verbal fluid reasoning and crystallized knowledge.",
    pscmp = "Passage Comprehension standard score.
             The first four items require pointing a  picture represented by a phrase. 
             The remaining items require reading a short passage and identifying a missing key word. 
             Higher scores = more vocab and comprehension skill.",
    appld = "Applied Problems standard score.
             Measures practical problem solving in mathematics. 
             Higher scores = more practical math problem solving skill.",
    memse = "Memory for Sentences standard score.
             Measures the ability to remember and repeat simple words, phrases, and sentences presented auditorily.
             Higher scores = better short-term memory and comprehension.",
    incom = "Incomplete Words standard score.
             After hearing a recorded word that has one or more phonemes missing, the subject names the complete word. 
             Higer scores = better auditory processing.",
    memna = "Memory for Names standard score. 
             Measures the ability to learn associations between unfamiliar auditory and visual stimuli, which increases in diffculty.
             Higher scores = better long-term memory retrieval",
    lwid  = "Letter-Word Identification standard score.
             The first five items measure the ability to match a pictograph of a word with an actual picture.
             The remaining items measure reading identification skill in identifying isolated letters and words.
             Higher scores = more verbal knowledge.",
    wrdat = "Word Attack standard score. 
             Measures the subject’s ability pronunciate unfamiliar printed words.
             The test involves reading letter combinations that do not form actual words.
             Higher scores mean better auditory processing.",
    calc  = "Calculations standard score. 
             Measures mathematical calculation performance.
             Calcuations include addition, subtraction, multiplication, division, and combinations of eaach.
             It also includes geometric, trigonometric, logarithmic, and calculus operations.
             Higher scores mean higher math calculation performance."
  )

# Save data objects -------------------------------------------------------


