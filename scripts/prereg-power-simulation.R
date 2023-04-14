# Setup -------------------------------------------------------------------
## Libraries ----
library(tidyverse)
library(lmerTest)
library(parameters)
library(interactions)
library(future)

## Sourced data & scripts ----
source("scripts/0-corr_table.R")
load("data/seccyd_dvs_wj.Rdata")

## Set seed for reproducibility----
set.seed(29837460)

## Set ggplot2 theme ----
theme_set(
  theme_light() +
    theme(
      text = element_text(size = 12),
      axis.line = element_line(),
      title = element_text(size = 12),
      panel.border = element_blank(),
      panel.background = element_rect(color = NA),
      panel.grid = element_blank(),
      plot.background = element_rect(color = NA),
      plot.title = element_text(hjust = .5),
      plot.subtitle = element_text(hjust = .5),
      strip.background = element_rect(color = NA, fill = NA),
      strip.text = element_text(color = "black", hjust = 0.5, size = 10)
    )
)

# Data prep ---------------------------------------------------------------
## Average test scores over time per subtest
seccyd_dvs_wj_data3 <- 
  seccyd_dvs_wj_data2 |> 
  group_by(id) |> 
  summarize(
    across(starts_with("wj_"), list(mean = ~ mean(.x, na.rm = T), n = ~sum(!is.na(.x))))
  ) |> 
  mutate(across(everything(), ~ifelse(is.nan(.x), NA, .x))) |>
  mutate(across(starts_with("wj"), ~scale(.x) |> as.numeric())) |> 
  select(id, ends_with("mean")) 

# Simulate Adversity Effects ----------------------------------------------
## hypothetical betas
sim_adv_effects <- seq(-.15, .15, .025) |> round(3)
sim_adv_effect_mean <- -.2
sigma <- c(.5, 1)

## custom function for looking at betas
rnorm_fixed <- function(n, mean, sd, fixed_beta) {
  # force the mean and sd to be exactly as specified
  my_betas <- as.vector(mean + sd * scale(rnorm(n)))
  
  # difference between the mean of effects and the fixed beta differnence
  my_beta_diff <- my_betas[1] - (mean + fixed_beta)
  
  # Make the first value of the effects the fixed beta
  my_betas[1] <- mean + fixed_beta
  
  # randomly pick an effect to absorb the new value to keep the fixed mean
  random_replace <- sample(2:10, 1)
  
  # add the difference back to the vector
  my_betas[random_replace] <- my_betas[random_replace] + my_beta_diff
  
  # return the betas
  my_betas
}

## plan a multicore session
plan(multisession, workers = 10)

## Simulate the effect of adversity using the betas above
sim_mods <- 
  furrr::future_map(1:500, function(x){
    furrr::future_map(sigma, function(y){
      furrr::future_map(sim_adv_effects, function(z){
        # Generate simulation effects
        effect_sizes <- rnorm_fixed(n = 10, mean =  sim_adv_effect_mean, sd = .05, fixed_beta = z)
        
        # Simulate data with effects
        sim_seccyd_data <- 
          seccyd_dvs_wj_data3 |> 
          transmute(
            id = id,
            adversity = rnorm(n()) |> scale() |> as.numeric(),
            wj_picvo_mean = wj_picvo_mean + adversity*effect_sizes[1]  + rnorm(n(), sd = y),
            wj_vrba_mean  = wj_vrba_mean  + adversity*effect_sizes[2]  + rnorm(n(), sd = y),
            wj_pscmp_mean = wj_pscmp_mean + adversity*effect_sizes[3]  + rnorm(n(), sd = y),
            wj_appld_mean = wj_appld_mean + adversity*effect_sizes[4]  + rnorm(n(), sd = y),
            wj_memse_mean = wj_memse_mean + adversity*effect_sizes[5]  + rnorm(n(), sd = y),
            wj_incom_mean = wj_incom_mean + adversity*effect_sizes[6]  + rnorm(n(), sd = y),
            wj_memna_mean = wj_memna_mean + adversity*effect_sizes[7]  + rnorm(n(), sd = y),
            wj_lwid_mean  = wj_lwid_mean  + adversity*effect_sizes[8]  + rnorm(n(), sd = y),
            wj_wrdat_mean = wj_wrdat_mean + adversity*effect_sizes[9]  + rnorm(n(), sd = y),
            wj_calc_mean  = wj_calc_mean  + adversity*effect_sizes[10] + rnorm(n(), sd = y)
          ) |> 
          pivot_longer(c(-id, -adversity), names_to = c("wj_sub_test"), values_to = c("score")) |> 
          mutate(
            wj_sub_test1 = faux::contr_code_sum(wj_sub_test)
          )
        
        # Run model
        lmer(score ~ adversity*wj_sub_test1 + (1|id), data = sim_seccyd_data) |> 
          broom.mixed::tidy() |> 
          mutate(
            sim = x, 
            mean_effect = sim_adv_effect_mean,
            beta_diff = z,
            sigma = y
          )
        
      })
    })
  })

## collate results
sim_mod_df <- 
  flatten(sim_mods) |> 
  flatten() |> 
  list_rbind()

## go pack to sequential
plan(sequential)

# Plot the power curves ---------------------------------------------------
power_curve <- 
  sim_mod_df |> 
  mutate(
    term = str_replace_all(term, "wj_sub_test1.(wj_.*_mean)-intercept","\\1")
  ) |> 
  select(term, estimate, p.value, mean_effect, sigma, beta_diff) |> 
  filter(str_detect(term, "adversity:wj_picvo")) |> 
  mutate(across(where(is.numeric), ~round(.x, 4))) |> 
  group_by(term, sigma, mean_effect, beta_diff) |> 
  summarize(power = sum((p.value<.05)/500)) |> 
  ggplot(aes(x =  beta_diff, y =  power)) +
  geom_line(aes(group = sigma)) +
  geom_point() +
  geom_hline(aes(yintercept = .80), linetype = "dashed") +
  scale_x_continuous(breaks = seq(-.15, .15, by = .05) |> round(2)) +
  scale_y_continuous(breaks = seq(0, 1, by = .2)) +
  facet_wrap(~sigma, nrow = 2) +
  labs(
    title = "Power for Interaction Effects for N = 1156",
    subtitle = "Rows plot power for lower (.5) versus higher (1) error",
    x = "Interaction Effect (Standardized B)",
    y = "Power"
  )

# Save power analysis objects ---------------------------------------------
save(
  sim_mod_df,
  power_curve,
  file = "preregistration/power-analysis/power-objects.Rdata"
)

