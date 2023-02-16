# Setup -------------------------------------------------------------------
## Libraries
library(tidyverse)
library(ggdist)

## Load data
load("data/seccyd_dvs_wj.Rdata")

## ggplot2 theme
theme_set(
  theme_bw() +
    theme(
      axis.line.y       = element_line(),
      axis.text.y       = element_text(size = rel(1.1)),
      axis.title.y      = element_text(size = rel(1.25), margin = margin(1,0,0,0,"lines")),
      axis.ticks.y      = element_line(),
      axis.text.x       = element_text(size = rel(1.1)),
      axis.title.x      = element_text(size = rel(1.25), margin = margin(1,0,0,0,"lines")),
      axis.line.x       = element_line(),
      panel.border      = element_blank(), 
      panel.spacing.y   = unit(0.5, "lines"),
      plot.margin       = margin(.25,.25,.25,.25,"lines"),
      plot.background   = element_rect(color = NA),
      plot.title        = element_text(size = rel(1.25), hjust = 0.5, margin = margin(0,0,.5,0, "lines")),
      plot.subtitle     = element_blank(),
      panel.grid        = element_line(color = NA),
      strip.background  = element_blank(), 
      strip.placement   = "outside",
      strip.text        = element_text(size = rel(1), angle = 0)
    )
)

# Aggregate Data ----------------------------------------------------------
seccyd_dvs_wj_data3 <- 
  seccyd_dvs_wj_data2 |> 
  group_by(id) |> 
  summarize(
    across(starts_with("wj_"), list(mean = ~ mean(.x, na.rm = T), n = ~sum(!is.na(.x))))
  ) |> 
  mutate(across(everything(), ~ifelse(is.nan(.x), NA, .x))) |> 
  select(id, ends_with("mean")) |> 
  mutate(
    wj_overall_mean = across(starts_with("wj_")) |> rowMeans(na.rm=T)
  )

## WJ factor levels and labels
wj_order <- 
  c(
    "wj_overall_mean",
    "wj_picvo_mean",
    "wj_vrba_mean",
    "wj_pscmp_mean",
    "wj_appld_mean",
    "wj_memse_mean",
    "wj_incom_mean",
    "wj_memna_mean",
    "wj_lwid_mean",
    "wj_wrdat_mean",
    "wj_calc_mean"
  )

wj_labels <- 
  c(
    "Overall Within-Person",
    "Picture Vocab",
    "Verbal Analogies",
    "Passage Comprehension",
    "Applied Problems",
    "Memory for Sentences (STM)",
    "Incomplete Words (auditory)",
    "Memory for Names (LTM)",
    "Letter-Word (VK)",
    "Word Attacl (auditory)",
    "Calculations (math)"
  )

# WJ distribution figure --------------------------------------------------
## Prep underying data
fig_seccyd_subtest_data <- 
  seccyd_dvs_wj_data3 |> 
  pivot_longer(c(-id)) |> 
  mutate(
    wj_test_type = case_when(str_detect(name, "(memna|memse|vrba|incom|picvo)_mean$") ~ "Cognitive Battery",
                             str_detect(name, "(pscmp|appld|lwid|wrdat|calc)_mean$") ~ "Achievement Battery",
                             T~"Global perforamnce"),
    wj_highlight = ifelse(name == "wj_overall_mean", "overall","subtest"),
    wj_test_order = factor(name, levels = wj_order, labels = wj_labels) |> fct_rev(),
    side = ifelse(name == "wj_overall_mean", "top", "bottom")
  )

## Make the figure
fig_wj_scores <- 
  fig_seccyd_subtest_data |> 
  ggplot(aes(x = wj_test_order, y = value, fill = wj_test_order, color = wj_test_order)) +
  geom_hline(
    data = fig_seccyd_subtest_data |> filter(name == "wj_overall_mean"),
    aes(yintercept = mean(value, na.rm = T)), 
    color = "#d2d6de"
  ) +
  geom_dotsinterval(side = "bottom", scale = .5, show.legend = F, color = NA) +
  stat_slabinterval(side = "top", scale = .5, point_interval = "mean_qi", color = "black", show.legend = F) +
  scale_x_discrete("") +
  scale_y_continuous("Standard Score") +
  scale_fill_manual(values = c( ggpubr::get_palette("npg",k = 10),"#d2d6de")) +
  coord_flip() +
  ggtitle("Woodcock-Johnson Distributions") +
  theme(
    axis.line.y = element_blank(), 
    axis.ticks.y = element_blank()
    )

ggsave("archive/figures/wj_distributions.pdf", fig_wj_scores, height = 8, width = 5)
