# Setup -------------------------------------------------------------------
## Libraries
library(tidyverse)
library(ggdist)

## Load data
walk(
  list.files("data", "^analysis", full.names = T), 
  function(x) load(x, envir =.GlobalEnv)
)

# Plotting aesthetics -----------------------------------------------------
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

## WJ factor levels and labels
wj_order <- 
  c(
    "wj_overall_mean",
    "wj_picvo",
    "wj_vrba",
    "wj_pscmp",
    "wj_appld",
    "wj_memse",
    "wj_incom",
    "wj_memna",
    "wj_lwid",
    "wj_wrdat",
    "wj_calc"
  )

wj_labels <- 
  c(
    "Overall",
    "Picture Vocab",
    "Verbal Analogies",
    "Passage Comprehension",
    "Applied Problems",
    "Short-Term Memory",
    "Auditory Processing",
    "Auditory-Visual Associations",
    "Syymbolic Learning",
    "Unfamilar Words",
    "Calculations"
  )

# WJ distribution figure --------------------------------------------------
## Prep underying data
fig2_data <- 
  dvs_analysis_long |> 
  bind_rows(
    dvs_analysis_long |> 
      summarize(
        mean_score = mean(mean_score, na.r = T),
        wj_subtest = "wj_overall_mean",
        .by = id
      )
  ) |> 
  arrange(id, wj_subtest) |> 
  mutate(
    wj_test_type = case_when(str_detect(wj_subtest, "(memna|memse|vrba|incom|picvo)_mean$") ~ "Cognitive Battery",
                             str_detect(wj_subtest, "(pscmp|appld|lwid|wrdat|calc)_mean$") ~ "Achievement Battery",
                             T~"Overall Performance"),
    wj_test_order = factor(wj_subtest, levels = wj_order, labels = wj_labels) |> fct_rev(),
    side = ifelse(wj_subtest == "wj_overall_mean", "top", "bottom")
  )

## Make the figure
fig2 <- 
  fig2_data |> 
  ggplot(
    aes(
      x = wj_test_order, 
      y = mean_score, 
      fill = wj_test_order, 
      color = wj_test_order)
  ) +
  geom_hline(
    data = fig2_data |> filter(wj_subtest == "wj_overall_mean"),
    aes(yintercept = mean(mean_score, na.rm = T)), 
    color = "#d2d6de"
  ) +
  geom_dotsinterval(side = "bottom", scale = .5, show.legend = F, color = NA) +
  stat_slabinterval(
    side = "top", 
    scale = .5, 
    point_interval = "mean_qi", 
    color = "black", 
    show.legend = F
  ) +
  scale_x_discrete("") +
  scale_y_continuous("Standard Score") +
  scale_fill_manual(values = c( ggpubr::get_palette("npg",k = 10),"#d2d6de")) +
  coord_flip() +
  ggtitle("Woodcock-Johnson Distributions") +
  theme(
    axis.line.y = element_blank(), 
    axis.ticks.y = element_blank()
  )

## Save output
ggsave("figures/fig2-wj-distributions.pdf", fig2, height = 8, width = 5)
