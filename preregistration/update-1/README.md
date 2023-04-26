# Preregistration Update - Income-to-Needs Follow up

``` r
# Libraries
library(tidyverse)
library(broom)

# set directory to root directory
knitr::opts_knit$set(root.dir = "../../")
```

*Last updated on Wednesday, April 26, 2023 at 11:34 AM*

## Background

Our primary analyses revewaled a strange result for our model of
income-to-needs variablity. This update is to report a plan for
following up this result with a set of secondary analyses.

Our model unpacking the effect of income-to-needs variablity on WJ
overall and subtest performance raised questions about the validity of
income-to-needs variability scores as indicators of unpredictability. In
short, simple effects suggest that the subtests that were reduced by
income-to-needs mean scores were enhanced in analyses using
income-to-needs variablity. That is, more poverty was related to less
income-to-needs variablity. Put differently, richer families were more
likely to experience income fluctuations.

This is odd because it suggests that high poverty and high poverty
variability show opposite effects. Moreover, neighborhood poverty and
neighborhood poverty variability do not follow this pattern.

We belieive such effects are driven by the fact that income-to-needs
mean and variability scores are strongly related (see below), which has
been reported elsewhere ([Li et al., 2018](#ref-li2018)). We also knew
about this correlation before conducting our anlaysis. However, previous
literature had documented this issue and it was not entirely clear what
the consequence of this correlation would be for the current study.

## Update: Secondary Analysis

``` r
# Load and source relevant files
load("data/analysis-ivs.Rdata")
source("scripts/0-corr_table.R")

# ggplot2 theme
theme_set(
  theme_light() +
    theme(
      text = element_text(size = 14),
      title = element_text(size = 14, hjust = .5),
      axis.line = element_line(),
      panel.border = element_blank(),
      panel.background = element_rect(color = NA),
      panel.grid = element_blank(),
      plot.background = element_rect(color = NA),
      plot.title = element_text(hjust = .5, face = "bold"),
      strip.background = element_rect(color = NA, fill = NA),
      strip.text = element_text(color = "black", hjust = 0.5, face = "bold.italic")
    )
)
```

We plan to conduct a set of secondary analyses that use different
methods for computing income-to-needs variability to unpack this issue.
In our primary analyses, we used simple standard deviations of
income-to-needs scores from 1 to 54 months. Li and colleagues
([2018](#ref-li2018)) used residual variance scores. These scores are
nearly identical and correlate the same with average income-to-needs
scores (see below).

To handle the strong relation between average and variability scores, we
computed average percent change scores. In time series analysis, percent
change reflects how much a score changes relative to the previous point.
For example, if one’s income is \$1,000 at timepoint 1 and increases to
\$1,500 at timepoint 2, their percent change score would be .50 or 50%
(\$500 increase is half of income at timepoint 1). The percent change
score is always relative to the previous timepoint so if income
increases another \$500 at timepoint 3, the percent change score would
be .33 or 33% (\$500 is 1/3 of timepoint 2 income of \$1500).

We computed all three types of income variability scores: simple
standard deviation, residual variance, and percent change. Both standard
deviation and residual variance scores correlate strongly with mean
scores and are nearly identidical to each other. However, percent change
scores only weakly correlate with mean scores and the sign of this
correlation flips. That is, higher income is related to lower average
percentage change scores.

``` r
# Linear model function
incnt_linear <- function(df){
  lm(incnt ~ as.numeric(assessment), data=df)
}

# Grab only income-to-needs scores
new_incnt_data1 <- 
  ivs_data2 |> 
  filter(
    assessment %in% c("01", "06", "15", "24", "36", "54")
  ) |> 
  select(id, assessment, incnt)

# Nest data to apply linear function to each observation
new_incnt_data2 <- 
  new_incnt_data1 |> 
  drop_na(incnt) |> 
  nest(.by = id) |> 
  mutate(
    incnt_lm        = map(data, incnt_linear),
    incnt_mean      = map_dbl(data,\(x) mean(x$incnt, na.rm = T)),
    incnt_sd        = map_dbl(data,\(x) sd(x$incnt, na.rm = T)),
    incnt_coefs     = map(incnt_lm, tidy),
    incnt_stats     = map(incnt_lm, glance),
    incnt_intercept = map_dbl(incnt_coefs, \(x) unlist(x[1,"estimate"])),
    incnt_slope     = map_dbl(incnt_coefs, \(x) unlist(x[2,"estimate"])),
    incnt_deviance  = incnt_stats |> map_dbl("deviance"),
    incnt_sigma     = incnt_stats |> map_dbl("sigma"),
    incnt_pc        = map_dbl(data,\(x) mean(abs(x$incnt - lag(x$incnt))/lag(x$incnt), na.rm = T)),
    incnt_n         = map_dbl(data, nrow),
    .by = id
  )

new_incnt_data2 |> 
  select(matches("(mean|sd|sigma|pc|_n)$")) |> 
  corr_table(
    numbered = T,
    c.names = c("Income Mean", "Income SD", "Income Residual Variance", "Income Average Percent Change", "N scores"),
    stats = c("n", "mean", "sd", "min", "median", "max", "skew","kurtosis")
  ) |> 
  knitr::kable()
```

| Variable                          | 1         | 2        | 3        | 4         | 5     |
|:----------------------------------|:----------|:---------|:---------|:----------|:------|
| 1\. Income Mean                   | \-        | 1299     | 1256     | 1299      | 1355  |
| 2\. Income SD                     | 0.70\*\*  | \-       | 1256     | 1299      | 1299  |
| 3\. Income Residual Variance      | 0.68\*\*  | 0.95\*\* | \-       | 1256      | 1256  |
| 4\. Income Average Percent Change | -0.17\*\* | 0.17\*\* | 0.18\*\* | \-        | 1299  |
| 5\. N scores                      | 0.20\*\*  | 0.12\*\* | 0.13\*\* | -0.09\*\* | \-    |
| N                                 | 1355      | 1299     | 1256     | 1299      | 1355  |
| Mean                              | 3.39      | 1.09     | 1.01     | 0.59      | 5.35  |
| SD                                | 2.69      | 1.14     | 1.08     | 0.84      | 1.31  |
| Min                               | 0.13      | 0.00     | 0.00     | 0.00      | 1.00  |
| Median                            | 2.72      | 0.81     | 0.73     | 0.35      | 6.00  |
| Max                               | 23.79     | 17.78    | 14.39    | 12.16     | 6.00  |
| Skew                              | 2.10      | 5.14     | 4.86     | 5.16      | -2.20 |
| Kurtosis                          | 7.08      | 50.30    | 41.43    | 41.44     | 3.86  |

And this can be visualized by drawing scatter plots of each variability
scores against average scores:

``` r
new_incnt_data2 |> 
  select(id, matches("(mean|sd|sigma|pc)$")) |> 
  pivot_longer(c(-id, -incnt_mean), names_to = "var_type", values_to = "score") |> 
  mutate(var_type = factor(var_type,c("incnt_sd", "incnt_sigma","incnt_pc"),c("SD", "Residual","Percent Change"))) |> 
  ggplot(aes(x = incnt_mean, y = score)) +
  geom_point(color = "gray80") +
  stat_smooth(method = "lm", se = F, color = ggsci::pal_cosmic("hallmarks_light")(10)[4]) +
  scale_x_continuous("Income-to-Needs Mean Score") +
  scale_y_continuous("Income-to-Needs Variability Score") +
  facet_wrap(~var_type, ncol = 1, scales = "free_y")
```

<img src="README_files/figure-commonmark/scatterplots-1.png"
data-fig-align="center" />

## Conclusion

Based on these descriptive results, we plan to conduct our follow up
analysis with these three variability scores. We will follow the same
exact modeling strategy as our primary analyses.

## References

<div id="refs" class="references csl-bib-body hanging-indent"
line-spacing="2">

<div id="ref-li2018" class="csl-entry">

Li, Z., Liu, S., Hartman, S., & Belsky, J. (2018). Interactive Effects
of Early-Life Income Harshness and Unpredictability on Children’s
Socioemotional and Academic Functioning in Kindergarten and Adolescence.
*Developmental Psychology*, *54*(11), 2101–2112.
<https://doi.org/gfmd6w>

</div>

</div>
