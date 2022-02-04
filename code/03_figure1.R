## Imports ----
library(tidyverse)
library(ggsci)
library(here)
source(here("code", "mk_nytimes.R"))

## Constants ----
MAIN_CODS <- c(
    "All external causes of deaths",
    "Assault",
    "Intentional self-harm",
    "Transport accidents",
    "Drug Overdose"
)

## Data ----
analytic_df <- readRDS(here("data", "excess_mortality_estimates.RDS"))

plot_df <- analytic_df %>% 
    filter(date == max(date),
           cause_of_death %in% MAIN_CODS) %>% 
    mutate(cause_of_death_cat = factor(cause_of_death,
                                       levels = rev(MAIN_CODS),
                                       ordered = TRUE))

## Plot ----
p1 <- ggplot(plot_df,
       aes(x = cumulative_mean / pop * 100000,
           xmin = cumulative_lower / pop * 100000,
           xmax = cumulative_upper / pop * 100000,
           y = cause_of_death_cat,
           color = race_eth,
           group = race_eth,
           size = cumulative_mean)) + 
    geom_vline(xintercept = 0,
               alpha = .5) + 
    geom_point(position = position_dodge2(width = .3)) + 
    geom_errorbarh(position = position_dodge2(width = .3),
                   size = .7, 
                   height = .3) + 
    scale_size_binned_area("Absolute number\nof excess deaths",
                           breaks = (c(100, 500, 1000, 5000, 10000)),
                           labels = function(x) sprintf("%i", round(x))) + 
    scale_color_jama(name = "Race and Ethnicity", ) + 
    scale_x_continuous("Total excess deaths per 100,000 population (95% PI)") + 
    scale_y_discrete("Type of death") + 
    mk_nytimes(legend.position = "right")
    

## Save ----
ggsave(
    here("plots", "figure1.pdf"),
    p1,
    width = 10,
    height = 5,
    scale = 1,
    device = cairo_pdf
)
ggsave(
    here("plots", "figure1.jpg"),
    p1,
    width = 10,
    height = 5,
    scale = 1,
    dpi = 300
)
write_csv(plot_df, here("output", "fig1_data.csv"))
