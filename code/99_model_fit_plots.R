## Imports ----
library(tidyverse)
library(ggsci)
library(here)
source(here("code", "mk_nytimes.R"))

## Data ----
analytic_df <- readRDS(here("data", "excess_mortality_estimates.RDS"))

for (r in unique(analytic_df$race_eth)) {
    p_x <- ggplot(data = analytic_df %>% 
                      filter(race_eth == r)) +
        geom_vline(xintercept = as.Date("2020-03-01"),
                   alpha = .5,
                   linetype = "dotted") + 
        geom_point(aes(x = date, y = n_deaths / pop * 100000),
                   size = .3,
                   alpha = .75) +
        geom_ribbon(aes(
            x = date,
            y = expected / pop * 100000,
            ymin = expected_lower / pop * 100000,
            ymax = expected_upper / pop * 100000
        ),
        alpha = .2) + 
        geom_line(aes(x = date, y = expected / pop * 100000),
                  alpha = .5) +
        facet_wrap(~ cause_of_death, 
                   ncol = 5,
                   scales = "free") + 
        scale_y_continuous("Weekly deaths per 100,000 population") + 
        scale_x_date(NULL) + 
        mk_nytimes() + 
        labs(title = r)
    ggsave(here("plots", sprintf("supp_model_fit - %s.pdf", r)),
           p_x, 
           width = 12, 
           height = 6,
           scale = 1.2,
           device = cairo_pdf)
}
