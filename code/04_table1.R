## Imports ----
library(tidyverse)
library(ggsci)
library(here)

## Constants ----
MAIN_CODS <- c(
    "All external causes of deaths",
    "Assault",
    "Intentional self-harm",
    "Transport accidents",
    "Drug Overdose"
)

## Data ----
analytic_df <-
    readRDS(here("data", "excess_mortality_estimates.RDS")) %>%
    filter(cause_of_death %in% MAIN_CODS,
           date >= as.Date("2020-03-01"))

table1 <- left_join(
    analytic_df %>%
        group_by(cause_of_death, race_eth) %>%
        summarize(
            n_deaths = sum(n_deaths),
            pop = mean(pop),
            expected = sum(expected),
            expected_lower = sum(expected_lower),
            expected_upper = sum(expected_upper)
        ),
    analytic_df %>%
        filter(date == max(date)) %>%
        select(cause_of_death, race_eth, starts_with("cumulative_"))
) %>%
    ungroup() %>%
    transmute(
        cause_of_death = factor(
            cause_of_death,
            levels = c(
                "All external causes of deaths",
                "Assault",
                "Intentional self-harm",
                "Transport accidents",
                "Drug Overdose"
            ),
            labels = c(
                "All",
                "Homicide",
                "Suicide",
                "Transportation fatalities",
                "Drug overdose"
            ),
            ordered = TRUE
        ),
        race_eth = factor(
            race_eth,
            levels = c(
                "All",
                "Non-Hispanic White",
                "Non-Hispanic Black", 
                "Hispanic",
                "Asian or Pacific Islander",
                "American Indian or Alaska Native"
            ),
            labels = c(
                "All",
                "Non-Hispanic White",
                "Non-Hispanic Black",
                "Hispanic", 
                "Non-Hispanic Asian or Pacific Islander",
                "Non-Hispanic American Indian or Alaska Native"
            ),
            ordered = TRUE
        ),
        obs_expected = sprintf(
            "%s (%s)",
            prettyNum(round(n_deaths), big.mark = ","),
            prettyNum(round(expected), big.mark = ",")
        ),
        excess_deaths = sprintf(
            "%s (%s to %s)",
            prettyNum(round(cumulative_mean), big.mark = ","),
            prettyNum(round(cumulative_lower), big.mark = ","),
            prettyNum(round(cumulative_upper), big.mark = ",")
        ),
        excess_per_capita = sprintf(
            "%0.2f (%0.2f to %0.2f)",
            round(cumulative_mean / pop * 100000, 2),
            round(cumulative_lower / pop * 100000, 2),
            round(cumulative_upper / pop * 100000, 2)
        ),
        obs_vs_exp = sprintf(
            "%0.2f (%0.2f to %0.2f)",
            round(n_deaths / expected, 2),
            round(n_deaths / expected_upper, 2),
            round(n_deaths / expected_lower, 2)
        )
    ) %>% 
    arrange(cause_of_death,
            race_eth)

write_csv(table1, here("output", "table1.csv"))
