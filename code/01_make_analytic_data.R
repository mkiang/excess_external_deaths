## Imports ----
library(tidyverse)
library(here)
library(janitor)

## Population data ----
pop_df <- read_delim(here("data_raw", 
                          "yearly_bridgedrace_hisp_allexternal.txt")) %>% 
    clean_names() %>% 
    transmute(
        hisp = hispanic_origin,
        race,
        year = as.integer(year),
        pop = as.integer(population)
    ) %>% 
    filter(!is.na(year)) %>% 
    distinct() 

## Deaths data ----
deaths_df <- bind_rows(
    ## Read in all external causes
    read_delim(here(
        "data_raw", "monthly_bridgedrace_hisp_allexternal.txt"
    )) %>%
        clean_names() %>%
        filter(is.na(notes)) %>%
        transmute(
            cause_of_death = "All external causes of deaths",
            hisp = hispanic_origin,
            race,
            year = as.integer(year),
            date = as.Date(sprintf("%s/01", month_code)),
            n_deaths = deaths
        ) %>% 
        group_by(cause_of_death, hisp, race, year, date) %>% 
        summarize(n_deaths = sum(n_deaths)) %>% 
        ungroup(),
    ## Read in specific external causes
    read_delim(here(
        "data_raw", "monthly_bridgedrace_hisp_allexternal.txt"
    )) %>%
        clean_names() %>%
        filter(is.na(notes)) %>%
        transmute(
            cause_of_death = ucd_icd_sub_chapter,
            hisp = hispanic_origin,
            race,
            year = as.integer(year),
            date = as.Date(sprintf("%s/01", month_code)),
            n_deaths = deaths
        ),
    ## Read in poisonings
    read_delim(here(
        "data_raw", "monthly_bridgedrace_hisp_drugoverdose.txt"
    )) %>%
        clean_names() %>%
        filter(is.na(notes)) %>%
        transmute(
            cause_of_death = "Drug Overdose",
            hisp = hispanic_origin,
            race,
            year = as.integer(year),
            date = as.Date(sprintf("%s/01", month_code)),
            n_deaths = deaths
        )
)

## Join pop and deaths ----
analytic_df <- left_join(deaths_df, pop_df) %>% 
    filter(year >= 2015)

## Now we need new race/ethnicity categories ----
## All Hispanic count as Hispanic. Remaining non-Hispanic are broken down
## into American Indian or Alaska Native, White, Black, and 
## Asian or Pacific Islander (and total)

## First make a total population one ----
analytic_df <- bind_rows(
    analytic_df %>% 
        mutate(hisp = "all",
               race = "all"),
    analytic_df
)

## Now make a new race variable ----
analytic_df <- analytic_df %>% 
    mutate(race_eth = case_when(
        hisp == "all" & race == "all" ~ "All",
        hisp == "Hispanic or Latino" ~ "Hispanic",
        hisp == "Not Hispanic or Latino" & 
            race == "White" ~ "Non-Hispanic White",
        hisp == "Not Hispanic or Latino" & 
            race == "Black or African American" ~ "Non-Hispanic Black",
        hisp == "Not Hispanic or Latino" & 
            race == "American Indian or Alaska Native" ~ "American Indian or Alaska Native",
        hisp == "Not Hispanic or Latino" & 
            race == "Asian or Pacific Islander" ~ "Asian or Pacific Islander",
        TRUE ~ NA_character_
    )) %>% 
    filter(!is.na(race_eth))

## Sum deaths and population according to new race var ----
analytic_df <- analytic_df %>% 
    group_by(cause_of_death, race_eth, year, date) %>% 
    summarize(n_deaths = sum(n_deaths),
              pop = sum(pop, na.rm = TRUE)) %>% 
    arrange(cause_of_death, race_eth, date) %>% 
    ungroup()

## Save ----
write_csv(analytic_df, 
          here("data", "analytic_df.csv"))
saveRDS(analytic_df,
        here("data", "analytic_df.RDS"))
