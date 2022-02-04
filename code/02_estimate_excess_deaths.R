## Imports ----
library(tidyverse)
library(here)
library(forecast)

## Constants ----
FORECAST_START <- as.Date("2020-03-01")
FORECAST_WINDOW <- 10 ## Predict from March 2020 to Dec 2020

## Load data ----
all_long <- readRDS(here("data", "analytic_df.RDS"))

## Helper function ----
return_fitted_and_predicted <- function(
    all_long,
    forecast_start,
    forecast_window,
    cause_x,
    race_x) {
    
    current_df <- all_long %>%
        filter(cause_of_death == cause_x,
               race_eth == race_x)
    
    sub_df <- current_df %>%
        filter(date < forecast_start)
    
    tt <- ts(sub_df$n_deaths,
             deltat = 1 / 12,
             start = min(sub_df$year))
    tt <- tsclean(tt)
    
    # fit model with up to six harmonic terms
    mm <- list(aicc = Inf)
    for (i in 1:6) {
        mm.i <- auto.arima(tt, xreg = fourier(tt, K = i), seasonal = FALSE)
        if (mm.i$aicc < mm$aicc) {
            mm <- mm.i
            k.best <- i
        }
    }
    fitted_df <- tibble(date = sub_df$date,
                        expected = fitted(mm))
    
    # obtain forecasts
    ff <- forecast(mm, xreg = fourier(tt, K = k.best, h = forecast_window))
    
    # extract observed values
    oo <- current_df[('2020-03-01' <= current_df$date) &
                         (current_df$date < (forecast_start + forecast_window * 30.25)),
                     c('date', "n_deaths")]
    
    ## extract expected values
    ee <- data.frame(
        date = seq(forecast_start, by = "month", length.out = 10),
        expected = as.numeric(ff$mean),
        expected_lower = as.numeric(ff$lower[, '95%']),
        expected_upper = as.numeric(ff$upper[, '95%']) 
    )
    
    # obtain prediction intervals for totals
    set.seed(94118)
    NN <- 10000
    SS <- NULL
    for (ii in 1:NN) {
        sim.i <- simulate(
            mm,
            future = TRUE,
            nsim = forecast_window,
            xreg = fourier(tt, K = k.best, h = forecast_window)
        )
        SS.i <- data.frame(pt = sum(sim.i))
        SS <- rbind(SS, SS.i)
    }
    
    current_df %>%
        left_join(bind_rows(fitted_df, ee)) %>% 
        mutate(cumulative_lower = unname(quantile(sum(oo$n_deaths) - SS$pt, .025)),
               cumulative_upper = unname(quantile(sum(oo$n_deaths) - SS$pt, .975)),
               cumulative_mean = mean(sum(oo$n_deaths) - SS$pt))
}

## Fit and predict every combination of cause/race ----
if (!file.exists(here("data", "excess_mortality_estimates.RDS"))) {
    param_grid <- all_long %>% 
        select(cause_of_death, race_eth) %>% 
        distinct()
    
    holder <- vector("list", NROW(param_grid))
    for (i in 1:NROW(holder)) {
        if (is.null(holder[[i]])) {
            holder[[i]] <-
                return_fitted_and_predicted(
                    all_long,
                    FORECAST_START,
                    FORECAST_WINDOW,
                    cause_x = param_grid$cause_of_death[i],
                    race_x = param_grid$race_eth[i]
                )
        }
    }
    
    ## Save ----
    holder <- bind_rows(holder)
    saveRDS(holder, here("data", "excess_mortality_estimates.RDS"))
    write_csv(holder, here("data", "excess_mortality_estimates.csv"))
}
