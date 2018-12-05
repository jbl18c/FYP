library(tidyverse)
library(rio)
library(AER)
library(stargazer)


ace_xr_raw <- import("data/ace_xregion.dta")

xr_df <- ace_xr_raw %>%
  select(country, state, temp_avg, temp2, landlocked, miss_presence, yearsed,
         lgdp, invdistcoast, capital_old, invdis2, lpopd_i) %>%
  drop_na(yearsed, miss_presence, lgdp)

ols_fit <- lm(lgdp ~ temp_avg + temp2 + landlocked + invdistcoast +
                capital_old + invdis2 + lpopd_i + yearsed, data = ace_xr_raw)
tsls_fit <- ivreg(lgdp ~ temp_avg + temp2 + landlocked + invdistcoast +
                capital_old + invdis2 + lpopd_i + yearsed |
                  miss_presence + temp_avg + temp2 + landlocked + invdistcoast +
                  capital_old + invdis2 + lpopd_i, data = xr_df)

stargazer(tsls_fit, type = "text")
