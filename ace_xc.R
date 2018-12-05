library(tidyverse)
library(rio)
library(AER)
library(stargazer)

ace_xc_raw <- import("data/ace_xcountry.dta")

xc_df <- ace_xc_raw %>%
  select(logpgdp05, tyr05_n, ruleoflaw, lat_abst, 
         africa, america, asia, f_brit, f_french, 
         lcapped, lpd1500s, prienr1900, protmiss) %>%
  rename("gdp" = logpgdp05,
         "yr_school" = tyr05_n,
         "latitude" = lat_abst,
         "settlermortality" = lcapped,
         "pop" = lpd1500s,
         "enrollment1900" = prienr1900)

ggplot(xc_df, aes(x = ruleoflaw,
                  y = gdp)) +
  geom_point() +
  ggsave("institutioneffect.png")

ggplot(xc_df, aes(x = protmiss,
                  y = gdp)) +
  geom_point() +
  ggsave("hkiv.png")


fit1 <- lm(logpgdp05 ~ tyr05_n + ruleoflaw + lat_abst +
             africa + america + asia + f_brit + f_french, data = xc_df)

tsls_fit_xc <- ivreg(logpgdp05 ~ tyr05_n + lcapped + lpd1500s | 
                lcapped + lpd1500s + prienr1900 + protmiss, data = xc_df)

stargazer(tsls_fit_xc, type = "text")
