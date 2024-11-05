# author Julian
library(data.table)
library(ggplot2)
library(here)
library(dplyr)
library(progress)

d_1 <- data.table(fread(here("INFO 241_ Data Collection  - Sheet1.csv")))
d_2 = copy(d_1)
d <- bind_rows(
  d_1[, `:=` (treatment = ifelse(treatment_g1 == "Neutral", 0, 1), num_flyers = num_flyers_g1, qr_scanned = qr_scanned_g1 , seconds_to_cap = seconds_to_cap_g1, group = 0)],
  d_2[, `:=` (treatment = ifelse(treatment_g2 == "Neutral", 0, 1), num_flyers = num_flyers_g2, qr_scanned = qr_scanned_g2 , seconds_to_cap = seconds_to_cap_g2, group = 1)]
  ) [, .(treatment, num_flyers, qr_scanned, seconds_to_cap, group, time, date, day_of_week)]
# changing a specifc value 
d[seconds_to_cap == 202, seconds_to_cap := 500]
d[, quote_used := seconds_to_cap / (60 * 10)]
d[, `:=` (extra_qr = qr_scanned / quote_used, extra_num_flyers = num_flyers /quote_used)]

mod1 = lm(seconds_to_cap ~ treatment + group, data = d)

mod2 = lm(extra_qr ~ treatment + group, data = d)
mod3 = lm(extra_num_flyers ~ treatment + group, data = d)
mod4 = lm(qr_scanned ~ treatment + group, data = d)

stargazer::stargazer(mod1, mod2, mod3, mod4, type = "text")

# interaction models

mod_1 = lm(seconds_to_cap ~ treatment + group + group * treatment, data = d)
mod_2 = lm(extra_qr ~ treatment + group + group * treatment, data = d)
mod_3 = lm(extra_num_flyers ~ treatment + group + group * treatment, data = d)
mod_4 = lm(qr_scanned ~ treatment + group + group * treatment, data = d)

stargazer::stargazer(mod_1, mod_2, mod_3, mod_4, type = "text")

# duplicate data
d_duplicated = bind_rows(d, d, d, d, d, d, d)

duplicated_group_one = d_duplicated[group == 0]
# 
mod1 = lm(seconds_to_cap ~ treatment, data = duplicated_group_one)
mod2 = lm(extra_qr ~ treatment, data = duplicated_group_one)
mod3 = lm(extra_num_flyers ~ treatment, data = duplicated_group_one)
mod4 = lm(qr_scanned ~ treatment, data = duplicated_group_one)

stargazer::stargazer(mod1, mod2, mod3, mod4, type = "text")

# interaction models with duplicated

mod_1 = lm(seconds_to_cap ~ treatment + group + group * treatment, data = d_duplicated)
mod_2 = lm(extra_qr ~ treatment + group + group * treatment, data = d_duplicated)
mod_3 = lm(extra_num_flyers ~ treatment + group + group * treatment, data = d_duplicated)
mod_4 = lm(qr_scanned ~ treatment + group + group * treatment, data = d_duplicated)

stargazer::stargazer(mod_1, mod_2, mod_3, mod_4, type = "text")
