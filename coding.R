library(data.table)
library(ggplot2)
library(here)
library(dplyr)

hours = 4
sessions_per_hour = 6
people_per_session = 2

size <- hours * sessions_per_hour * people_per_session

demo_size = 1000
d <- data.table(
  shared_flyers_control = rnorm(demo_size, 40 / sessions_per_hour, 10 / sessions_per_hour **2),
  replies_control = rnorm(demo_size, 3, 1),
  treatment = sample(0:1, demo_size, replace = TRUE)
)
d <- d[, shared_flyers_treatment := shared_flyers_control + rnorm(demo_size, 10, 1)]
d <- d[, replies_treatment := replies_control + rnorm(demo_size, 2, 1)]
d <- d[, replies_tau := replies_treatment - replies_control]
d <- d[, shared_tau := shared_flyers_treatment - shared_flyers_control]

d <- d[, shared_measured := I(treatment == 1) * shared_flyers_treatment + I(treatment == 0) * shared_flyers_control]
d <- d[, replies_measured := I(treatment == 1) * replies_treatment + I(treatment == 0) * replies_control]


number_of_sessions_per_group <- 1:100

p_values <- function(samples) {
  sub_sample <- bind_rows(
    d[treatment==1,][sample(1:.N, samples, replace = FALSE), ],
    d[treatment==0,][sample(1:.N, samples, replace = FALSE), ]
  )
  shared_measured_ate <- mean(sub_sample[treatment == 1]$shared_flyers_treatment) - mean(sub_sample[treatment == 0]$shared_flyers_control)
  replies_measured_ate <- mean(sub_sample[treatment == 1]$replies_treatment) - mean(sub_sample[treatment == 0]$replies_control)
  
  ri_ates_shared <- replicate(1000, {
    sub_sample_ <- sub_sample[, ri_treat := sample(c(0,1), .N, replace = TRUE)]
    mean(sub_sample_$shared_measured[sub_sample_$ri_treat == 1]) - mean(sub_sample_$shared_measured[sub_sample_$ri_treat == 0])
    })
  shared_p = mean(abs(ri_ates_shared) >= abs(shared_measured_ate))
  ri_ates_replies <- replicate(1000, {
    sub_sample_ <- sub_sample[, ri_treat := sample(c(0,1), .N, replace = TRUE)]
    mean(sub_sample_$replies_measured[sub_sample_$ri_treat == 1]) - mean(sub_sample_$replies_measured[sub_sample_$ri_treat == 0])
  })
  replies_p = mean(abs(ri_ates_replies) >= abs(replies_measured_ate))
  c(shared_p, replies_p)
}

p_values_per_number_of_sessions <- lapply(number_of_sessions_per_group, p_values)

# line diagram powers agains percentages of sampling
perc_and_powers <- 
  data.table(perc = percentages_to_sample, power = unlist(p_values_per_number_of_sessions))
ggplot(perc_and_powers, aes(x = perc, y = power)) +
  geom_line() 
