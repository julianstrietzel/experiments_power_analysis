library(data.table)
library(ggplot2)
library(here)
library(dplyr)
library(progress)

hours = 4
sessions_per_hour = 6
people_per_session = 2

size <- hours * sessions_per_hour * people_per_session

demo_size = 1000
d <- data.table(
  shared_flyers_control = rnorm(demo_size, 40, 1 ),
  replies_control = rnorm(demo_size, 3, 1),
  treatment = sample(0:1, demo_size, replace = TRUE)
)
d[, shared_flyers_treatment := shared_flyers_control + rnorm(demo_size, 10, 1)]
d[, replies_treatment := replies_control + rnorm(demo_size, 2, 1)]
d[, shared_measured := I(treatment == 1) * shared_flyers_treatment + I(treatment == 0) * shared_flyers_control]
d[, replies_measured := I(treatment == 1) * replies_treatment + I(treatment == 0) * replies_control]

# min all measurements to zero
d[, shared_measured := pmax(shared_measured, 0)]
d[, replies_measured := pmax(replies_measured, 0)]



number_of_sessions_per_group <- 1:100

p_values <- function(samples) {
  sub_sample <- rbindlist(list(
    d[treatment==1,][sample(1:.N, samples, replace = FALSE), ],
    d[treatment==0,][sample(1:.N, samples, replace = FALSE), ]
  ))
  shared_measured_ate <- mean(sub_sample[treatment == 1]$shared_measured) - mean(sub_sample[treatment == 0]$shared_measured)
  replies_measured_ate <- mean(sub_sample[treatment == 1]$replies_measured) - mean(sub_sample[treatment == 0]$replies_measured)
  ri_ates <- replicate(500, {
    # Increasing efficiency of estimate by making sure both groups are 50% of the sample
    ri_sample <- sub_sample[, ri_treat := sample(rep(0:1, each = samples))] 
    c(
      shared_ate = mean(ri_sample$shared_measured[ri_sample$ri_treat == 1]) - mean(ri_sample$shared_measured[ri_sample$ri_treat == 0]),
      replies_ate = mean(ri_sample$replies_measured[ri_sample$ri_treat == 1]) - mean(ri_sample$replies_measured[ri_sample$ri_treat == 0])
    )
  })

  shared_p = mean(abs(ri_ates["shared_ate", ]) >= abs(shared_measured_ate))
  replies_p = mean(abs(ri_ates["replies_ate", ]) >= abs(replies_measured_ate))
  
  c(shared_p, replies_p)
}
pb <- progress_bar$new(
  format = "  Progress [:bar] :percent Elapsed time: :elapsed Full time: :eta",
  total = length(number_of_sessions_per_group),   
  clear = FALSE, 
  width = 100
)

p_values_per_number_of_sessions <- lapply(number_of_sessions_per_group, function(n) {
  pb$tick()  # Update the progress bar
  p_values(n)
})

p_values_dt <- rbindlist(lapply(p_values_per_number_of_sessions, as.list))
setnames(p_values_dt, c("shared_p", "replies_p"))

# line diagram powers agains percentages of sampling
split_values <- c(10, 20, 40)
ggplot() +
  geom_line() +
  geom_line(aes(x = number_of_sessions_per_group, y = 1 - p_values_dt$shared_p, color = "Shared"), alpha = 0.7, size=1.2) +
  geom_line(aes(x = number_of_sessions_per_group, y = 1 - p_values_dt$replies_p, color = "Replied"), alpha = 0.7, size=1.2) +
  geom_vline(xintercept = split_values, linetype = "dashed") +
  geom_hline(yintercept = 1 - 0.05, linetype = "dashed", color="green") +
  scale_x_continuous(breaks = sort(c(seq(0, 50, by = 5), split_values))) +
  scale_color_manual(values = c("Shared"="red", "Replied"="blue")) +
  labs(x = "Number of sessions per group", y = "Power shared", colour="Response") + 
  theme_minimal()

