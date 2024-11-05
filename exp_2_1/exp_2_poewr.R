# author Julian
library(data.table)
library(ggplot2)
library(here)
library(dplyr)
library(progress)
library(stargazer)

tenth_sessions = fread(here("exp_2_1", "10_sessions_vw_only_v.csv"))
fifth_sessions = fread(here("exp_2_1", "5_sessions_vw_only_v.csv"))

third_sessions = fread(here("exp_2_1", "3_sessions_vw_only_v.csv"))
half_sessions = fread(here("exp_2_1", "2_sessions_vw_only_v.csv"))
full_sessions = fread(here("exp_2_1", "1_sessions_vw_only_v.csv"))



model_tenth = tenth_sessions[, lm(success_rate ~ factor(team) + treatment)]
model_fifth = fifth_sessions[, lm(success_rate ~ factor(team) + treatment)]
model_third = third_sessions[, lm(success_rate ~ factor(team) + treatment)]
model_half = half_sessions[, lm(success_rate ~ factor(team) + treatment)]
model_full = full_sessions[, lm(success_rate ~ factor(team) + treatment)]

stargazer(model_full, model_half, model_third, model_fifth, model_tenth, type = "text", omit = "session_starts", column.labels = c("Full", "Half", "Third", "Fifth", "Tenth"))

model_full_start = full_sessions[, lm(success_rate ~ factor(team) + treatment + factor(session_starts))]
model_half_start = half_sessions[, lm(success_rate ~ factor(team) + treatment + factor(session_starts))]
model_third_start = third_sessions[, lm(success_rate ~ factor(team) + treatment + factor(session_starts))]
model_fifth_start = fifth_sessions[, lm(success_rate ~ factor(team) + treatment + factor(session_starts))]
model_tenth_start = tenth_sessions[, lm(success_rate ~ factor(team) + treatment + factor(session_starts))]
anova(model_full, model_full_start)
anova(model_half, model_half_start)
anova(model_third, model_third_start)
anova(model_fifth, model_fifth_start)
anova(model_tenth, model_tenth_start)


stargazer(model_full, model_full_start, model_half, model_half_start, type = "text", omit = "session_starts")
stargazer(model_fifth, model_fifth_start, model_tenth, model_tenth_start, type = "text", omit = "session_starts")



model_full_team_interaction = full_sessions[, lm(success_rate ~ factor(team) * treatment)]
anova(model_full, model_full_team_interaction)
stargazer(model_full, model_full_team_interaction, type = "text")

model_half_team_interaction = half_sessions[, lm(success_rate ~ factor(team) * treatment)]
anova(model_half, model_half_team_interaction)
stargazer(model_half, model_half_team_interaction, type = "text")

model_third_team_interaction = third_sessions[, lm(success_rate ~ factor(team) * treatment)]
anova(model_third, model_third_team_interaction)
stargazer(model_third, model_third_team_interaction, type = "text", column.labels = c("No Interaction", "Interaction"), title="Third Sessions")


model_fifth_team_interaction = fifth_sessions[, lm(success_rate ~ factor(team) * treatment)]
stargazer(model_fifth, model_fifth_team_interaction, type = "text", column.labels = c("No Interaction", "Interaction"), title="Fifth Sessions")

model_ten_team_interaction = tenth_sessions[, lm(success_rate ~ factor(team) * treatment)]
anova(model_tenth, model_ten_team_interaction)
stargazer(model_fifth, model_tenth, model_ten_team_interaction, type = "text", omit = "session_starts",
          column.labels = c("Fifth", "Tenth", "Tenth Interaction"), title="2/1 min subsessions"
          )

twentieth_sessions = fread(here("exp_2_1", "20_sessions_vw_only_v.csv"))
model_twenty = twentieth_sessions[, lm(success_rate ~ factor(team) * treatment)]
stargazer(model_twenty, type = "text")

# changing the intercepts
model_fifth_no_intercept = fifth_sessions[, lm(success_rate ~ factor(team) + treatment - 1)]
model_fifth_interaction_no_intercept = fifth_sessions[, lm(success_rate ~ factor(team) * treatment - 1)]
stargazer(model_fifth, model_fifth_no_intercept, model_fifth_team_interaction, model_fifth_interaction_no_intercept)

stargazer(model_fifth, model_fifth_no_intercept, type = "text", column.labels = c("Intercept", "No Intercept"), title = "Fifth Sessions")
stargazer(model_fifth_team_interaction, model_fifth_interaction_no_intercept, type = "text", column.labels = c("Intercept", "No Intercept"), title="Fifth Sessions with interaction")

# looking if using session_within_session is a good idea
model_fifth_within = fifth_sessions[, lm(success_rate ~ factor(team) * treatment + factor(session_within_session))]
anova(model_fifth_team_interaction, model_fifth_within)
stargazer(model_fifth_team_interaction, model_fifth_within, type = "text", column.labels = c("No Within", "Within"), title="Fifth Sessions", omit = "session_within")

model_third_within = third_sessions[, lm(success_rate ~ factor(team) * treatment + factor(session_within_session))]
anova(model_third_team_interaction, model_third_within)
stargazer(model_third_team_interaction, model_third_within, type = "text", column.labels = c("No Within", "Within"), title="Third Sessions", omit = "session_within")

model_interacted = third_sessions[, lm(success_rate ~ factor(team) * treatment + factor(session_within_session) * treatment)]
stargazer(model_fifth_within, model_interacted, type = "text", column.labels = c("No Interaction", "Interaction"), title="Fifth Sessions")
# simulating if this brings us to where we want to be:
sessions = 16

d = data.table(
  treatment = rep(c(0, 1), sessions * 5),
  team = c(rep(0, sessions * 5), rep(1, sessions * 5))
)
# create samples with different success rates averages for each team and treatment
d[, success_rate := rnorm(.N, mean = .26, sd = 0.26) * (1-treatment) * team 
  + rnorm(.N, mean = .26, sd = 0.1) * treatment * team 
  + rnorm(.N, mean = .42, sd = 0.31) * treatment * (1-team) 
  + rnorm(.N, mean = .34, sd = 0.33) * (1-treatment) * (1-team)
    ]
model = lm(success_rate ~ factor(team) * treatment, data = d)
model2 = lm(success_rate ~ factor(team) + treatment, data = d)
stargazer(model, model2, type = "text")

d = data.table(
  treatment = rep(c(0, 1), sessions * 10),
  team = c(rep(0, sessions * 10), rep(1, sessions * 10))
)
# create samples with different success rates averages for each team and treatment
d[, success_rate := rnorm(.N, mean = .25, sd = 0.29) * (1-treatment) * team 
  + rnorm(.N, mean = .25, sd = 0.17) * treatment * team 
  + rnorm(.N, mean = .40, sd = 0.36) * treatment * (1-team) 
  + rnorm(.N, mean = .35, sd = 0.37) * (1-treatment) * (1-team)
]
model = lm(success_rate ~ factor(team) * treatment, data = d)
model2 = lm(success_rate ~ factor(team) + treatment, data = d)
stargazer(model, model2, type = "text")


# Look if splitting this multiple sessions for vetle and will is a good idea

two_min_split = fread(here("exp_2_1", "5_sessions_v_and_w_split_second_half.csv"))
model_two_min = two_min_split[, lm(success_rate ~ factor(team) * treatment)]
model_two_min_ignored_vw = two_min_split[team != "vw", lm(success_rate ~ factor(team) * treatment)]
model_two_no_intercept = two_min_split[, lm(success_rate ~ factor(team) * treatment - 1)]
stargazer(model_two_min, model_two_min_ignored_vw,model_two_no_intercept, type = "text", column.labels = c("No Split", "Split", "Split ignoring first half"), title="Fifth Sessions")

# -> Splitting is super helpful, the more data the better though. Don't throw away anything!