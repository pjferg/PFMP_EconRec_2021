
# Load packages
library(tidyverse)
library(stargazer)
library(lfe)
library(bife)

# Read in data set
data <- read_csv("data.csv")

# Table 1 - Descriptive Statistics

data_descriptives <- data %>%
  select(home_win, margin, home_injuries_top5, away_injuries_top5, home_injuries_top10, away_injuries_top10,
         home_odds_win_prob_close, expected_margin, home_injuries_95th_to_90th, away_injuries_95th_to_90th, 
         standing_home, standing_away, home_elo_2009, 
         away_elo_2009)

stargazer(as.data.frame(data_descriptives), omit.summary.stat = c("p25", "p75"), median = FALSE,
          type = "latex", title="Descriptive statistics",
          covariate.labels = c("Home Win = 1", "Margin", "Home Star Injuries (95th)", "Away Star Injuries (95th)",
                               "Home Star Injuries (90th)", "Away Star Injuries (90th)", "Prob. Home Win",
                               "Expected Margin", "Home Injuries (90-95th)",
                               "Away Injuries (90-95th)", "Home Star `Acute` Injuries (95th)",
                               "Away Star `Acute` Injuries (95th)", "Home Standings", "Away Standings",
                               "Home Elo Rating", "Away Elo Rating"),
          digits=2)


# Table 2 - Exogeneity of Star Injuries - Regressions of Star Injuries on Game and Team Attributes

exog_1 <- felm(home_injuries_top5 ~ standing_home + standing_away + year + round + thurs_fri + sat + sun +
                 evening | 0 | 0 | 0, data = data)

exog_2 <- felm(away_injuries_top5 ~ standing_home + standing_away + year + round + thurs_fri + sat + sun +
                 evening | 0 | 0 | 0, data = data)

exog_3 <- felm(home_injuries_top5 ~ home_odds_open + year + round + thurs_fri + sat + sun +
                 evening | 0 | 0 | 0, data = data)

exog_4 <- felm(away_injuries_top5 ~ home_odds_open + year + round + thurs_fri + sat + sun +
                 evening | 0 | 0 | 0, data = data)

exog_5 <- felm(home_injuries_top5 ~ home_elo_2009 + away_elo_2009 + year + round + thurs_fri + sat + sun +
                 evening | 0 | 0 | 0, data = data)

exog_6 <- felm(away_injuries_top5 ~ home_elo_2009 + away_elo_2009 + year + round + thurs_fri + sat + sun +
                 evening | 0 | 0 | 0, data = data)

stargazer(exog_1, exog_2, exog_3, exog_4, exog_5, exog_6,
          omit = c("Constant"),
          omit.stat = c("adj.rsq", "ser"), type = "latex", df = FALSE,
          add.lines = list(c("Robust SEs", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
          covariate.labels = c("Home Standings", "Away Standings", 
                               "Opening Odds for Home Win", "Home Elo", "Away Elo", 
                               "Yearly Trend", "Round Trend", "Thursday-Friday", 
                               "Saturday", "Sunday", "Evening Slot"),
          title = "Exogeneity of Star Injuries - Regressions of Star Injuries on Game and Team Attributes",
          dep.var.labels   = c("Home Star Injuries", "Away Star Injuries",
                               "Home Star Injuries", "Away Star Injuries",
                               "Home Star Injuries", "Away Star Injuries"))

# Table 3 - Measuring the Productivity of Star Players: Regressions of Team Performance on Star Injuries

main_1 <- felm(home_win ~ home_injuries_top5 + away_injuries_top5 |
                 0 | 0 | home, data = data)

main_2 <- felm(home_win ~ home_injuries_top5 + away_injuries_top5 |
                 factor(home) + factor(year) | 0 | home, data = data)

main_3 <- felm(home_win ~ home_injuries_top5 + away_injuries_top5 + standing_home + standing_away|
                 factor(home) + factor(year) | 0 | home, data = data)

main_4 <- felm(home_win ~ home_injuries_top5 + away_injuries_top5 |
                 factor(home_season) | 0 | home_season, data = data)

main_5 <- felm(margin ~ home_injuries_top5 + away_injuries_top5 |
                 0 | 0 | home, data = data)

main_6 <- felm(margin ~ home_injuries_top5 + away_injuries_top5 |
                 factor(home) + factor(year) | 0 | home, data = data)

main_7 <- felm(margin ~ home_injuries_top5 + away_injuries_top5 + standing_home + standing_away |
                 factor(home) + factor(year) | 0 | home, data = data)

main_8 <- felm(margin ~ home_injuries_top5 + away_injuries_top5 |
                 factor(home_season) | 0 | home_season, data = data)

stargazer(main_1, main_2, main_3, main_4, main_5, main_6, main_7, main_8,
          omit = c("Constant", "factor"),
          omit.stat = c("adj.rsq", "ser"), type = "latex", df = FALSE,
          add.lines = list(c("Home team FEs", "No", "Yes", "Yes", "No", "No", "Yes", "Yes", "No"),
                           c("Season FEs", "No", "Yes", "Yes", "No", "No", "Yes", "Yes", "No"),
                           c("Home team-Season FEs", "No", "No", "No", "Yes", "No", "No", "No", "Yes")),
          covariate.labels = c("Home Star Injuries", "Away Star Injuries",
                               "Home Standing", "Away Standing"),
          title = "Measuring the Productivity of Star Players: Regressions of Team Performance on Star Injuries",
          dep.var.labels   = c("Home Win = 1", "Margin"))

# Table 4 - Measuring the Productivity of Star Players: Regressions of Betting Market Expectations on Star Injuries

market_1 <- felm(home_odds_win_prob_close ~ home_injuries_top5 + away_injuries_top5 |
                   0 | 0 | home, data = data)

market_2 <- felm(home_odds_win_prob_close ~ home_injuries_top5 + away_injuries_top5 |
                   factor(home) + factor(year) | 0 | home, data = data)

market_3 <- felm(home_odds_win_prob_close ~ home_injuries_top5 + away_injuries_top5 + standing_home + standing_away |
                   factor(home) + factor(year) | 0 | home, data = data)

market_4 <- felm(home_odds_win_prob_close ~ home_injuries_top5 + away_injuries_top5 |
                   factor(home_season) | 0 | home_season, data = data)

market_5 <- felm(expected_margin ~ home_injuries_top5 + away_injuries_top5 |
                   0 | 0 | home, data = data)

market_6 <- felm(expected_margin ~ home_injuries_top5 + away_injuries_top5 |
                   factor(home) + factor(year) | 0 | home, data = data)

market_7 <- felm(expected_margin ~ home_injuries_top5 + away_injuries_top5 + standing_home + standing_away |
                   factor(home) + factor(year) | 0 | home, data = data)

market_8 <- felm(expected_margin ~ home_injuries_top5 + away_injuries_top5 |
                   factor(home_season) | 0 | home_season, data = data)

stargazer(market_1, market_2, market_3, market_4, market_5, market_6, market_7, market_8,
          omit = c("Constant", "factor"),
          omit.stat = c("adj.rsq", "ser"), type = "latex", df = FALSE,
          add.lines = list(c("Home team FEs", "No", "Yes", "Yes", "No", "No", "Yes", "Yes", "No"),
                           c("Season FEs", "No", "Yes", "Yes", "No", "No", "Yes", "Yes", "No"),
                           c("Home team-Season FEs", "No", "No", "No", "Yes", "No", "No", "No", "Yes")),
          covariate.labels = c("Home Star Injuries", "Away Star Injuries",
                               "Home Standing", "Away Standing"),
          title = "Measuring the Productivity of Star Players: Regressions of Market Expectations on Star Injuries",
          dep.var.labels   = c("Prob. Home Win", "Expected Margin"))


# Table 6 - What Constitutes a Superstar? Regressions of Team Performance on Injuries to Players at the 90th Percentile

top10_1 <- felm(home_win ~ home_injuries_top10 + away_injuries_top10 |
                  0| 0 | home, data = data)

top10_2 <- felm(home_win ~ home_injuries_top10 + away_injuries_top10 |
                  factor(home) + factor(year) | 0 | home, data = data)

top10_3 <- felm(home_win ~ home_injuries_top10 + away_injuries_top10 + standing_home + standing_away |
                  factor(home) + factor(year) | 0 | home, data = data)

top10_4 <- felm(home_win ~ home_injuries_top10 + away_injuries_top10 |
                  factor(home_season) | 0 | home_season, data = data)

top10_5 <- felm(margin ~ home_injuries_top10 + away_injuries_top10 |
                  0 | 0 | home, data = data)

top10_6 <- felm(margin ~ home_injuries_top10 + away_injuries_top10 |
                  factor(home) + factor(year) | 0 | home, data = data)

top10_7 <- felm(margin ~ home_injuries_top10 + away_injuries_top10 + standing_home + standing_away |
                  factor(home) + factor(year) | 0 | home, data = data)

top10_8 <- felm(margin ~ home_injuries_top10 + away_injuries_top10 |
                  factor(home_season) | 0 | home_season, data = data)

stargazer(top10_1, top10_2, top10_3, top10_4, top10_5, top10_6, top10_7, top10_8, 
          omit = c("Constant", "factor"),
          omit.stat = c("adj.rsq", "ser"), type = "latex", df = FALSE,
          add.lines = list(c("Home team FEs", "No", "Yes", "Yes", "No", "No", "Yes", "Yes", "No"),
                           c("Season FEs", "No", "Yes", "Yes", "No", "No", "Yes", "Yes", "No"),
                           c("Home team-Season FEs", "No", "No", "No", "Yes", "No", "No", "No", "Yes")),
          covariate.labels = c("Home Star Injuries (90th)", "Away Star Injuries (90th)",
                               "Home Standing", "Away Standing"),
          title = "What Constitutes a Superstar? Regressions of Market Expectations on Injuries to Players at the 90th Percentile",
          dep.var.labels   = c("Home Win = 1", "Margin"))

# Table 7 - What Constitutes a Superstar? Regressions of Betting Market Expectations on Injuries to Players at the 90th to 95th Percentile

other5th_1 <- felm(home_win ~ home_injuries_95th_to_90th + away_injuries_95th_to_90th |
                     0 | 0 | home, data = data)

other5th_2 <- felm(home_win ~ home_injuries_95th_to_90th + away_injuries_95th_to_90th |
                     factor(home) + factor(year) | 0 | home, data = data)

other5th_3 <- felm(home_win ~ home_injuries_95th_to_90th + away_injuries_95th_to_90th + standing_home + standing_away |
                     factor(home) + factor(year) | 0 | home, data = data)

other5th_4 <- felm(home_win ~ home_injuries_95th_to_90th + away_injuries_95th_to_90th |
                     factor(home_season) | 0 | home_season, data = data)

other5th_5 <- felm(margin ~ home_injuries_95th_to_90th + away_injuries_95th_to_90th |
                     0 | 0 | home, data = data)

other5th_6 <- felm(margin ~ home_injuries_95th_to_90th + away_injuries_95th_to_90th |
                     factor(home) + factor(year) | 0 | home, data = data)

other5th_7 <- felm(margin ~ home_injuries_95th_to_90th + away_injuries_95th_to_90th + standing_home + standing_away |
                     factor(home) + factor(year) | 0 | home, data = data)

other5th_8 <- felm(margin ~ home_injuries_95th_to_90th + away_injuries_95th_to_90th |
                     factor(home_season) | 0 | home_season, data = data)

stargazer(other5th_1, other5th_2, other5th_3, other5th_4, other5th_5, other5th_6,
          other5th_7, other5th_8,
          omit = c("Constant", "factor"),
          omit.stat = c("adj.rsq", "ser"), type = "latex", df = FALSE,
          add.lines = list(c("Home team FEs", "No", "Yes", "Yes", "No", "No", "Yes", "Yes", "No"),
                           c("Season FEs", "No", "Yes", "Yes", "No", "No", "Yes", "Yes", "No"),
                           c("Home team-Season FEs", "No", "No", "No", "Yes", "No", "No", "No", "Yes")),
          covariate.labels = c("Home Star Injuries (90th-95th)", "Away Star Injuries (90th-95th)",
                               "Home Standing", "Away Standing"),
          title = "What Constitutes a Superstar? Regressions of Market Expectations on Injuries to Players at the 90th to 95th Percentile",
          dep.var.labels   = c("Home Win = 1", "Margin"))

# Figure 1 - Distribution of treatment effects from regression of game outcome on placebo injuries to star players on the away team.

vec_home <- pull(data, home_injuries_top5)
vec_away <- pull(data, away_injuries_top5)

main_results_placebo = function() {
  data_sim <- data %>%
    mutate(away_injuries_top5_sim = sample(vec_away, nrow(data), replace = FALSE, prob = NULL)) %>%
    mutate(home_injuries_top5_sim = sample(vec_home, nrow(data), replace = FALSE, prob = NULL)) 
  main_placebo <- felm(home_win ~ away_injuries_top5_sim + home_injuries_top5_sim + standing_home + standing_away|
                         factor(home) + factor(year) | 0 | 0, data = data_sim)
  main_placebo
}

sims_main = rerun(1000, main_results_placebo())

x_main <- t(sapply(sims_main,coefficients))
x_main <- as.data.frame(as.table(x_main))

quantile(x_main$Freq, c(.05, .95))

ggplot(x_main, aes(Freq)) +
  geom_density(adjust = 4, fill="grey", alpha=.5) + 
  scale_x_continuous(limits=c(-0.40, 0.30)) +
  geom_vline(xintercept = 0.1392, linetype = "dashed") + 
  geom_vline(xintercept = quantile(x_main$Freq, c(.05)), linetype = "dotted") +
  geom_vline(xintercept = quantile(x_main$Freq, c(.95)), linetype = "dotted") + 
  theme_bw(base_size = 20) + 
  xlab("Coefficient Estimate") + ylab("Density") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggsave("placebo_1.png")


# Figure 2 - Distribution of treatment effects from regression of game outcome on placebo injuries to star players on the home team.

main_results_2_placebo = function() {
  data_sim <- data %>%
    mutate(away_injuries_top5_sim = sample(vec_away, nrow(data), replace = FALSE, prob = NULL)) %>%
    mutate(home_injuries_top5_sim = sample(vec_home, nrow(data), replace = FALSE, prob = NULL)) 
  main_2_placebo <- felm(margin ~ away_injuries_top5_sim + home_injuries_top5_sim + standing_home + standing_away|
                           factor(home) + factor(year) | 0 | 0, data = data_sim)
  main_2_placebo
}

sims_main_2 = rerun(1000, main_results_2_placebo())

x_main_2 <- t(sapply(sims_main_2,coefficients))
x_main_2 <- as.data.frame(as.table(x_main_2))

quantile(x_main_2$Freq, c(.05, .95))

ggplot(x_main_2, aes(Freq)) +
  geom_density(adjust = 4, fill="grey", alpha=.5) + 
  scale_x_continuous(limits=c(-20, 20)) +
  geom_vline(xintercept = 14.1244, linetype = "dashed") + 
  geom_vline(xintercept = quantile(x_main_2$Freq, c(.05)), linetype = "dotted") +
  geom_vline(xintercept = quantile(x_main_2$Freq, c(.95)), linetype = "dotted") + 
  theme_bw(base_size = 20) + 
  xlab("Coefficient Estimate") + ylab("Density") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggsave("placebo_2.png")
