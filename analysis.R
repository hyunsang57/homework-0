load("rdas/results.rda")
library(tidyverse)
library(dslabs)

Bayesian_results%>%
  ggplot(aes(avg, posterior_mean, size = n)) +
  geom_point(alpha = 0.5, show.legend = FALSE) +
  geom_abline(slope = 1, intercept = 0, lty = "dotted")

ggsave("figs/Bayesian-results-scatter-plot.png")

data.frame(clinton_EV, clinton_EV_2) %>% 
  gather(clinton, electoral_votes) %>%
  mutate(bias = ifelse(clinton == "clinton_EV", "no_bias", "with_bias")) %>%
  ggplot(aes(electoral_votes)) + 
  geom_histogram(binwidth=1, col="black", alpha = 0.5) + 
  xlab("results") +
  geom_vline(xintercept = 269, col="orange") +
  facet_grid(bias~., scales="free")

ggsave("figs/clinton-EV-comparison-barplot.png")

polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)

ggsave("figs/spread-predict-by-pollster-scatter-smooth.png")

polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump","Clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30,50))

ggsave("figs/trump-clinton-analysis.png")
