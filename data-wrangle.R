# libraries that used in this example
library(tidyverse)
library(dslabs)

# DATA WRANGLING
results <- polls_us_election_2016 %>%
  filter(
    state != "U.S." & # exclude national poll data
      !str_detect(state, "CD") & # we will not include data with "CD-#" at the end of state name
      enddate >= "2016-10-31" &
      (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarise(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))

results %>% arrange(abs(avg)) # the resulting table containing the average, sd, and n for each state.

results <- left_join(results, results_us_election_2016, by = "state")
results_us_election_2016 %>% filter(!state %in% results$state)
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm=TRUE), sd)) # NAs = median value


# Bayesian calculation
mu <- 0
tau <- 0.02
Bayesian_results <- results %>% mutate(sigma = sd/sqrt(n),
                                       B = sigma^2 / (sigma^2 + tau^2),
                                       posterior_mean = mu + (1-B)*(avg-mu),
                                       posterior_se = sqrt(1 / (1/sigma^2 + 1/tau^2))
) %>%
  arrange(abs(posterior_mean))


# generate 1,000 simulated results
mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2 / (sigma^2 + tau^2),
                     posterior_mean = mu + (1-B)*(avg-mu),
                     posterior_se = sqrt(1/ (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result>0, electoral_votes, 0)) %>%
    summarise(clinton = sum(clinton)) %>%
    .$clinton + 7
})
mean(clinton_EV>269)

# model with general bias
mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/n + bias_sd^2),
                     B = sigma^2 / (sigma^2 + tau^2),
                     posterior_mean = mu + (1-B)*(avg-mu),
                     posterior_se = sqrt(1/ (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result>0, electoral_votes, 0)) %>%
    summarise(clinton = sum(clinton)) %>% .$clinton + 7 # 7 for Rhode Island and D.C.
})
mean(clinton_EV_2>269)

# Forecasting
# we use only one pollster to avoid pollster effect.
one_pollster <- polls_us_election_2016 %>%
  filter(pollster == "Ipsos" & state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Since there's no pollster effect,
# perhaps the theoretical standard error will match the data-derived se.
se <- one_pollster %>%
  summarise(empirical = sd(spread),
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
one_pollster %>% ggplot(aes(spread)) +
  geom_histogram(binwidth = 0.01, col = "black", alpha = 0.5)

one_pollster %>% 
  ggplot(aes(enddate, spread)) + geom_point() +
  geom_smooth(method = "loess", span=0.1)

save(list = ls(), file = "rdas/results.rda")