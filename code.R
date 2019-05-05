library(tidyverse)
library(dslabs)

results_us_election_2016 %>% top_n(10, electoral_votes)

results <- polls_us_election_2016 %>%
  filter(
    state != "U.S." & 
      !str_detect(state, "CD") & 
      enddate >= "2016-10-31" &
      (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarise(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))

results %>% arrange(abs(avg))

results <- left_join(results, results_us_election_2016, by = "state")
results_us_election_2016 %>% filter(!state %in% results$state)
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm=TRUE), sd))

# Bayesian calculation
mu <- 0
tau <- 0.02
Bayesian_results <- results %>% mutate(sigma = sd/sqrt(n),
                                       B = sigma^2 / (sigma^2 + tau^2),
                                       posterior_mean = mu + (1-B)*(avg-mu),
                                       posterior_se = sqrt(1 / (1/sigma^2 + 1/tau^2))
) %>%
  arrange(abs(posterior_mean))

Bayesian_results%>%
  ggplot(aes(avg, posterior_mean, size = n)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, lty = "dotted")

# generate 1,000 results
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

data.frame(clinton_EV) %>%
  ggplot(aes(clinton_EV)) +
  geom_histogram(binwidth = 1, col = "black", alpha = 0.5) +
  geom_vline(xintercept = 269)

# The above plot does not include general bias
# Now, let's include general bias in our model
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

data.frame(clinton_EV_2) %>%
  ggplot(aes(clinton_EV_2)) +
  geom_histogram(binwidth = 1, col = "black", alpha = 0.5) +
  geom_vline(xintercept = 269)

df <- data.frame(clinton_EV, clinton_EV_2) %>% 
  gather(clinton, electoral_votes) %>%
  mutate(bias = ifelse(clinton == "clinton_EV", "no_bias", "with_bias"))

df %>% ggplot(aes(electoral_votes)) + 
  geom_histogram(binwidth=1, col="black", alpha = 0.5) + 
  xlab("results") +
  geom_vline(xintercept = 269, col="orange") +
  facet_grid(bias~., scales="free")

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

polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)

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