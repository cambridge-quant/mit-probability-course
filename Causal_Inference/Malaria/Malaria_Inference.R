# Program that determines causal inference between malaria and bed nets
# Does using insecticide-treated bed nets compared to no nets decrease the risk
# of contracting malaria after 1 year?

# Import libraries
library(tipr)
library(broom)
library(rsample)
library(halfmoon)
library(tidyverse)
library(conflicted)
library(propensity)
library(causalworkshop)

# Visualize data
net_data |>
  ggplot(aes(malaria_risk, fill = net)) +
  geom_density(color = NA, alpha = 0.8)

# Get distribution means
net_data |>
  group_by(net) |>
  summarize(malaria_risk = mean(malaria_risk))

# Produce a linear model
net_data |>
  lm(malaria_risk ~ net, data = _) |>
  tidy()

# Use logistic regression to get propensity scores
propensity_model <- glm(
  net ~ income + health + temperature,
  data = net_data,
  family = binomial()
)
# The first six propensity scores
head(predict(propensity_model, type = "response"))

# Get inverse probability weights
net_data_wts <- propensity_model |>
  augment(newdata = net_data, type.predict = "response") |>
  # .fitted is the value predicted by the model
  # for a given observation
  mutate(wts = wt_ate(.fitted, net))
net_data_wts |>
  select(net, .fitted, wts) |>
  head()

# Plot distribution of propensity score by distribution
ggplot(net_data_wts, aes(.fitted)) +
  geom_mirror_histogram(
    aes(fill = net),
    bins = 50
  ) +
  scale_y_continuous(labels = abs) +
  labs(x = "propensity score")

# Now do a weighted version
ggplot(net_data_wts, aes(.fitted)) +
  geom_mirror_histogram(
    aes(group = net),
    bins = 50
  ) +
  geom_mirror_histogram(
    aes(fill = net, weight = wts),
    bins = 50,
    alpha = .5
  ) +
  scale_y_continuous(labels = abs) +
  labs(x = "propensity score")

# Calculate and plot standardized mean differences for each confounder
plot_df <- tidy_smd(
  net_data_wts,
  c(income, health, temperature),
  .group = net,
  .wts = wts
)
ggplot(
  plot_df,
  aes(
    x = abs(smd),
    y = variable,
    group = method,
    color = method
  )
) +
  geom_love()

# Check distribution for extreme weights
net_data_wts |>
  ggplot(aes(wts)) +
  geom_density(fill = "#CC79A7", color = NA, alpha = 0.8)

# Create a linear model, this time with weights
net_data_wts |>
  lm(malaria_risk ~ net, data = _, weights = wts) |>
  tidy(conf.int = TRUE)

# Bootstrapping algorithm
fit_ipw <- function(split, ...) {
  # get bootstrapped data sample with `rsample::analysis()`
  .df <- analysis(split)
  # fit propensity score model
  propensity_model <- glm(
    net ~ income + health + temperature,
    data = .df,
    family = binomial()
  )
  # calculate inverse probability weights
  .df <- propensity_model |>
    augment(type.predict = "response", data = .df) |>
    mutate(wts = wt_ate(.fitted, net))
  # fit correctly bootstrapped ipw model
  lm(malaria_risk ~ net, data = .df, weights = wts) |>
    tidy()
}

bootstrapped_net_data <- bootstraps(
  net_data,
  times = 1000,
  # required to calculate CIs later
  apparent = TRUE
)
bootstrapped_net_data

# Create an estimated distribution
ipw_results <- bootstrapped_net_data |>
  mutate(boot_fits = map(splits, fit_ipw))
ipw_results
ipw_results |>
  mutate(
    estimate = map_dbl(
      boot_fits,
      # pull the `estimate` for `netTRUE` for each fit
      \(.fit) .fit |>
        filter(term == "netTRUE") |>
        pull(estimate)
    )
  ) |>
  ggplot(aes(estimate)) +
  geom_histogram(fill = "#D55E00FF", color = "white", alpha = 0.8)

# Calculate 95% confidence intervals
boot_estimate <- ipw_results |>
  # calculate T-statistic-based CIs
  int_t(boot_fits) |>
  filter(term == "netTRUE")
boot_estimate

# Tipping analysis
tipping_points <- tip_coef(boot_estimate$.upper, exposure_confounder_effect = 1:5)
tipping_points |>
  ggplot(aes(confounder_outcome_effect, exposure_confounder_effect)) +
  geom_line(color = "#009E73", linewidth = 1.1) +
  geom_point(fill = "#009E73", color = "white", size = 2.5, shape = 21) +
  labs(
    x = "Confounder-Outcome Effect",
    y = "Scaled mean differences in\n confounder between exposure groups"
  )
adjusted_estimates <- boot_estimate |>
  select(.estimate, .lower, .upper) |>
  unlist() |>
  adjust_coef_with_binary(
    exposed_confounder_prev = 0.26,
    unexposed_confounder_prev = 0.05,
    confounder_outcome_effect = -10
  )
adjusted_estimates
