# Project 3: Revenue Forecasting & Scenario Planning (Beginner-Friendly)
# -------------------------------------------------------------------
# What you will build:
# 1) Baseline 6-month revenue forecast (time-series)
# 2) Driver-based model (marketing spend, price, promos, seasonality)
# 3) Best/Base/Worst scenario comparison

# --------- 0) Install packages (run once) ---------
# install.packages(c("tidyverse","lubridate","forecast","broom"))

library(tidyverse)
library(lubridate)
library(forecast)
library(broom)

# --------- 1) Load data ---------
df <- read_csv("project3_revenue_forecasting_dataset.csv")

# Convert month to Date (first day of month)
df <- df %>%
  mutate(month_date = ymd(paste0(month, "-01"))) %>%
  arrange(month_date)

glimpse(df)

# --------- 2) Quick visuals (sanity checks) ---------
ggplot(df, aes(x = month_date, y = revenue)) +
  geom_line() +
  labs(title = "Monthly Revenue (Historical)", x = "Month", y = "Revenue") +
  theme_minimal()

ggplot(df, aes(x = month_date, y = marketing_spend)) +
  geom_line() +
  labs(title = "Monthly Marketing Spend", x = "Month", y = "Marketing Spend") +
  theme_minimal()

# --------- 3) Baseline time-series forecast (ETS) ---------
rev_ts <- ts(df$revenue, start = c(year(min(df$month_date)), month(min(df$month_date))), frequency = 12)

fit_ets <- ets(rev_ts)
fc_ets <- forecast(fit_ets, h = 6)

autoplot(fc_ets) +
  labs(title = "Baseline Revenue Forecast (ETS)", x = "Month", y = "Revenue")

baseline_df <- tibble(
  month_date = seq(from = max(df$month_date) %m+% months(1), by = "1 month", length.out = 6),
  baseline_revenue = as.numeric(fc_ets$mean)
) %>%
  mutate(month = format(month_date, "%Y-%m"))

# --------- 4) Driver-based model (regression) ---------
df_model <- df %>%
  mutate(log_mkt = log(marketing_spend),
         month_factor = factor(month_num))

lm_fit <- lm(revenue ~ log_mkt + avg_price + promo_flag + month_factor, data = df_model)

summary(lm_fit)
tidy(lm_fit)

# --------- 5) Future 6 months (base assumptions) ---------
last_month <- max(df$month_date)
future_months <- seq(from = last_month %m+% months(1), by = "1 month", length.out = 6)

base_mkt <- mean(tail(df$marketing_spend, 3))
base_price <- mean(tail(df$avg_price, 3))

future_base <- tibble(
  month_date = future_months,
  month_num = month(future_months),
  month_factor = factor(month(future_months), levels = 1:12),
  marketing_spend = base_mkt,
  avg_price = base_price,
  promo_flag = if_else(month_num %in% c(11,12,7), 1, 0)
) %>%
  mutate(log_mkt = log(marketing_spend),
         month = format(month_date, "%Y-%m"))

# --------- 6) Scenarios ---------
scenario_base <- future_base %>% mutate(scenario = "Base")

scenario_best <- future_base %>%
  mutate(marketing_spend = marketing_spend * 1.10,
         avg_price = avg_price * 1.02,
         log_mkt = log(marketing_spend),
         scenario = "Best")

scenario_worst <- future_base %>%
  mutate(marketing_spend = marketing_spend * 0.90,
         avg_price = avg_price * 0.98,
         promo_flag = 0,
         log_mkt = log(marketing_spend),
         scenario = "Worst")

future_all <- bind_rows(scenario_base, scenario_best, scenario_worst) %>%
  mutate(pred_revenue = predict(lm_fit, newdata = .))

# --------- 7) Scenario chart ---------
ggplot(future_all, aes(x = month_date, y = pred_revenue, color = scenario, group = scenario)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Scenario Revenue Forecast (Driver-Based Model)",
       x = "Month", y = "Predicted Revenue") +
  theme_minimal()

# Baseline vs driver-based base scenario (table)
scenario_compare <- future_all %>%
  filter(scenario == "Base") %>%
  left_join(baseline_df, by = c("month_date","month")) %>%
  select(month, baseline_revenue, pred_revenue)

print(scenario_compare)

# --------- 8) Save outputs ---------
write_csv(future_all, "scenario_forecast_6m.csv")
write_csv(baseline_df, "baseline_forecast_6m.csv")

# Interview line:
# "I created a baseline ETS forecast and a driver-based model with marketing, price, promos, and seasonality,
# then simulated best/base/worst scenarios to quantify upside/downside risk for planning."
