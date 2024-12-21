# Packages ----------------------------------------------------------------
library(devtools)
library(excessmort) 
library(lubridate)
library(dplyr)
library(knitr)
library(ggplot2)

# Question 1 --------------------------------------------------------------
# Q1 - Examine the population sizes by age group and sex.
# Describe any interesting patterns.
# Original ages intervals
fig_1 <- puerto_rico_counts |>
  filter(between(year(date), 2002, 2018)) |>
  ggplot(aes(date, population, color = agegroup)) +
  geom_line() +
  facet_wrap(~ sex, labeller = labeller(sex = c("male" = "Male", "female" = "Female"))) +
  labs(
    title = "Population Trends in Puerto Rico (2002–2018)",
    subtitle = "Population distribution by sex and age group over time",
    x = "Year",
    y = "Population Size",
    color = "Age Group (Years)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.5, "cm"),
    legend.spacing.y = unit(0.2, "cm"),
    legend.spacing.x = unit(0.2, "cm"),
    legend.margin = margin(t = 2, r = 2, b = 2, l = 2)
    )

fig_1

ggsave(file.path(getwd(), "../docs/fig_1.png"), plot = fig_1, width = 6, height = 4, dpi = 300)

# Question 2 --------------------------------------------------------------
# Use data from before 2017 to estimate expected mortality and a standard 
# deviation for each week. Do this by age group and sex. Describe tendencies 
# you observe. You can combine data into bigger age groups if the data show 
# they have similar death rates.

# Aggregating data by age groups and summarize
counts <- collapse_counts_by_age(puerto_rico_counts,
                                 breaks = c(0, 5, 20, 40, 60, 75, Inf)) |>
  group_by(date, agegroup, sex) |>
  summarize(population = mean(population),
            outcome = sum(outcome),
            .groups = "drop")

# Weekly counts
weekly_counts <- counts |>
  filter(between(year(date), 2002, 2016)) |>
  mutate(date = floor_date(date, unit = "week", week_start = 3)) |>
  group_by(date, sex, agegroup) |>
  summarize(outcome = sum(outcome),
            population = mean(population),
            days = n(),
            .groups = "drop") |>
  filter(days == 7) |>
  select(-days)

# Fitting expected counts without excluding any dates
expected <- weekly_counts |>
  group_by(agegroup, sex) |>
  group_split() |>
  lapply(function(df) {
    compute_expected(
      counts = df,
      exclude = NULL,
      harmonics = 2,
      trend.knots.per.year = 6,
      keep.components = TRUE,
      verbose = TRUE,
      weekday.effect = TRUE
    )
  })

# Extract metadata for agegroup and sex
metadata_expected <- weekly_counts |>
  group_by(agegroup, sex) |>
  group_keys()

# Combine results with metadata into a single data frame
combined_results <- Map(function(fit, meta) {
  fit$agegroup <- meta$agegroup
  fit$sex <- meta$sex
  return(fit)
}, expected, split(metadata_expected, seq_len(nrow(metadata_expected))))

# Bind all results into a single data frame
combined_results_df <- do.call(rbind, combined_results)

# Expected Mortality Rates by Age Group and Sex
fig_2 <- combined_results_df |>
  group_by(date, agegroup, sex, population) |>
  summarize(expected = sum(expected),
            observed = sum(outcome),
            expected_rate = expected/sum(population)*100000,
            observed_rate = observed/sum(population)*100000,
            .groups = "drop") |>
  ggplot(aes(x = date)) + 
  facet_grid(agegroup ~ sex, scales = "free_y", labeller = labeller(sex = c("male" = "Male", "female" = "Female"))) +
  geom_point(aes(y = observed_rate), 
             alpha = 0.5) + 
  geom_line(aes(y = expected_rate), size = 0.7, color = "3366FF") + 
  scale_y_continuous(labels = scales::comma) + 
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 years") +
  labs(
    title = "Expected and Observed Death Rate in Puerto Rico (2002-2016)",
    subtitle = "Rates distribution by sex and age group over time",
    x = "Date",
    y = "Weekly Rate per 100,000 People",
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fig_2

ggsave(file.path(getwd(), "../docs/fig_2.png"), plot = fig_2, width = 6, height = 4, dpi = 300)

# Expected Mortality Rate by Age Group and Sex — Another way
fig_3 <- combined_results_df |>
  group_by(date, agegroup, sex, population) |>
  summarize(expected = sum(expected),
            observed = sum(outcome),
            expected_rate = expected/sum(population)*100000,
            observed_rate = observed/sum(population)*100000,
            .groups = "drop") |>
  ggplot(aes(date, expected_rate, color = agegroup)) +
  geom_line() +
  facet_wrap(~ sex, labeller = labeller(sex = c("male" = "Male", "female" = "Female"))) +
  labs(
    title = "Expected Death Rate in Puerto Rico (2002-2016)",
    subtitle = "Rates distribution by sex and age group over time",
    x = "Year",
    y = "Expected Rate per 100,000 People",
    color = "Age Group (Years)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.5, "cm"),
    legend.spacing.y = unit(0.2, "cm"),
    legend.spacing.x = unit(0.2, "cm"),
    legend.margin = margin(t = 2, r = 2, b = 2, l = 2)
  )

fig_3

ggsave(file.path(getwd(), "../docs/fig_3.png"), plot = fig_3, width = 6, height = 4, dpi = 300)

# Question 3 --------------------------------------------------------------
# Explore the data to see if there are periods during or before 2017 that appear
# to have excess mortality. If so, explain and recompute expected death rates
# removing these periods.

# Weekly counts for Q3
weekly_counts_q3 <- counts |>
  filter(between(year(date), 2002, 2018)) |>
  mutate(date = floor_date(date, unit = "week", week_start = 3)) |>
  group_by(date, sex, agegroup) |>
  summarize(outcome = sum(outcome),
            population = mean(population),
            days = n(),
            .groups = "drop") |>
  filter(days == 7) |>
  select(-days)

# Hurricane dates and dates to exclude when fitting models
hurricane_dates_q3 <- as.Date(c("2017-09-06", "2017-09-20"))
hurricane_dates_q4 <- floor_date(hurricane_dates_q3, unit = "week", week_start = 3)
hurricane_effect_ends_q3  <- as.Date(c("2018-03-06", "2018-03-20"))
hurricane_effect_ends_q4 <- floor_date(hurricane_effect_ends_q3, unit = "week", week_start = 3)
names(hurricane_dates_q3) <- c("Irma", "Maria")

exclude_dates_q3 <- c(seq(hurricane_dates_q3[1], hurricane_effect_ends_q3[1], by = "week"), # Hurricane Irma
                   seq(hurricane_dates_q3[2], hurricane_effect_ends_q3[2], by = "week"), # Hurricane Maria
                   seq(as.Date("2004-07-01"), as.Date("2005-08-01"), by = "week"), # Dengue Fever
                   seq(as.Date("2014-04-01"), as.Date("2015-10-01"), by = "week")) # Chikungunya
exclude_dates_q3 <- floor_date(exclude_dates_q3, unit = "week", week_start = 3)

# Expected counts — excluding dates
expected_q3 <- weekly_counts_q3 |>
  group_by(agegroup, sex) |>
  group_split() |>
  lapply(function(df) {
    compute_expected(
      counts = df,
      exclude = exclude_dates_q3,
      harmonics = 2,
      trend.knots.per.year = 2,
      keep.components = TRUE,
      verbose = TRUE,
      weekday.effect = FALSE
    )
  })

# Extract metadata for agegroup and sex
metadata_expected_q3 <- weekly_counts_q3 |>
  group_by(agegroup, sex) |>
  group_keys()

# Combine results with metadata into a single data frame
combined_results_q3 <- Map(function(fit_q3, meta_q3) {
  fit_q3$agegroup <- meta_q3$agegroup
  fit_q3$sex <- meta_q3$sex
  return(fit_q3)
}, expected_q3, split(metadata_expected_q3, seq_len(nrow(metadata_expected_q3))))

# Bind all results into a single data frame
combined_results_df_q3 <- do.call(rbind, combined_results_q3)

# Expected Mortality by Age Group and Sex
extra_1 <- combined_results_df_q3 |>
  filter(excluded == "FALSE") |>
  group_by(date, agegroup, sex, population) |>
  summarize(expected = sum(expected),
            observed = sum(outcome),
            expected_rate = expected/sum(population)*100000,
            observed_rate = observed/sum(population)*100000,
            .groups = "drop") |>
  ggplot(aes(x = date)) + 
  facet_grid(agegroup ~ sex, scales = "free_y", labeller = labeller(sex = c("male" = "Male", "female" = "Female"))) +
  geom_line(aes(y = expected_rate), size = 0.7, color = "3366FF") + 
  ylab("Weekly Rate per 100,000 people") + scale_y_continuous(labels = scales::comma) + 
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 years") + ggtitle("") + 
  xlab("Date") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

extra_1

ggsave(file.path(getwd(), "../docs/extra_1.png"), plot = extra_1, width = 6, height = 4, dpi = 300)

# Question 4 --------------------------------------------------------------
# Estimate excess deaths for each week of 2017-2018. Make sure you define the weeks
# so that one of the weeks starts the day María made landfall. Comment on excess
# mortality. Which age groups were affected? Were men and women affected differently?

# Weekly counts for Q4
weekly_counts_q4 <- counts |>
  filter(between(year(date), 2002, 2018)) |>
  mutate(date = floor_date(date, unit = "week", week_start = 3)) |>
  group_by(date, sex, agegroup) |>
  summarize(outcome = sum(outcome),
            population = mean(population),
            days = n(),
            .groups = "drop") |>
  filter(days == 7) |>
  select(-days)

# Dates to be used for estimation of the correlated errors
control_dates <- seq(as.Date("2002-01-02"), as.Date("2013-12-31"), by = "week")
control_dates <- floor_date(control_dates, unit = "week", week_start = 3)

# Fitting the excess model
f_test <- weekly_counts_q4 |>
  group_by(agegroup, sex) |>
  group_split() |>
  lapply(function(data) {
    excess_model(data,
                 event = hurricane_dates_q3[2],
                 start = hurricane_dates_q3[2] - 365,
                 end = hurricane_dates_q3[2] + 365,
                 exclude = exclude_dates_q3,
                 weekday.effect = FALSE,
                 control.dates = control_dates,
                 knots.per.year = 4,
                 discontinuity = TRUE,
                 include.trend = TRUE,
                 model = "correlated")
  })

# Extract metadata for agegroup and sex
metadata <- weekly_counts_q4 |>
  group_by(agegroup, sex) |>
  group_keys()

# Combine results with metadata
results <- lapply(seq_along(f_test), function(i) {
  model <- f_test[[i]]  # Access each model
  
  # Extract components
  data.frame(
    date = model$date,
    observed = model$observed,
    expected = model$expected,
    fitted = model$fitted,
    se = model$se,
    sd = model$sd,
    agegroup = metadata$agegroup[i],
    sex = metadata$sex[i]
  )
})

# Combine all results into a single data frame
final_results <- do.call(rbind, results)

# Calculate Z value
z <- qnorm(1 - 0.05/2)

# Arrange some additional estimates
p <- final_results |>
  mutate (y = (observed - expected) / expected, increase = fitted)

# Graph
fig_4 <- p |> ggplot(aes(date, y)) +
  geom_ribbon(aes(ymin = increase - z * se, ymax = increase + z * se), alpha = 0.5, fill = "#3366FF") +
  geom_hline(yintercept = 0) + 
  ylab("% increase from expected death rate") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") +
  xlab("Date") + 
  geom_point(alpha = 0.3) +
  geom_line(aes(y = increase), size = 0.7, col = "#3366FF") + 
  ggtitle("Excess Mortality by Age Group and Gender in Puerto Rico") +
  facet_grid(agegroup ~ sex, scales = "free_y", labeller = labeller(sex = c("male" = "Male", "female" = "Female"))) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

fig_4

ggsave(file.path(getwd(), "../docs/fig_4.png"), plot = fig_4, width = 8, height = 6, dpi = 500)