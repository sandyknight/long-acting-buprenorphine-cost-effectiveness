library(data.table)

library(tidyverse)


id <- fread("data/raw/K3anon_FullDataset_for_VfM.csv")

id <- id[drug_grp == "Opiate", ]

id <- id[, .(client_random_id, utla23cd)] |> unique()

df <-
  data.table::fread("data/raw/SIR_table_for_VfM_linked.csv")

df <- df[, .(client_random_id, year, phbudi_any)]

df <- data.table::merge.data.table(df, id, by = "client_random_id")

df <- df[, .(phbudi_any = sum(phbudi_any)), by = .(client_random_id, year, utla23cd)]

df[, any_lab := fifelse(phbudi_any > 0, "LAB", "Other")]

df <- df[, .(count = .N), by = .(year, utla23cd, any_lab)]

df <- df[year > 2019, ]

df |>
  filter(any_lab == "LAB") |>
  mutate(year = forcats::as_factor(year)) |>
  ggplot(aes(x = year, y = count)) +
  geom_col(aes(fill = "LAB")) +
  hrbrthemes::theme_ipsum(grid = FALSE, axis = TRUE) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Year", y = "(n)", title = "Count of clients recieving long-acting\nbuprenorphine for opioid use") +
  theme(legend.position = "none", plot.title.position = "plot")

df |>
  pivot_wider(names_from = any_lab, values_from = count, values_fill = 0) |>
  group_by(year) |>
  summarise(across(Other:LAB, sum)) |>
  mutate(lab_rate = LAB / (LAB + Other)) |>
  mutate(year = as_factor(year)) |>
  ggplot(aes(x = year, y = lab_rate)) +
  geom_col(aes(fill = "LAB")) +
  hrbrthemes::theme_ipsum(grid = FALSE, axis = TRUE) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Year", y = "(%)", title = "Percentage of clients recieving long-acting\nbuprenorphine for opioid use") +
  theme(legend.position = "none", plot.title.position = "plot")

dfla <-
  df |>
  pivot_wider(names_from = any_lab, values_from = count, values_fill = 0) |>
  mutate(year = as_factor(year)) |>
  group_by(utla23cd, year) |>
  summarise(across(Other:LAB, sum)) |>
  mutate(lab_rate = LAB / (LAB + Other)) |>
  ungroup()

dfla <- as.data.table(dfla)

geo <- fread("data/raw/Upper_Tier_Local_Authorities_(April_2023)_Names_and_Codes_in_the_United_Kingdom.csv")

setnames(geo, tolower)

geo <- geo[, .(utla23cd, utla23nm)]

dfla <- merge.data.table(dfla, geo, by = "utla23cd")
dt <- dfla[, .(year, utla23nm, LAB, lab_rate)]

setnames(dt, c("year", "area", "count", "rate"))

dt[, year := as.integer(rep(levels(year), 152))]






# Calculate quartiles and ranges for each year
summary_stats <- dt[, .(
  mean = mean(rate),
  IQR = IQR(rate),
  min = min(rate),
  q1 = quantile(rate, 0.25),
  median = median(rate),
  q3 = quantile(rate, 0.75),
  max = max(rate),
  # Also get min/max for each quartile group
  q1_min = min(rate[rate <= quantile(rate, 0.25)]),
  q1_max = max(rate[rate <= quantile(rate, 0.25)]),
  q2_min = min(rate[rate > quantile(rate, 0.25) & rate <= quantile(rate, 0.5)]),
  q2_max = max(rate[rate > quantile(rate, 0.25) & rate <= quantile(rate, 0.5)]),
  q3_min = min(rate[rate > quantile(rate, 0.5) & rate <= quantile(rate, 0.75)]),
  q3_max = max(rate[rate > quantile(rate, 0.5) & rate <= quantile(rate, 0.75)]),
  q4_min = min(rate[rate > quantile(rate, 0.75)]),
  q4_max = max(rate[rate > quantile(rate, 0.75)])
), by = year]

# Print formatted table
# Print formatted table with percentages and Inf handling
summary_stats[, .(
  Year = year,
  `Q1 Range` = sprintf(
    "%.1f%% - %.1f%%",
    ifelse(is.infinite(q1_min), 0, q1_min * 100),
    ifelse(is.infinite(q1_max), 0, q1_max * 100)
  ),
  `Q2 Range` = sprintf(
    "%.1f%% - %.1f%%",
    ifelse(is.infinite(q2_min), 0, q2_min * 100),
    ifelse(is.infinite(q2_max), 0, q2_max * 100)
  ),
  `Q3 Range` = sprintf(
    "%.1f%% - %.1f%%",
    ifelse(is.infinite(q3_min), 0, q3_min * 100),
    ifelse(is.infinite(q3_max), 0, q3_max * 100)
  ),
  `Q4 Range` = sprintf(
    "%.1f%% - %.1f%%",
    ifelse(is.infinite(q4_min), 0, q4_min * 100),
    ifelse(is.infinite(q4_max), 0, q4_max * 100)
  ),
  `Overall IQR` = sprintf(
    "%.1f%% - %.1f%%",
    ifelse(is.infinite(q1), 0, q1 * 100),
    ifelse(is.infinite(q3), 0, q3 * 100)
  )
)] |> fwrite("iqr_summary.csv")


p <-
  dt |>
  ggplot(aes(x = year, y = rate, group = year)) +
  geom_boxplot(pch = 2) +
  geom_text(
    data = summary_stats[year > 2021, ],
    aes(x = year, y = mean, label = scales::percent(mean)), size = 4
  ) +
  geom_label(
    data = summary_stats,
    aes(x = year, y = max, label = scales::percent(max)), size = 4, colour = "white", fill = afcolours::af_colours(n = 1),
    label.padding = unit(0.2, "cm")
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = NULL)


png(filename = "plots/annual_boxplot.png", width = 12, height = 12, units = "cm", res = 300)
p
dev.off()



#
# # Create visualization using ggplot2
# library(ggplot2)
#
# # Basic boxplot with quartile ranges
# ggplot(summary_stats) +
#   # Add lines for full range
#   geom_segment(aes(x = year, xend = year,
#                    y = min, yend = max),
#                color = "grey50", size = 0.5) +
#   # Add rectangle for IQR
#   geom_rect(aes(xmin = year - 0.25, xmax = year + 0.25,
#                 ymin = q1, ymax = q3),
#             fill = "lightblue", alpha = 0.5) +
#   # Add median line
#   geom_segment(aes(x = year - 0.25, xend = year + 0.25,
#                    y = median, yend = median),
#                color = "darkblue", size = 1) +
#   # Add points for quartile ranges
#   geom_point(aes(x = year, y = q1_min), color = "red", size = 2) +
#   geom_point(aes(x = year, y = q1_max), color = "red", size = 2) +
#   geom_point(aes(x = year, y = q2_min), color = "green", size = 2) +
#   geom_point(aes(x = year, y = q2_max), color = "green", size = 2) +
#   geom_point(aes(x = year, y = q3_min), color = "blue", size = 2) +
#   geom_point(aes(x = year, y = q3_max), color = "blue", size = 2) +
#   geom_point(aes(x = year, y = q4_min), color = "purple", size = 2) +
#   geom_point(aes(x = year, y = q4_max), color = "purple", size = 2) +
#   # Customize theme and labels
#   theme_minimal() +
#   labs(title = "Distribution of Buprenorphine Treatment Rates by Year",
#        subtitle = "Showing IQR (box) and range for each quartile (colored points)",
#        y = "Treatment Rate",
#        x = "Year") +
#   # Add legend for quartile ranges
#   annotate("text", x = max(summary_stats$year) + 0.5,
#            y = c(summary_stats$q1_max[1],
#                  summary_stats$q2_max[1],
#                  summary_stats$q3_max[1],
#                  summary_stats$q4_max[1]),
#            label = c("Q1 Range", "Q2 Range", "Q3 Range", "Q4 Range"),
#            color = c("red", "green", "blue", "purple"))
