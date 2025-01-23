library(data.table)
library(tidyverse)

id <- fread("data/raw/K3anon_FullDataset_for_VfM.csv")

id <- id[drug_grp == "Opiate", ]

id <- id[, .(client_random_id, utla23cd)] |> unique()

df <-
  data.table::fread("data/raw/SIR_table_for_VfM_linked.csv")

df[, year := lubridate::year(submoddt)]

df <- df[, .(client_random_id, year, phbudi_any)]

df <- data.table::merge.data.table(df, id, by = "client_random_id")

df <- df[, .(phbudi_any = sum(phbudi_any)), by = .(client_random_id, year, utla23cd)]

df[, any_lab := fifelse(phbudi_any > 0, "LAB", "Other")]

df <- df[, .(count = .N), by = .(year, utla23cd, any_lab)]

df <- df[year > 2019, ]

trch <-
  data.table::fread("data/published_allocations_tranches.csv", select = c("Area code", "Local authority", "Tranche"))


data.table::setnames(trch, janitor::make_clean_names(names(trch)))

dt <-
  data.table::merge.data.table(df, trch, by.x = "utla23cd", by.y = "area_code")

dt |>
  fwrite("data/lab-data-by-tranche-and-area.csv")


dt |>
  group_by(year, tranche, any_lab) |>
  summarise(count = sum(count)) |>
  ungroup() |>
  as.data.table() |>
  dcast(year + tranche ~ any_lab) |>
  fwrite("data/lab-data-by-tranche.csv")

dt <- data.table::dcast(dt, year + tranche + utla23cd ~ any_lab, value.var = "count", fun.aggregate = mean)

dt[, rate := LAB / (LAB + Other)]

setnafill(dt, fill = 0, cols = c("LAB", "rate"))


p5 <-
  dt[, .(year, tranche = paste("Tranche", tranche), rate)] |>
  ggplot(aes(x = year, y = rate, group = year)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~tranche, ) +
  labs(x = NULL, y = NULL) +
  theme(
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(colour = "white", hjust = 0),
    plot.caption = element_text(hjust = 0)
  )



png(filename = "plots/annual_boxplot_trch.png", width = 24, height = 12, units = "cm", res = 300)
p5
dev.off()



p6 <-
  dt[, .(year, tranche = paste("Tranche", tranche), rate)] |>
  ggplot(aes(x = year, y = rate, group = year)) +
  geom_boxplot() +
  scale_y_log10(labels = scales::percent) +
  facet_wrap(~tranche, ) +
  labs(x = NULL, y = NULL) +
  theme(
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(colour = "white", hjust = 0),
    plot.caption = element_text(hjust = 0)
  )



png(filename = "plots/annual_boxplot_trch_log10.png", width = 24, height = 12, units = "cm", res = 300)
p6
dev.off()




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
), by = .(year, tranche)]


summary_stats[, .(
  Year = year,
  Tranche = tranche,
  `Mean rate` = mean,
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
)] |> write_csv("data/iqr_by_tranch.csv")
