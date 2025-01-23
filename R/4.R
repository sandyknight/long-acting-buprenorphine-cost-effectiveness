library(data.table)
library(lubridate)
library(tidyverse)
library(hrbrthemes)

id <- fread("data/raw/K3anon_FullDataset_for_VfM.csv")

id <- id[drug_grp == "Opiate", ]

id <- id[, .(client_random_id, utla23cd)] |> unique()

df <-
  data.table::fread("data/raw/SIR_table_for_VfM_linked.csv")

df[, year := lubridate::year(submoddt)]

max(as.Date(unique(df[["submoddt"]])))

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




data.table::setnames(trch, janitor::make_clean_names(names(trch)))

trch <- trch[, tranche := forcats::as_factor(tranche)]

df <-
  data.table::merge.data.table(df, trch, by.x = "utla23cd", by.y = "area_code")



trch <-
  df[, .(count = sum(count)), by = .(date, tranche, any_lab)]

trch <-
  data.table::dcast(trch, date + tranche ~ any_lab)


trch |>
  ggplot(aes(x = date, y = LAB, group = tranche)) +
  geom_line(aes(colour = as.integer(tranche))) +
  scale_colour_gradient(breaks = c(1, 2, 3), guide = "legend") +
  labs(colour = NULL, y = NULL, x = NULL) +
  theme(
    legend.position = "top",
    legend.justification = "left"
  )


p4 <-
  trch |>
  ggplot(aes(x = date, y = LAB / Other, group = tranche)) +
  geom_line(aes(colour = as.integer(tranche))) +
  scale_colour_gradient(breaks = c(1, 2, 3), guide = "legend") +
  labs(colour = NULL, y = NULL, x = NULL) +
  theme(
    legend.position = "top",
    legend.justification = "left"
  ) +
  scale_y_continuous(labels = scales::percent)


png(filename = "plots/monthly_rate_by_trch.png", width = 12, height = 12, units = "cm", res = 300)
p4
dev.off()
