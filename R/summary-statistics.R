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


p1 <-
  df |>
  filter(any_lab == "LAB") |>
  mutate(year = forcats::as_factor(year)) |>
  ggplot(aes(x = year, y = count)) +
  geom_col(aes(fill = "LAB")) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none")


png(filename = "plots/national_count.png", width = 12, height = 12, units = "cm", res = 300)
p1
dev.off()



national_summary <-
  df[, .(count = sum(count)), by = .(year, any_lab)]

national_summary <-
  data.table::dcast(national_summary, year ~ any_lab)


national_summary <-
  national_summary[, .(
    Year = year,
    "Long-acting buprenorphine" = LAB,
    "Other treatment for opioid use" = Other,
    Increase = c(0, diff(LAB)),
    "Relative increase" = (c(0, diff(LAB)) / lag(LAB))
  )]


national_summary |>
  fwrite("data/national_summary.csv")


national_summary[, .(Year,
  "Rate (%)" = `Long-acting buprenorphine` / `Other treatment for opioid use`
)][, .(Year,
  `Rate (%)`,
  "Change in rate (p.p.)" = c(0, diff(`Rate (%)`)) * 100
)] |>
  fwrite("data/national_rate.csv")


p2 <-
  national_summary[, .(Year,
    "Rate (%)" = `Long-acting buprenorphine` / `Other treatment for opioid use`
  )] |>
  ggplot(aes(x = Year, y = `Rate (%)`)) +
  geom_line() +
  geom_point(pch = 4) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = NULL) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 5)
  )

png(filename = "plots/national_rate.png", width = 6, height = 6, units = "cm", res = 300)
p2
dev.off()


df |>
  pivot_wider(names_from = any_lab, values_from = count, values_fill = 0) |>
  group_by(year) |>
  summarise(across(Other:LAB, sum)) |>
  mutate(lab_rate = LAB / (LAB + Other)) |>
  mutate(year = as_factor(year)) |>
  ggplot(aes(x = year, y = lab_rate)) +
  geom_col(aes(fill = "LAB")) +
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

dfla |>
  mutate(any_la_lab = if_else(LAB > 0, "Yes", "No")) |>
  select(year, utla23nm, any_la_lab) |>
  group_by(year, any_la_lab) |>
  tally() |>
  ggplot(aes(x = year, y = n)) +
  geom_col(aes(fill = any_la_lab))



dfla |>
  mutate(any_la_lab = if_else(LAB > 0, "Yes", "No")) |>
  select(year, utla23nm, any_la_lab) |>
  ggplot(aes(x = year, y = utla23nm)) +
  geom_tile(aes(fill = any_la_lab), colour = "black")

dfla_list <-
  dfla |>
  group_by(year) |>
  group_split()

dfla

# bind_rows(lapply(dfla_list, function(x) head(arrange(x, -lab_rate)))) |>
#   filter(year  == 2024) |>
#   select(utla23nm,Other,  LAB, lab_rate) |>
#   fwrite("data/processed/lafivetoplabrates.csv")
#
# merge.data.table(
# dfla[, .(mean_rate = mean(lab_rate)), by = year],
# dfla[, .(rate_iqr = IQR(lab_rate)), by = year]
# ) |> fwrite("data/processed/annualsummarystats.csv")
#
#
#
# data.table::fread("data/processed/lafivetoplabrates.csv") |>
# ggplot(aes(x = year,  y = lab_rate)) +
#   hrbrthemes::theme_ipsum(grid = FALSE, axis = TRUE) +
#   geom_boxplot() +
#   geom_text(data = means,
#             aes(y = mean_rate,
#                 label = if_else(year %in% c("2022", "2023", "2024"),
#                                 scales::percent(mean_rate), NA_character_))) +
#   scale_y_continuous(labels = scales::percent) +
#   labs(x = "Year", y = "(%)", title = "Percentage of clients recieving long-acting\nbuprenorphine for opioid use", subtitle = "by local authority") +
#   theme(legend.position = "none", plot.title.position = "plot")
#
#
#
