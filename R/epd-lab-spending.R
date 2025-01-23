library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)

options(ggplot2.discrete.colour = afcolours::af_colours())

df <- data.table::fread("data/epd-data.csv")

df[, ACTUAL_COST := as.numeric(ACTUAL_COST)]

df2 <- df[, .(ACTUAL_COST = sum(ACTUAL_COST)), by = .(YEAR_MONTH, BNF_DESCRIPTION)]

df2[, date := paste0(YEAR_MONTH, "01")]

df2[, date := lubridate::as_date(date)]

df2[, type := data.table::fcase(
  grepl(pattern = "oral", BNF_DESCRIPTION), "Oral buprenorphine",
  grepl(pattern = "sublingual", BNF_DESCRIPTION), "Sublingual buprenorphine",
  grepl(pattern = "prolonged", BNF_DESCRIPTION), "Prolonged-release injected buprenorphine",
  default = BNF_DESCRIPTION 
)]

df2 <- 
  df2[, .(ACTUAL_COST = sum(ACTUAL_COST)), by = .(date, type)]




afcharts::use_afcharts()
df2 |> 
  ggplot(aes(x = date, y = ACTUAL_COST, group = type)) + 
  geom_point(aes(colour = type)) +
  geom_smooth(aes(colour = type)) +
  scale_colour_discrete() +
  scale_y_continuous(labels = scales::label_currency())



p7 <- 
df2 |> 
  ggplot(aes(x = date, y = ACTUAL_COST, group = type)) + 
  geom_area(aes(fill = type)) +
  scale_y_continuous(labels = scales::label_currency(prefix = "£")) +
  labs(fill = NULL,
        y = NULL,
        x = NULL,
        title = "Buprenorphine spending by route of administration",
        subtitle = "England, January 2020 to September 2024", caption = "Source: English Prescribing Dataset (EPD)") +
  theme(plot.title.position = "plot", legend.direction = "vertical", legend.position = "top", legend.justification = "left")




png(filename = "plots/epd-bupe-spend.png", width = 24, height = 16, units = "cm", res = 300)
p7
dev.off()
