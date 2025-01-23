library(data.table)
library(tidyverse)

id <- fread("data/raw/K3anon_FullDataset_for_VfM.csv")

id <- id[drug_grp == "Opiate", ]

id <- id[, .(client_random_id, utla23cd)] |> unique()

df <- 
  data.table::fread("data/raw/SIR_table_for_VfM_linked.csv")

df[, year := lubridate::year(submoddt)]

df <- df[ ,.(client_random_id, year, phbudi_any)]

df <- data.table::merge.data.table(df, id, by = "client_random_id")

df <- df[, .(phbudi_any = sum(phbudi_any)), by = .(client_random_id, year, utla23cd)]

df[, any_lab := fifelse(phbudi_any > 0, "LAB", "Other")]

df <- df[, .(count = .N), by = .(year, utla23cd, any_lab)]

df <- df[year > 2019,]

trch <- 
  data.table::fread("data/published_allocations_tranches.csv", select = c('Area code', 'Local authority', 'Tranche'))


data.table::setnames(trch, janitor::make_clean_names(names(trch)))

df <-
  data.table::merge.data.table(df, trch, by.x = "utla23cd", by.y = "area_code")


trch <- 
  df[, .(count = sum(count)), by = .(year, tranche, any_lab)]

trch <- 
  data.table::dcast(trch, year + tranche ~ any_lab)

p3 <- 
trch |> 
  ggplot(aes(x = year, y = LAB, group = tranche)) + 
  geom_col(aes(fill = as.integer(tranche)), position = "dodge", colour = "black") +
  scale_fill_gradient(breaks = c(1, 2, 3), guide = "legend") + 
  scale_y_continuous(labels = scales::comma) + 
  labs(fill = "Tranche", y = NULL, x = NULL) + 
  theme(legend.position = "top",
        legend.justification = "left",
        legend.title.position = "top")



png(filename = "plots/annual_count_by_trch.png", width = 12, height = 12, units = "cm", res = 300)
p3
dev.off()


