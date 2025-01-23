library(data.table)
library(tidyverse)

# Main table, treatment journeys 
id <- 
  data.table::fread("data/raw/K3anon_FullDataset_for_VfM.csv")


# Opioid clients only
id <- 
  id[drug_grp == "Opiate", ]


id <- 
  id[, .(client_random_id, n_jy, utla23cd)] 

df <- 
  data.table::fread("data/raw/SIR_table_for_VfM_linked.csv")

sum(duplicated(df))

df <- unique(df)

df <-
  df[ ,.(client_random_id, n_jy, submoddt, phbudi, phbudi_any)]

df <- 
  data.table::merge.data.table(df, id, by = c("client_random_id", "n_jy"))

sum(duplicated(df))

# Tranches

trch <- 
  data.table::fread("data/published_allocations_tranches.csv", select = c('Area code', 'Local authority', 'Tranche'))


data.table::setnames(trch, janitor::make_clean_names(names(trch)))




df <-
  data.table::merge.data.table(df, trch, by.x = "utla23cd", by.y = "area_code")


trch <- 
  df[, .(count = sum(count)), by = .(year, tranche, any_lab)]

trch <- 
  data.table::dcast(trch, year + tranche ~ any_lab)