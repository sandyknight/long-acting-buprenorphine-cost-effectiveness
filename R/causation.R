library(data.table)

id <-
  fread("data/raw/K3anon_FullDataset_for_VfM.csv")

id <- id[drug_grp == "Opiate", ]


df <-
  data.table::fread("data/raw/SIR_table_for_VfM_linked.csv")


df <- data.table::merge.data.table(df, id, by = c("client_random_id", "n_jy"))

total <-
  df[, .(disrsn, phbudi_any = fifelse(phbudi_any == 1L, "LAB", "Other"))][, .N, by = .(disrsn, phbudi_any)] |>
  data.table::dcast(disrsn ~ phbudi_any)

total[, N := LAB + Other]

total[, .(disrsn, LAB = sum(LAB) / N, Other = Other / sum(Other))]
