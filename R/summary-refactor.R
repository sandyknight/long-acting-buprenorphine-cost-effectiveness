library(data.table)

get_basic_dataset <-
  function () {
    # Main table, client data and treatment journey

    id <-
      data.table::fread("data/raw/K3anon_FullDataset_for_VfM.csv")

    # Opioid clients only
    id <-
      id[drug_grp == "Opiate", ]

    id <-
      id[, .(client_random_id, n_jy, utla23cd)]

    df <-
      data.table::fread("data/raw/SIR_table_for_VfM_linked.csv")

    ## phbudi_any
    ## Calculated field
    ## If any of the depot buprenorphine sub-interventions are ticked
    ## 1 = Yes, 0 = No/Missing

    # 426 duplicates to remove
    df <- unique(df)

    df <-
      df[, .(client_random_id,
             n_jy,
             submoddt,
             phbudi_any,
             other_ost = data.table::fifelse(phbudi_any == 1, 0L, 1L))]

    df <-
      data.table::merge.data.table(df, id, by = c("client_random_id", "n_jy"))

    # Get year from SIR date
    df[, year := lubridate::year(submoddt)]


    # Clients can have multiple SIRs per year
    # We want to count the number of people that had phbudi_any
    # at some point in the year

    # Remove SIR dates
    df <-
      df[, .(year, utla23cd, client_random_id, phbudi_any, other_ost)]

    # Keep unique rows

    df <- unique(df)

    # There will still be some clients with > 1 row
    # For instance where they changed sub-intervention mid year
    # But there should be no one counted twice has having phbudi_any
    return(df)
  }

df <-
  get_basic_dataset()


summarise_by

national_counts <-
  df[, lapply(.SD,
              sum),
     by = year,
     .SDcols = c("phbudi_any",
                 "other_ost")]

national_counts[, total := phbudi_any + ost_other]
national_counts[, lab_rate := phbudi_any / total]

# LA count
local_counts <- j
  df[, lapply(.SD, sum), by = .(year, utla23cd), .SDcols = c("phbudi_any", "other_ost")]

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
