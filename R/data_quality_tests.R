# Data quality

# Duplications

sum(duplicated(x)) == 0

# Missingness
sum(is.na(id[[x]]))

# Client count sense check with NDTMS published figures
#
dfchk <-
df[, .(client_random_id,
       n_jy,
       year = lubridate::year(submoddt),
       phbudi_any,
       phbupren_any)]

dfchk <-
  dfchk[, other_or_missing := data.table::fifelse(phbudi_any == 0  & phbupren_any == 0, 1, 0)]

dfchk <- unique(dfchk)

dfchk <-
  dfchk[, lapply(.SD, sum),
        by = year,
        .SDcols = c("phbudi_any", "phbupren_any", "other_or_missing")]

dfchk[, total := phbudi_any + phbupren_any + other_or_missing]

chkval <-
  data.table::fread("data/adult_opiate_ndtms.csv", select = seq(1, 6, 1))


chkval[!is.na(`All in treatment"), ]
