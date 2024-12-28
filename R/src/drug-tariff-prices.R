url <-
  "https://www.nhsbsa.nhs.uk/sites/default/files/2024-11/Part%20VIIIA%20Dec%2024.xls.csv"

library(data.table)

df <- fread(url, skip = 2)

df <-
  df[grepl("Buprenorphine",x =  Medicine, ignore.case = TRUE, perl = TRUE),][V3 == "pre-filled disposable injection",]

df[,price := `Basic Price` / 100]

df
