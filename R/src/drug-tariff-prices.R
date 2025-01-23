url <- # Save tariff CSV URL as url
  "https://www.nhsbsa.nhs.uk/sites/default/files/2024-11/Part%20VIIIA%20Dec%2024.xls.csv"
# load data.table
library(data.table)
# Read data from url, skipping the first two lines
df <- fread(url, skip = 2)

df <- # Filter for medicines containing the string "buprenorphine"
  df[grepl("Buprenorphine", x = Medicine, ignore.case = TRUE, perl = TRUE), ][V3 == "pre-filled disposable injection", ]
# Filter for depot bubprenorphine
df[, price := `Basic Price` / 100] # Price in GBP
