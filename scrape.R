library(dplyr)
library(readr)
library(stringr)
library(xml2)

source("edgar.R")

DownloadMasterIndex()
master.index <- ParseMasterIndex()

# Bezos
filings <- master.index %>%
  filter(cik == 1018724, form.type == "4")

sapply(filings$filename, DownloadFiling)

results <- bind_rows(lapply(filings$filename, ParseForm4NonDerivativeSecurities))

write_csv(results, "bezos.csv")
