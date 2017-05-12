library(dplyr)
library(readr)
library(stringr)
library(xml2)

source("edgar.R")

DownloadMasterIndex()
master.index <- ParseMasterIndex()

# Amazon/Bezos: 1018724
# Facebook/Zuckerberg: 1548760
# Apple/Cook: 1214156
# Google/Brin: 1295032
# Google/Page: 1295231
# Tesla/Musk: 1494730

filings <- master.index %>%
  filter(form.type == "4")

invisible(sapply(filings$filename, DownloadFiling))

results <- bind_rows(lapply(filings$filename, ParseForm4))

write_csv(results, "everybody.csv")
