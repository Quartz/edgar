library(dplyr)
library(readr)
library(stringr)
library(xml2)

source("edgar.R")

downloadMasterIndex()
master.index <- parseMasterIndex()

# Bezos
filings <- master.index %>%
  filter(cik ==  1018724, form.type == "4")

sapply(filings$filename, downloadFiling)

# 
# text <- read_file("cache/bezos-1.xml")
# 
# i <- str_locate(text, fixed("<?xml"))[1]
# j <- str_locate(text, fixed("\n</XML>"))[1]
# 
# text.xml <- str_sub(text, i, j - 1)
# 
# xml <- read_xml(text.xml)
# 
# transactions <- xml %>%
#   xml_find_all(".//nonDerivativeTransaction")
#   
# titles <- transactions %>% xml_find_all(".//securityTitle/value") %>% xml_text()
# dates <- transactions %>% xml_find_all(".//transactionDate/value") %>% xml_text()
# shares <- transactions %>% xml_find_all(".//transactionAmounts/transactionShares/value") %>% xml_text()
# prices <- transactions %>% xml_find_all(".//transactionAmounts/transactionPricePerShare/value") %>% xml_text()
# codes <- transactions %>% xml_find_all(".//transactionAmounts/transactionAcquiredDisposedCode/value") %>% xml_text()
# ownerships <- transactions %>% xml_find_all(".//ownershipNature/directOrIndirectOwnership/value") %>% xml_text()
# 
# results <- data.frame(titles, dates, shares, prices, codes, ownerships)
# 
