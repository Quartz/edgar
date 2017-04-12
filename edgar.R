library(dplyr)
library(readr)
library(stringr)
library(xml2)

FIRST.YEAR = 2004  # Sarbanes-Oxley!
LAST.YEAR = 2016
INDEX.DIR = "indices"
CACHE.DIR = "cache"

# Download index files for all supported years
DownloadMasterIndex <- function() {
  for (year in c(FIRST.YEAR:LAST.YEAR)) {
    for (qtr in c(1:4)) {
      url <- paste0("https://www.sec.gov/Archives/edgar/full-index/", year, "/QTR", qtr, "/master.gz")
      dir.path <- paste0(INDEX.DIR, "/", year, "/", qtr)
      file.path <- paste0(dir.path, "/master.gz")
      
      if (file.exists(file.path)) {
        print(paste("Skipping", file.path, "(already exists)"))
        next
      }
      
      print(paste("Downloading", file.path))
      
      dir.create(dir.path, recursive = TRUE)
      download.file(url, file.path, method="curl", quiet = TRUE)
    }
  }
}

# Parse master index files for all years and join into a giant index dataframe
ParseMasterIndex <- function() {
  master.index <- data.frame()
  
  for (year in c(FIRST.YEAR:LAST.YEAR)) {
    for (qtr in c(1:4)) {
      dir.path <- paste0(INDEX.DIR, "/", year, "/", qtr)
      file.path <- paste0(dir.path, "/master.gz")
      
      print(paste("Parsing", file.path))
      
      data <- read_delim(
        file.path,
        "|",
        skip = 11,
        col_names = c("cik", "company.name", "form.type", "date.filed", "filename")
      )
      
      master.index <- rbind(master.index, data)
    }
  }
  
  return(master.index)
}

# Download a specific filing by index path
DownloadFiling <- function(path) {
  if (file.exists(path)) {
    print(paste("Skipping", path, "(already exists)"))
    
    return()
  }
  
  url <- paste0("https://www.sec.gov/Archives/", path)
  bits <- str_locate(path, "(?<=/)[^/]*?$")
  dir <- str_sub(path, 1, bits[1] - 2)
  filename <- str_sub(path, bits[1])
  
  dir.create(dir, recursive = TRUE)
  
  print(paste("Downloading", url))

  download.file(url, path, method = "curl", quiet = TRUE)
}

# In the given element find one entry for the given path and return it's text, or return NA
FindOneOrNA <- function(root, xpath) {
  el <- root %>%
    xml_find_first(xpath)
  
  if (is.na(el)) {
    return(NA)
  }
  
  text <- el %>%
    xml_text()
  
  return(text)
}

# Parse a Form 4 filing
ParseForm4NonDerivativeSecurities <- function(path) {
  print(paste("Parsing", path))
  
  text <- read_file(path)

  i <- str_locate(text, fixed("<?xml"))[1]
  j <- str_locate(text, fixed("\n</XML>"))[1]
  
  if (is.na(i)) {
    print(paste("Skipping", path, "(no XML)"))
    return()
  }

  text.xml <- str_sub(text, i, j - 1)

  xml <- read_xml(text.xml)

  issuer <- xml %>%
    xml_find_first(".//issuer/issuerName") %>%
    xml_text()
  
  transactions <- xml %>%
    xml_find_all(".//nonDerivativeTransaction")

  title <- sapply(transactions, FindOneOrNA, ".//securityTitle/value")
  transaction.date <- sapply(transactions, FindOneOrNA, ".//transactionDate/value")
  transaction.code <- sapply(transactions, FindOneOrNA, ".//transactionCoding/transactionCode")
  shares <- as.numeric(sapply(transactions, FindOneOrNA, ".//transactionAmounts/transactionShares/value"))
  price.per.share <- as.numeric(sapply(transactions, FindOneOrNA, ".//transactionAmounts/transactionPricePerShare/value"))
  acquisition.code <- sapply(transactions, FindOneOrNA, ".//transactionAmounts/transactionAcquiredDisposedCode/value")
  ownership <- sapply(transactions, FindOneOrNA, ".//ownershipNature/directOrIndirectOwnership/value")
  
  results <- data.frame(
    file = rep(path, length(title)),
    issuer = rep(issuer, length(title)),
    title,
    transaction.date,
    # Transaction code mapping: https://www.sec.gov/opa/column-descriptions.html
    transaction.code,
    shares,
    price.per.share,
    total.sale = shares * price.per.share,
    acquisition.code,
    ownership,
    stringsAsFactors = FALSE
  )
  
  return(results)
}
