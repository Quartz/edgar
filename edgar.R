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

# Simple wrapper that applies FindOneOrNA to all elements
FindAll <- function(transactions, xpath) {
  return(sapply(transactions, FindOneOrNA, xpath))
}

# Parse Form 4 XML for non-derivative transactions
ParseForm4NonDerivativeTransactions <- function(xml) {
  transactions <- xml %>%
    xml_find_all(".//nonDerivativeTransaction")
  
  title <- FindAll(transactions, ".//securityTitle/value")
  transaction.date <- FindAll(transactions, ".//transactionDate/value")
  transaction.code <- FindAll(transactions, ".//transactionCoding/transactionCode")
  shares <- as.numeric(FindAll(transactions, ".//transactionAmounts/transactionShares/value"))
  price.per.share <- as.numeric(FindAll(transactions, ".//transactionAmounts/transactionPricePerShare/value"))
  acquisition.code <- FindAll(transactions, ".//transactionAmounts/transactionAcquiredDisposedCode/value")
  ownership <- FindAll(transactions, ".//ownershipNature/directOrIndirectOwnership/value")
  
  results <- data.frame(
    title,
    is.derivative = rep(FALSE, length(title)),
    transaction.date,
    # Transaction code mapping: https://www.sec.gov/opa/column-descriptions.html
    transaction.code,
    shares,
    price.per.share,
    total.sale = shares * price.per.share,
    acquisition.code,
    ownership,
    underlying.securities = rep(NA, length(title)),
    underlying.shares = rep(NA, length(title)),
    exercise.price = rep(NA, length(title)),
    stringsAsFactors = FALSE
  )
  
  return(results)
}

# Parse Form 4 XML for derivative transactions
ParseForm4DerivativeTransactions <- function(xml) {
  transactions <- xml %>%
    xml_find_all(".//derivativeTransaction")
  
  title <- FindAll(transactions, ".//securityTitle/value")
  transaction.date <- FindAll(transactions, ".//transactionDate/value")
  transaction.code <- FindAll(transactions, ".//transactionCoding/transactionCode")
  shares <- as.numeric(FindAll(transactions, ".//transactionAmounts/transactionShares/value"))
  price.per.share <- as.numeric(FindAll(transactions, ".//transactionAmounts/transactionPricePerShare/value"))
  acquisition.code <- FindAll(transactions, ".//transactionAmounts/transactionAcquiredDisposedCode/value")
  ownership <- FindAll(transactions, ".//ownershipNature/directOrIndirectOwnership/value")
  underlying.securities <- FindAll(transactions, ".//underlyingSecurity/underlyingSecurityTitle/value")
  underlying.shares <- FindAll(transactions, ".//underlyingSecurity/underlyingSecurityShares/value")
  exercise.price <- FindAll(transactions, ".//conversionOrExercisePrice/value")
  
  results <- data.frame(
    title,
    is.derivative = rep(TRUE, length(title)),
    transaction.date,
    # Transaction code mapping: https://www.sec.gov/opa/column-descriptions.html
    transaction.code,
    shares,
    price.per.share,
    total.sale = shares * price.per.share,
    acquisition.code,
    ownership,
    underlying.securities,
    underlying.shares,
    exercise.price,
    stringsAsFactors = FALSE
  )
  
  return(results)
}

# Parse a Form 4 filing for both derivative and non-derivative transactions
ParseForm4 <- function(path) {
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
  
  issuer.name <- FindOneOrNA(xml, ".//issuer/issuerName")
  issuer.cik <- FindOneOrNA(xml, ".//issuer/issuerCik")
  owner.name <- FindOneOrNA(xml, ".//reportingOwner/reportingOwnerId/rptOwnerName")
  owner.cik <- FindOneOrNA(xml, ".//reportingOwner/reportingOwnerId/rptOwnerCik")
  
  non.derivatives <- ParseForm4NonDerivativeTransactions(xml)
  derivatives <- ParseForm4DerivativeTransactions(xml)
  transactions <- rbind(non.derivatives, derivatives)
  
  identifiers <- data.frame(
    file = rep(path, nrow(transactions)),
    owner.name = rep(owner.name, nrow(transactions)),
    owner.cik = rep(owner.cik, nrow(transactions)),
    issuer.name = rep(issuer.name, nrow(transactions)),
    issuer.cik = rep(issuer.cik, nrow(transactions)),
    stringsAsFactors = FALSE
  )
  
  results <- cbind(identifiers, transactions) %>%
    arrange(transaction.date)

  return(results)
}
