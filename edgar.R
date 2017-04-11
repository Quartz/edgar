library(dplyr)
library(readr)
library(stringr)
library(xml2)

FIRST.YEAR = 2016
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
      untar(file.path)
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
downloadFiling <- function(path) {
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
