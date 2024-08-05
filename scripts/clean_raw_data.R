##### Function to import data from (multiple) LIONESS scripts ####

# Important: this works only all the raw files are stored in the same folder,
# and specific folder only contains LIONESS files
library(tidyverse)
library(tibble)
library(readxl)

# function takes the folder path and the number of periods expected from complete participants as input

readLIONESS <- function(folder, writingLocation, periods, lastPage) {
  merged_data <- data.frame()
  
  # how many raw files are in the folder?
  num_files <- length(list.files(folder))
  
  # loop through them and merge them into 1 file
  for (n in 1:num_files) {
    
    filename <- paste(folder ,list.files(folder)[n], sep = '')
    
    # read the core sheet
    core <-  read_excel(path = filename,
                        sheet = "core") %>%
      select(-c("ipaddress", "ipaddress_part", "location" )) %>% 
      write_csv(paste0(writingLocation, n, "-core-raw.csv"))
    
    decisions <-  read_excel(path = filename,
                             sheet = "decisions") %>%
      write_csv(paste0(writingLocation, n, "-decisions-raw.csv"))
    
    session <-  read_excel(path = filename,
                           sheet = "session") %>%
      write_csv(paste0(writingLocation, n, "-session-raw.csv"))
    
    # which players completed the experiment
    player_finished <- core %>%
      dplyr::filter(period == periods) %>%
      #filter(onPage == lastPage & period == periods) %>%
      dplyr::select(playerNr)
    
    # exclude incomplete participants
    decisions <- decisions %>%
      dplyr::filter(playerNr %in% player_finished$playerNr)
    
    # append to other batches (if existing)
    merged_data <-
      dplyr::bind_rows(merged_data, decisions)
  }
  
  return(merged_data)
}