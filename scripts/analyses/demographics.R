### Report about sample characteristics

# import libraries
library(tidyverse)
library(stargazer)


## Sample before exclusion  ----------------------------------------------------

data_beast <- read.csv("data/beast/clean_data/beast_data_expanded_no_filters.csv") %>% 
  dplyr::select(experiment, age, gender_factor, ID) %>% 
  rename(gender_factor=gender)
  
data_elections <- read.csv("data/elections/clean_data/elections_data_expanded_no_filters.csv")  %>% 
  dplyr::select(experiment, age, gender, ID)

## Experiment 1 n of unique ppts
unique_ppts1 <- data_beast %>% 
  dplyr::select(ID) %>% 
  n_distinct()

## Experiment 2 n of unique ppts
unique_ppts2 <- data_elections  %>% 
  dplyr::select(ID) %>% 
  n_distinct()

demographics_all <- bind_rows(data_beast, data_elections)
  
## demographics
demographics_all <- demographics_all %>%
  dplyr::select(experiment, age, gender, ID) %>% 
  group_by(ID, experiment) %>% 
  distinct() %>% 
  group_by(experiment, gender) %>% 
  summarise(mean_age = mean(age),
            sd_age = sd(age),
            n = n()) %>% 
  mutate(perc_gender = n / sum(n))

## save as html table
stargazer::stargazer(data.frame(demographics_all), digits = 2, out = "tables/demographics_all.html", summary = FALSE)


## Sample after exclusion -----------------------------------------------------

data <- read.csv("data/full_dataset.csv")

final_ppts1 <- data %>% 
  dplyr::filter(experiment == "Experiment 1") %>% 
  dplyr::select(ID) %>% 
  n_distinct()

final_ppts2 <- data %>% 
  dplyr::filter(experiment == "Experiment 2") %>% 
  dplyr::select(ID) %>% 
  n_distinct()


## demographics
demographics <- data %>%
  dplyr::select(experiment, age, gender, ID) %>% 
  group_by(ID, experiment) %>% 
  distinct() %>% 
  group_by(experiment, gender) %>% 
  summarise(mean_age = mean(age),
            sd_age = sd(age),
            n = n()) %>% 
  mutate(perc_gender = n / sum(n))

## save as html table
stargazer::stargazer(data.frame(demographics), digits = 2, out = "tables/demographics_final.html", summary = FALSE)

