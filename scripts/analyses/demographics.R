
### Report about sample characteristics

# import libraries
library(tidyverse)
library(stargazer)

data <- read.csv("data/full_dataset.csv")

## Sample  -----------------------------------------------------------------

## Experiment 1 n of unique ppts
unique_ppts1 <- data %>% 
  dplyr::filter(experiment == "Experiment 1") %>% 
  select(ID) %>% 
  n_distinct()

## Experiment 2 n of unique ppts
unique_ppts2 <- data %>% 
  dplyr::filter(experiment == "Experiment 2") %>% 
  select(ID) %>% 
  n_distinct()
  
## demographics
demographics <- data %>%
  select(experiment, age, gender, ID) %>% 
  group_by(ID, experiment) %>% 
  distinct() %>% 
  group_by(experiment, gender) %>% 
  summarise(mean_age = mean(age),
            sd_age = sd(age),
            n = n()) %>% 
  mutate(perc_gender = n / sum(n))

## save as html table
stargazer::stargazer(data.frame(demographics), digits = 2, out = "tables/demographics.html", summary = FALSE)

