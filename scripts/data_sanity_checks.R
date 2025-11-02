### Here we load data after wrangling, and check that everything is ok!

## load packages
source("scripts/check_pkgs.R")

## load raw data
load('data/beast/clean_data/meanS_data_beast.rda')
load('data/elections/clean_data/meanS_data_elections.rda')

### Sanity checks: n of trials, n of participants, per each treatment

# n of players in each condition
ggplot(data = beast_data_2by2) +
  geom_bar(aes(x = interaction_f)) +
  labs(title = "players in each treatment bin",
       subtitle = paste("unique players = ", length(unique(beast_data_2by2$ID))),
       x = "treatment") +
  theme_base(20)

# how many ppts are in all bins?
beast_data_2by2 %>% 
  summarise(Count = n()) %>% 
  ggplot +
  geom_bar(aes(x=Count)) +
  labs(title = "players by how many treatments they were in",
       subtitle = paste("unique players = ", length(unique(beast_data_2by2$ID))),
       x = "n treatments") +
  theme_base(20)


### Sanity checks: n of trials, n of participants, per each treatment

# n of players in each condition
ggplot(data = elections_data_2by2) +
  geom_bar(aes(x = interaction_f)) +
  labs(title = "players in each treatment bin",
       subtitle = paste("unique players = ", length(unique(elections_data_2by2$ID))),
       x = "treatment") +
  theme_base(20)


# how many ppts are in all bins?
elections_data_2by2 %>% 
  summarise(Count = n()) %>% 
  ggplot +
  geom_bar(aes(x=Count)) +
  labs(title = "players by how many treatments they were in",
       subtitle = paste("unique players = ", length(unique(elections_data_2by2$ID))),
       x = "n treatments") +
  theme_base(20)


### -------- How many trials are filtered due to 0>s>1?

source("scripts/load_experimental_data.R")

beast_filterS<- beast_data %>%
  dplyr::filter(!is.infinite(s)) %>% 
  dplyr::filter(s <= 1 & s >= 0) %>% 
  nrow()

excluded <- nrow(beast_data) - beast_filterS
excluded/nrow(beast_data) 


election_filterS<- elections_data %>%
  dplyr::filter(!is.infinite(s)) %>% 
  dplyr::filter(s <= 1 & s >= 0) %>% 
  nrow()

excluded_elections<- nrow(elections_data) - election_filterS
excluded_elections/nrow(elections_data) 

