### Here we load data after wrangling, and check that everything is ok!
# @andrea do this also fo elections


#### Sanity checks: n of trials, n of participants, per each treatment

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

