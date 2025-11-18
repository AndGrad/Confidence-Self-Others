## load packages and load data from data preparation script
source("scripts/check_pkgs.R")

### Vizualization of correct E1 guesses ------------------------------------------

## How often is e1 == to social info?

beast_all_trials <- read_csv("data/beast/clean_data/beast_data_expanded_no_filters.csv")

correct_guesses_plot <- beast_all_trials %>% 
  ggplot(aes(x = norm1, fill = factor))+
  geom_histogram(binwidth = .02,   aes(fill = (..xmin.. <= 1 & ..xmax.. > 1)),
                 color = "black",
                 show.legend = FALSE
  ) +
  labs( x = "E1 / Correct Value") +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  theme_tidybayes(base_size = 20)

ggsave(correct_guesses_plot, filename = "plots/supplementary_figures/correct_guesses_plot.png")

n_perfect_guesses <- beast_all_trials  %>% 
  filter(e1 == nAnimals) %>% 
  count() %>% 
  pull() 

round(n_perfect_guesses/nrow(data_beast) * 100, 2)