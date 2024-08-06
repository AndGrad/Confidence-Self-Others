## load packages
source("scripts/check_pkgs.R")

# load theme for plots
source('scripts/plots/theme_plot.R')

### ---------------- BEAST DATA
beast_data <- read.csv("data/beast/clean_data/beast_data_filtered.csv")

## palette for plot
palette <- c("#FFFFFF", "#bae4b3", "#74c476", "#238b45", "#000000")

# create data for plot
beast_data <- beast_data %>%
  mutate(bin = ifelse(s==0, 1, ifelse(s < 0.5, 2, ifelse(s == 0.5, 3, ifelse(s > 0.5 & s < 1, 4, 5))))) %>% 
  group_by(bin,ID) %>%
  summarise(count = n()) %>% 
  ungroup() %>% 
  group_by(ID) %>% 
  mutate(heuristic_total = sum(count)) %>% 
  mutate(freq = count / heuristic_total) %>% 
  dplyr::select(-c(count)) %>% 
  spread(key = bin, value = freq) %>% 
  arrange(desc(`1`), desc(`2`), desc(`3`), desc(`4`), desc(`5`))

heuristics_beast_data_individual <- beast_data
heuristics_beast_data_individual$index <- 0
for(n in 1:nrow(heuristics_beast_data_individual)){
  heuristics_beast_data_individual$index[n] <- n
}

heuristics_beast_data_individual<- heuristics_beast_data_individual %>% 
  gather("bin", "freq", 3:7) 

# MAKE PLOT
heuristic_plot_beast_individual <- ggplot(data = heuristics_beast_data_individual, aes(x = index, y = freq)) +
  geom_col(position = position_stack(reverse = TRUE) ,aes( fill = factor(bin)), color = 'black')    +
  labs(title = 'Experiment 1',
       x = "Participant",
       y = "Proportion") +
  scale_fill_manual(values = palette, name = "Adjustment \nstrategy",
                    labels = c('Stay', 'Compromise [s<0.5]', "Compromise [s=0.5]", "Compromise [s>0.5]", "Copy")) +
  coord_flip()

heuristic_plot_beast_individual

### ---------------- ELECTIONS DATA
## load elections data
elections_data <- read.csv("data/elections/clean_data/elections_data_filtered.csv")

# CREATE DATA
elections_data <- elections_data %>%
  mutate(bin = ifelse(s==0, 1, ifelse(s < 0.5, 2, ifelse(s == 0.5, 3, ifelse(s > 0.5 & s < 1, 4, 5))))) %>% 
  group_by(bin,ID) %>%
  summarise(count = n()) %>% 
  ungroup() %>% 
  group_by(ID) %>% 
  mutate(heuristic_total = sum(count)) %>% 
  mutate(freq = count / heuristic_total) %>% 
  dplyr::select(-c(count)) %>% 
  spread(key = bin, value = freq) %>% 
  arrange(desc(`1`), desc(`2`), desc(`3`), desc(`4`), desc(`5`))

heuristics_elections_data_individual <- elections_data
heuristics_elections_data_individual$index <- 0
for(n in 1:nrow(heuristics_elections_data_individual)){
  heuristics_elections_data_individual$index[n] <- n
}

heuristics_elections_data_individual<- heuristics_elections_data_individual %>% 
  gather("bin", "freq", 3:7) 

# MAKE PLOT
heuristic_plot_elections_individual <- ggplot(data = heuristics_elections_data_individual, aes(x = index, y = freq)) +
  geom_col(position = position_stack(reverse = TRUE) ,aes( fill = factor(bin)), color = 'black')    +
  labs(title = 'Experiment 2',
       x = "Participant",
       y = "Proportion") +
  scale_fill_manual(values = palette, name = "Adjustment \nstrategy",
                    labels = c('Stay', 'Compromise [s<0.5]', "Compromise [s=0.5]", "Compromise [s>0.5]", "Copy")) +
  coord_flip()

heuristic_plot_elections_individual

### add theme and merge together

heuristic_plot_beast_individual <- heuristic_plot_beast_individual +
  guides(fill = guide_legend(title = "Strategy", title.position = "top"))+
  theme(panel.grid.major.x = NULL,
        legend.position = "bottom", 
        text = element_text(size = 80),
        plot.title = element_text(size = 80),
        legend.text = element_text(size = 60),
        legend.title = element_text(size = 70, face = 'bold'))

heuristic_plot_elections_individual <- heuristic_plot_elections_individual+
  labs(x="")+
  guides(fill = guide_legend(title = "Strategy", title.position = "top"))+
  theme(
      panel.grid.major.x = NULL,
        legend.position = "bottom", 
        text = element_text(size = 80),
        plot.title = element_text(size = 80),
        legend.text = element_text(size = 60),
      legend.title = element_text(size = 70, face = 'bold'))

figSX <- heuristic_plot_beast_individual + heuristic_plot_elections_individual

figSX <- figSX + plot_layout(guides = "collect") & theme(legend.position = 'bottom' )
# plot_annotation(tag_levels = 'A') &
# theme(plot.tag = element_text(face = 'bold'))
figSX 

cowplot::save_plot('plots/figSX.png', figSX, base_width = 16, base_height = 18)
