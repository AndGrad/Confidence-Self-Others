## load packages and load data from data preparation script
source("scripts/check_pkgs.R")

### Vizualization of correct E1 guesses ------------------------------------------

## How often is e1 == to social info?

beast_all_trials <- read_csv("data/beast/clean_data/beast_data_expanded_no_filters.csv")

## load the data
data_excluded_b <- beast_all_trials %>% 
  mutate(count = n()) %>% 
  dplyr::filter(s > 1 | s < 0) %>% 
  dplyr::select(s, count) %>% 
  mutate(exp = "exp1")

data_excluded_e <- read_csv("data/elections/clean_data/elections_data_expanded_no_filters.csv") %>% 
  mutate(count = n()) %>% 
  dplyr::filter(s > 1 | s < 0) %>% 
  dplyr::select(s, count) %>% 
  mutate(exp = "exp2")

data_excluded <- bind_rows(data_excluded_b, data_excluded_e)

total_observations <- data_excluded_b$count[1] + data_excluded_e$count[1]

## label for annotation
annotation_label <- paste0("N = ", nrow(data_excluded),
                           " (",round(nrow(data_excluded)/(total_observations)*100,2), "% of total observations)")

## make plot
data_excluded %>% 
  ggplot(data = .) +
  geom_histogram(
    aes(x = s),
    fill = "#FFDAB9", 
    color = "black", 
    binwidth = 0.1
  ) +
  annotate(
    "rect",
    xmin = -Inf,
    xmax = 0,
    ymin = -Inf,
    ymax = Inf,
    fill = "lightgrey",
    alpha = 0.6 
  ) +
  annotate(
    "rect",
    xmin = 1,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    fill = "lightgrey",
    alpha = 0.6
  ) +
  annotate(
    "text",
    x = 15,
    y = 90, 
    label = annotation_label,
    hjust = 0, 
    vjust = 1,
    size = 4  
  ) +
  geom_vline(
    xintercept = c(0, 1),
    color = "black",
    size = 0.8 
  ) +
  theme_minimal() +
  labs(
    x = "s",
    y = "Frequency (Count)"
  ) +
  theme_tidybayes(base_size = 20)
