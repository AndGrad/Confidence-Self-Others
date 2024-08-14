###-------------------------------------------------------------------------
###-------------------------------------------------------------------------
###                                                                      ---
###                               FIGURE 3                               ---
###                                                                      ---
###-------------------------------------------------------------------------
###-------------------------------------------------------------------------

## load packages
source("scripts/check_pkgs.R")

## load raw data
load('data/beast/clean_data/heuristics_data_beast.rda')
load('data/elections/clean_data/heuristics_elections_beast.rda')

## load theme for plots
source('scripts/plots/theme_plot.R')

# make custom palette
palette <- c("#FFFFFF", "#bae4b3", "#74c476", "#238b45", "#000000")


## Experiment 1
heuristic_plot_beast <-
  ggplot(data = heuristics_beast_data, aes(x = interaction_f, y = freq)) +
  geom_col(position = position_stack(reverse = TRUE) ,
           aes(fill = factor(bin)),
           color = 'black')    +
  geom_label(
    aes(label = perc),
    fontface = "bold",
    fill = 'white',
    position = position_stack(vjust = .5),
    size = 8,
  ) +
  labs(title = 'Experiment 1',
       x = "Confidence (Self:Other)",
       y = "Proportion of strategies") +
  scale_fill_manual(
    values = palette,
    name = "Adjustment strategy",
    labels = c('Stay', 'Comp [s<0.5]', "Comp [s=0.5]", "Comp [s>0.5]", "Copy")
  ) +
  scale_x_discrete(labels = c("Low:Low",
                              "Low:High",
                              "High:Low",
                              "High:High")) +
  guides(fill = guide_legend(title.position = "top")) +
  coord_flip()

heuristic_plot_beast

## Experiment 2
heuristic_plot_elections <-
  ggplot(data = heuristics_elections_data, aes(x = interaction_f, y = freq)) +
  geom_col(position = position_stack(reverse = TRUE) ,
           aes(fill = factor(bin)),
           color = 'black')    +
  geom_label(
    aes(label = perc),
    fontface = "bold",
    fill = 'white',
    position = position_stack(vjust = .5),
    size = 8,
  ) +
  labs(title = 'Experiment 2',
       x = "Confidence (Self:Other)",
       y = "Proportion of strategies") +
  scale_fill_manual(
    values = palette,
    name = "Adjustment strategy",
    labels = c('Stay', 'Comp [s<0.5]', "Comp [s=0.5]", "Comp [s>0.5]", "Copy")
  ) +
  scale_x_discrete(labels = c("Low:Low",
                              "Low:High",
                              "High:Low",
                              "High:High")) +
  guides(fill = guide_legend(title.position = "top")) +
  coord_flip()

## merge plots

heuristic_plot_elections <- heuristic_plot_elections +  labs(x = "")

## save into figure
fig3 <-
  heuristic_plot_beast + heuristic_plot_elections &
  theme(
    legend.position = "bottom",
    panel.grid.major.x = NULL,
    legend.margin = margin(5, 5, 5, ),
    legend.spacing.x = unit(4, "pt"),
    legend.spacing.y = unit(4, "pt"),
    legend.box = NULL
    
  )

## merge guides
fig3 <- fig3 + plot_layout(guides = "collect")
# plot_annotation(tag_levels = 'A') &
# theme(plot.tag = element_text(face = 'bold'))
fig3

## test figure
ggsave(
  'plots/figure3.png',
  width = 10,
  height = 5.8,
  dpi = 180
)
