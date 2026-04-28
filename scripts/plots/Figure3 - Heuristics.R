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
  geom_col(linewidth = 0.6,
           position = position_stack(reverse = TRUE) ,
           aes(fill = factor(bin)),
           color = 'black')    +
  geom_label(
    aes(label = perc),
    fontface = "bold",
    fill = 'white',
    position = position_stack(vjust = .5),
    size = 3.5,
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
  geom_col(linewidth = 0.6,
           position = position_stack(reverse = TRUE) ,
           aes(fill = factor(bin)),
           color = 'black')    +
  geom_label(
    aes(label = perc),
    fontface = "bold",
    fill = 'white',
    position = position_stack(vjust = .5),
    size = 3.5,
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
    legend.margin = margin(5, 5, 5, 5 ),
    legend.spacing.y = unit(4, "pt"),
    legend.box = NULL
    
  )

text_scale <- 1.0  
base_size  <- 14

## merge guides

fig3_scaled <- fig3 &
  theme( 
    text         = element_text(size = base_size * text_scale),
    axis.text    = element_text(size = base_size * text_scale),
    axis.text.y = element_text(size = base_size * text_scale, angle = 40),
    axis.title   = element_text(size = base_size * text_scale),
    plot.title   = element_text(size = base_size * text_scale * 1.2, hjust = 0.5),
    legend.text  = element_text(size = 10 * text_scale,
                                margin = margin(l = 2, unit = "pt")),
    legend.title = element_text(size = 12 * text_scale, face = "bold", hjust = 0.5),
    legend.key.spacing.x = unit(6, "pt"), 
    legend.spacing.y = unit(0, "pt"),   
    strip.text   = element_text(size = base_size * text_scale),
    axis.ticks = element_line(linewidth = 0.6)
    
  ) 


fig3_scaled + plot_layout(guides = "collect")
## test figure
# ggsave(
#   'plots/figure3.png',
#   width = 10,
#   height = 5.8,
#   dpi = 180
# )

ggsave(
  'plots/figure3.pdf',
  width = 8.2,
  scale = 1,
  height = 5,
  dpi = 300
)
  
