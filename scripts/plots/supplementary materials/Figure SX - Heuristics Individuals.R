# Figure 2 ---------------------------------------------------------------------
source('scripts/prepare_environment.R')

### heuristics individual

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

figSX <- figSX + plot_layout(guides = "collect")
# plot_annotation(tag_levels = 'A') &
# theme(plot.tag = element_text(face = 'bold'))
figSX 

save_plot('plots/figSX.png', figSX, base_width = 16, base_height = 18)
