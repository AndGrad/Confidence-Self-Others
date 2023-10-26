#### Figure SX - Empirical cumulative distributions-----------------------------

load('data/ecdf_beast.RData')
load('data/ecdf_elections.RData')



ecdf_beast <- ecdf_beast+
  labs(title = 'Experiment 1',
       x = 'Single round adjustment (s)')

ecdf_elections <- ecdf_elections+
  labs(title = 'Experiment 2',
       x = 'Single round adjustment (s)',
       y='') 

fig2 <- ecdf_beast + ecdf_elections & theme(legend.position = "bottom")
fig2 <- fig2+ plot_layout(guides = "collect")
# plot_annotation(tag_levels = 'A') &
# theme(plot.tag = element_text(face = 'bold'))
fig2

ggsave('plots/fig2.png', width = 9, height = 5, dpi = 120)
