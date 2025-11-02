###-------------------------------------------------------------------------
###-------------------------------------------------------------------------
###                                                                      ---
###                               FIGURE 2                               ---
###                                                                      ---
###-------------------------------------------------------------------------
###-------------------------------------------------------------------------


## load packages
source("scripts/check_pkgs.R")

## model fit data
load('model_fits/posterior_distributions_2025.rda')

## load raw data
load('data/beast/clean_data/meanS_data_beast.rda')
load('data/elections/clean_data/meanS_data_elections.rda')

## load regression predictions
load('model_fits/experiment1/interaction_effect_beast.rda')
load('model_fits/experiment2/interaction_effect_elections.rda')

# load theme for plots
source('scripts/plots/theme_plot.R')

##### Figure 1 -----------------------------------------------------------------

## plot posterior distribution of estimated coefficients
plot_br <-
  ggplot(data = posterior_data_combined, aes(x = estimate, y = confidence_f)) +
  stat_slab(
    aes(
      fill = experiment,
      fill_ramp = after_stat(level),
      color = experiment
    ),
    .width = c(.95, 1),
    trim = TRUE,
    slab_alpha = .6
  ) +
  stat_pointinterval(
    aes(color = experiment),
    #point_size = 4,
    position = position_dodge(width = .6, preserve = "single")
  ) +
  scale_fill_manual(values = c("#FFC20A", "#0C7BDC")) +
  scale_color_manual(values = c("#FFC20A", "#0C7BDC")) +
  geom_vline(
    xintercept =  0,
    lwd = .5,
    lty = 2,
    color = 'black'
  ) +
  geom_vline(
    xintercept =  c(-.1, .1),
    lwd = .5,
    lty = 2,
    color = 'darkgrey'
  ) +
  scale_y_discrete(limits = rev) +
  xlim(c(-0.16, 0.18)) +
  theme(
    strip.text.y.left = element_text(angle = 0),
    strip.background = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position.inside = c(.8, .85),
  ) +
  labs(
    x = 'Estimate',
    y = 'Regression coefficient',
    fill = NULL,
    color = NULL,
    tag = "c"
  ) +
  guides(fill_ramp = 'none')


plot_br

# PANEL B & c

## load posterior prediction data to add to plot
df_interaction_effect_beast <- df_interaction_effect_beast %>%
  mutate(interaction_f = ordered(c("LL", "LH", "HL", "HH"))) # add treatment factor to match plot axis

df_interaction_effect_elections <-
  df_interaction_effect_elections %>%
  mutate(interaction_f = ordered(c("LL", "LH", "HL", "HH"))) # add treatment factor to match plot axis

## Experiment 1 boxplots
boxplot_beast <- beast_data_2by2 %>%
  ggplot(aes(
    x = (interaction_f),
    y = S,
    fill = interaction_f
  )) +
  geom_half_boxplot(
    nudge = 0,
    errorbar.draw = FALSE,
    outlier.shape = NA,
    width = 0.8,
    size = .8
  ) +
  geom_half_point(
    alpha = .35,
    width = 0.8,
    size = 2,
    aes(color = interaction_f)
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 21,
    size = 3,
    stroke = 1.5,
    color = "black",
    fill = "white"
  ) +
  geom_hline(yintercept = .5,
             lty = 2,
             lwd = 1) +  #line of s = .5+
  geom_pointrange(
    data = df_interaction_effect_beast,
    aes(
      x = interaction_f,
      y = estimate__,
      ymin = lower__,
      ymax = upper__,
      fill = interaction_f
    ),
   position = position_nudge(x = .3),
    #geom = "point",
    shape = 24,
    size = 1.2,
    #stroke = 2,
    color = "red",
    fill = 'red',
    linewidth = 1.5
  ) +
  # scale_x_discrete(labels=c(expression(paste(S[Low] , O[Low])),
  #                           (expression(paste(S[Low] , O[High]))),
  #                           (expression(paste(S[High] , O[Low]))),
  #                           (expression(paste(S[High] , O[High])))
  #                           ))+
  scale_x_discrete(labels = c("Low:Low",
    "Low:High",
    "High:Low",
    "High:High")) +
  labs(
    title = 'Experiment 1',
    x = 'Confidence (Self:Other)',
    y =  bquote("Mean adjustment " ~ "(" * bar(S) * ")"),
    
    tag = "a"
  ) +
  
  scale_fill_manual(values = c("#bd5249",
    "#b387c0",
    "#2c90bd",
    "#d8a15a")) +
  scale_color_manual(values = c("#bd5249",
    "#b387c0",
    "#2c90bd",
    "#d8a15a")) +
  theme(legend.position = "none") +
   coord_flip()

boxplot_beast


####### Experiment 2 boxplots -----

boxplot_elections <- elections_data_2by2 %>%
  ggplot(aes(
    x = (interaction_f),
    y = S,
    fill = interaction_f
  )) +
  geom_half_boxplot(
    nudge = 0,
    errorbar.draw = FALSE,
    outlier.shape = NA,
    width = 0.8,
    size = .8
  ) +
  geom_half_point(
    size = 2,
    alpha = .35,
    width = 0.8,
    aes(color = interaction_f)
  ) +
  geom_pointrange(
    data = df_interaction_effect_elections,
    aes(
      x = interaction_f,
      y = estimate__,
      ymin = lower__,
      ymax = upper__,
      fill = interaction_f
    ),
   # position = position_nudge(x = .3),
    #geom = "point",
    shape = 24,
    size = .8,
    #stroke = 2,
    color = "red",
    fill = 'white',
    linewidth = 1.5
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 21,
    size = 2.5,
    stroke = 1.5,
    color = "black",
    fill = "white"
  ) +
  labs(
    title = 'Experiment 2',
    x = '',
    y = bquote("Mean adjustment " ~ "(" * bar(S) * ")"),
    tag = "b"
  ) +
  scale_fill_manual(values = c("#bd5249",
    "#b387c0",
    "#2c90bd",
    "#d8a15a")) +
  scale_color_manual(values = c("#bd5249",
    "#b387c0",
    "#2c90bd",
    "#d8a15a")) +
  # scale_x_discrete(labels=c(expression(paste(S[Low] , O[Low])),
  #                           (expression(paste(S[Low] , O[High]))),
  #                           (expression(paste(S[High] , O[Low]))),
  #                           (expression(paste(S[High] , O[High])))
  #  ))+
  scale_x_discrete(labels = c("Low:Low",
    "Low:High",
    "High:Low",
    "High:High")) +
  
  theme(legend.position = "none"
   ) +
    geom_hline(yintercept = .5,
    lty = 2,
    lwd = 1) +  #line of s = .5 +
  theme(strip.text.y.left = element_text(angle = 0)) +
  #strip.background = element_blank(),
  # axis.text.y = element_blank(),
  #axis.ticks.y = element_blank())+
  coord_flip()

boxplot_elections

## merge plots together in one
raw_data <-
  cowplot::plot_grid(
    boxplot_beast,
    boxplot_elections,
    align = "H",
    nrow = 1,
    rel_widths = c(1, .97)
  )

raw_data


fig2 <- 
  cowplot::plot_grid(
  raw_data,
  plot_br,
  align = "H",
  nrow = 2,
  rel_heights = c(1, .8)
)

fig2

# ## preview the plots
# 
# nflplotR::ggpreview(
#   plot = fig1,
#   width = 9.6,
#   height = 8,
#   asp = NULL,
#   dpi = 144,
#   device = "png",
#   units = c("in"),
#   scale = 1.1,
#   #limitsize = TRUE,
#   bg = NULL
# )

## save pdf, only for later
ggsave(
  "plots/figure2.png",
  plot = fig2,
 width = 10.2,
 height = 7.8,
  dpi = 144,
 # device = "png",
  units = c("in"),
  scale = 1.2 ,
  #limitsize = TRUE,
  bg = 'white'
)

# plot.new()
# png(filename = "plots/figure2.png", width = 10.2, height = 8.8, units = "in",
#     pointsize = 12, bg = "white", res = 144, family = "",
#     type = "cairo", antialias = "d")   


# alternative
#ggsave("plots/figure2a.png", fig2, width = 10.2,height = 9, dpi = 120)



