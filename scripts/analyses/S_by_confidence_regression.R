###------------------------------------------------------------------------
###------------------------------------------------------------------------
###                                                                     ---
###                     LINEAR REGRESSION ANALYSIS                      ---
###                                                                     ---
###------------------------------------------------------------------------
###------------------------------------------------------------------------

## regression of S by confidence condition

## SETUP ------------------------------------------------------------------

## load packages and load data from data preparation script
source("scripts/check_pkgs.R")

## load full dataset 
load('data/full_dataset.rda')
options(contrasts = rep("contr.treatment", 2))

## EXPERIMENT 1

## load regression results if already run
if (file.exists('model_fits/experiment1/model_interaction_beast.RData') == TRUE & file.exists('model_fits/experiment1/interaction_effect_beast.rda') == TRUE) {
  
load('model_fits/experiment1/model_interaction_beast.RData')
load('model_fits/experiment1/interaction_effect_beast.rda')

} else {

## RUN THE MODELS ---------------------------------------------------------

## select data
data_beast <- full_dataset %>% 
  dplyr::filter(experiment == "Experiment 1")

## Regression model - experiment 1
model_interaction_beast <-
  brms::brm(
    s ~ confidence_self * confidence_other + (1 |
                                                ID),
    family = gaussian,
    data = data_beast,
    cores = 3,
    chains = 4,
    iter = 3000
  )
####### attempt######


data_beast %>% 
  count(s == 0) %>% 
  mutate(prop = n / sum(n))

prior <- brms::prior_string("normal(0, 0.1)", class = "b")


### zoib option 1
zoib_formula_int <- bf(
  s ~ confidence_self * confidence_other  + (1 |ID),  # Formula for mu
  phi ~ confidence_self * confidence_other+ (1 |ID),  # precision of 0-1 values
  zoi ~ confidence_self * confidence_other + (1 |ID),  # Formula for zero-one inflation
  coi ~ confidence_self * confidence_other + (1 |ID)  # Formula for phi (constant)
)

## zoib option 2
zoib_formula_f <- bf(
  s ~ interaction_f  + (1 |ID),  # Formula for mu
  phi ~ interaction_f + (1 |ID),  # precision of 0-1 values
  zoi ~ interaction_f + (1 |ID),  # Formula for zero-one inflation
  coi ~ interaction_f + (1 |ID)  # Formula for phi (constant)
)

## add priors?

# get_prior(zoib_formula, data = data_beast, family = zero_one_inflated_beta())
# 
# model_interaction_beast_zoib_prior <-
#   brms::brm(
#     s ~ interaction_f + (1 |ID),
#     family = zero_one_inflated_beta(),
#     sample_prior = "only",
#     data = data_beast,
#     prior = prior,
#     cores = 3,
#     chains = 4,
#     iter = 3000
#   )

## fit model 1

zoib_model1 <- brm(
  formula = zoib_formula_int,
  data = data_beast,
  family = zero_one_inflated_beta(),
 # prior = priors,
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4, # Use multiple cores for faster sampling
  seed = 1234 # for reproducibility
)

## fit model 2

zoib_model2 <- brm(
  formula = zoib_formula_f,
  data = data_beast,
  family = zero_one_inflated_beta(),
  # prior = priors,
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4, # Use multiple cores for faster sampling
  seed = 1234 # for reproducibility
)

## evaluate model

as_draws_df(zoib_model, pars = "b_")[, 1:4] %>%
  mutate_at(c("b_phi_Intercept"), exp) %>%
  mutate_at(vars(-"b_phi_Intercept"), plogis) %>%
  posterior_summary() %>%
  as.data.frame() %>%
  rownames_to_column("Parameter") %>%
  kable(digits = 2)

conditional_effects(zoib_model)

conditions_to_predict <- distinct(data_beast, interaction_f, ID)

# Get the posterior distribution of the predicted mean for each condition
zoib_model%>%
  epred_draws(newdata = conditions_to_predict, dpar = "mu") %>%
  
  # Summarise the distribution to get a mean and 95% credible interval
  summarise(
    predicted_mean = mean(.epred),
    lower_ci = quantile(.epred, 0.025),
    upper_ci = quantile(.epred, 0.975)
  )

### test hypotheses

## model 1 

## with interaction
h1 <- c("LH - HL" = "plogis(Intercept + confidence_otherHigh) > plogis(Intercept) + confidence_selfHigh")
hypothesis(zoib_model, h1)

h1 <- c("LH - HL" = "plogis(Intercept + interaction_fLH) > plogis(Intercept)")
hypothesis(zoib_model2, h)

## model 2
h2 <- c("LH - HL" = "plogis(Intercept + interaction_fHL) < plogis(Intercept)")
hypothesis(zoib_model, h2)

## contrasts
aa <- zoib_model1 %>%
  avg_comparisons(variables = c("confidence_self", "confidence_other"))%>% 
  marginaleffects::posterior_draws()

bb <- zoib_model2 %>%
  avg_comparisons(variables = c("interaction_f"))%>% 
  marginaleffects::posterior_draws()

bb %>% 
filter(contrast == "HL - LL")%>%
  median_hdi(draw)
bb %>% 
  filter(contrast == "LH - LL")%>%
  median_hdi(draw)

ggplot(aa, aes(x = draw, fill = contrast)) +
  stat_halfeye(.width = c(0.8, 0.95), point_interval = "median_hdi") +
  theme_clean() +
  facet_wrap(~term)

pp_check(zoib_model)
########################

model_interaction_beast <- model_interaction_beast_zoib

## save modelfit
save(list = c("model_interaction_beast"), 
     file = "model_fits/experiment1/model_interaction_beast.RData")

## check that mcmc are mixing
mcmc_trace(model_interaction_beast, pars = c('b_Intercept',
                                             'b_confidence_selfHigh','b_confidence_otherHigh', 'b_confidence_selfHigh:confidence_otherHigh'))

mcmc_rank_overlay(model_interaction_beast, pars = c('b_Intercept',
                                                    'b_confidence_selfHigh','b_confidence_otherHigh', 'b_confidence_selfHigh:confidence_otherHigh'))

## summary of results
summary(model_interaction_beast)

## plot conditionall effects 
conditional_effects(model_interaction_beast)

## extract cofficients
c_eff_interaction <- conditional_effects(model_interaction_beast)

## save treatment effects into a dataframe
df_interaction_effect_beast <- as.data.frame(c_eff_interaction$`confidence_self:confidence_other`)

## save into a r object for plotting later
save(df_interaction_effect_beast, file = "model_fits/experiment1/interaction_effect_beast.rda")

## save html table of regression results
tab_model(
  model_interaction_beast,
  show.se = TRUE,
  pred.labels = c(
    "Intercept",
    "Confidence Self (High)",
    "Confidence Other (High)",
    "Interaction"
  ),
  file = "tables/beast_regression_analysis.html"
)

## transform the data into matrix
posterior_beast <- as.matrix(model_interaction_beast)

## make data frame 
posterior_data_beast <- data.frame(self = posterior_beast[,'b_confidence_selfHigh'],
                                   other = posterior_beast[,'b_confidence_otherHigh'],
                                   interaction = posterior_beast[,'b_confidence_selfHigh:confidence_otherHigh'],
                                   experiment = factor(rep('Experiment 1', 6000)), labels = '1')

# transform it into long version for ggplot
posterior_data_beast_long <- gather(posterior_data_beast, key = 'confidence', value = 'estimate', 1:3)

}

## ------------------------------------------------------------------------------------------------

## EXPERIMENT 2

## load regression results if already run
if (file.exists('model_fits/experiment2/model_interaction_elections.RData') == TRUE & file.exists('model_fits/experiment2/interaction_effect_elections.rda') == TRUE) {
  
  load('model_fits/experiment2/model_interaction_elections.RData')
  load('model_fits/experiment2/interaction_effect_elections.rda')
  
  
} else {

## select data
data_elections <- full_dataset %>% 
  dplyr::filter(experiment == "Experiment 2")

## Regression model - experiment 2
model_interaction_elections <-
  brms::brm(
    s ~ confidence_self * confidence_other + expertise + favorability_fact + same_majority_fact + population + (1|ID),
    family = gaussian,
    data = data_elections,
    cores = 3,
    chains = 4,
    iter = 3000
  )

## save modelfit
save(list = c("model_interaction_elections"), 
     file = "model_fits/experiment2/model_interaction_elections.RData")

## check that mcmc are mixing
mcmc_trace(model_interaction_elections, pars = c('b_Intercept',
                                             'b_confidence_selfHigh','b_confidence_otherHigh', 'b_confidence_selfHigh:confidence_otherHigh'))

mcmc_rank_overlay(model_interaction_elections, pars = c('b_Intercept',
                                                    'b_confidence_selfHigh','b_confidence_otherHigh', 'b_confidence_selfHigh:confidence_otherHigh'))

## summary of results
summary(model_interaction_elections)

## plot conditionall effects 
conditional_effects(model_interaction_elections)

## extract cofficients
c_eff_interaction_elections <- conditional_effects(model_interaction_elections)

## save treatment effects into a dataframe
df_interaction_effect_elections <- as.data.frame(c_eff_interaction_elections$`confidence_self:confidence_other`)

## save into a r object for plotting later
save(df_interaction_effect_elections, file = "model_fits/experiment2/interaction_effect_elections.rda")

## save html table of regression results
tab_model(
  model_interaction_elections,
  show.se = TRUE,
  pred.labels = c(
    "Intercept",
    "Confidence Self (High)",
    "Confidence Other (High)",
    "Self-rated expertise",
    "Desirable outcome: yes",
    "Same predicted majority: yes",
    "State population size", 
    "Confidence Self X Confidence Other"
  ),
  file = "tables/elections_regression_analysis.html"
)

## convert output from model into matrix
posterior_elections <- as.matrix(model_interaction_elections)

## make it into a dataframe
posterior_data_elections <- data.frame(self = posterior_elections[,'b_confidence_selfHigh'],
                                       other = posterior_elections[,'b_confidence_otherHigh'],
                                       interaction = posterior_elections[,'b_confidence_selfHigh:confidence_otherHigh'],
                                       experiment = factor(rep('Experiment 2', 6000)), labels = '2')


# transform it into long version for ggplot
posterior_data_elections_long <- gather(posterior_data_elections, key = 'confidence', value = 'estimate', 1:3) 


## COMBINE DATASETS
posterior_data_combined <- rbind(posterior_data_beast_long, posterior_data_elections_long) %>%
  mutate(confidence_f = as.factor(confidence),
         confidence_f = ordered(confidence_f, levels = c("self", "other", "interaction"), 
                                labels = c("Confidence self", "Confidence other", "Interaction")))

# add confidence intervals
posterior_data_combined <- posterior_data_combined %>% 
  group_by(confidence_f, experiment) %>% 
  mutate(q5 = quantile(estimate, .05),
         q95 = quantile(estimate, .95),
         median = median(estimate),
         mean = mean(estimate),
         x = density(estimate, n = 6000)$x,
         y = density(estimate, n = 6000)$y,
         ymax = max(y))

## save posterior data combined
save(posterior_data_combined, file = 'model_fits/posterior_distributions.rda')

}




