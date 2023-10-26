###------------------------------------------------------------------------
###------------------------------------------------------------------------
###                                                                     ---
###                         REGRESSION ANALYSIS                         ---
###                                                                     ---
###------------------------------------------------------------------------
###------------------------------------------------------------------------


## SETUP ------------------------------------------------------------------

## load packages and load data from data preparation script
library(tidyverse)
library(brms)
library(rstan)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(repmod)
library(sjPlot)
library(posterior)

## load full dataset 
load('data/full_dataset.rda')


options(contrasts = rep("contr.treatment", 2))

## load regression results if already run
if (file.exists('model_fits/experiment1/model_interaction_beast.RData') == TRUE & file.exists('model_fits/experiment1/interaction_effect_beast.rda') == TRUE) {
  
load('model_fits/experiment1/model_interaction_beast.RData')
load('model_fits/experiment1/interaction_effect_beast.rda')
  
  
} else {

## RUN THE MODELS ---------------------------------------------------------

## EXPERIMENT 1

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

## Regression model - experiment 1
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




