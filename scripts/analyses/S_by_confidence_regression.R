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

## specify priors
priors <-c(prior("normal(.5, .1)", class = "Intercept"),
          prior("uniform(-.2, .2)", class = "b", lb = -0.2, ub = 0.2),
          prior("normal(0, 1)", class = "sd"))

model_interaction_beast_prior <-
  brms::brm(
    s ~ confidence_self * confidence_other + (1 | ID),
    prior = priors,
    sample_prior = 'only',
    family = gaussian,
    data = data_beast,
    cores = 4,
    chains = 4,
    iter = 4000, 
    seed = 88
  )

## prior predictive check
pp_check(model_interaction_beast_prior, ndraws = 100)

## Regression model - experiment 1
model_interaction_beast <-
  brms::brm(
    s ~ confidence_self * confidence_other + (1 | ID),
    prior = priors,
    sample_prior = 'yes',
    family = gaussian,
    data = data_beast,
    cores = 4,
    chains = 4,
    iter = 4000, 
    seed = 88
  )


pp_check(model_interaction_beast)

## save modelfit
save(list = c("model_interaction_beast"), 
     file = "model_fits/experiment1/model_interaction_beast.RData")
}

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

beast_regression_analysis_table <- bayestestR::describe_posterior(model_interaction_beast ) %>% 
  dplyr::select( -c("CI","ROPE_CI", "ROPE_Percentage", "ROPE_high", "ROPE_low"))
  ))

beast_regression_analysis_table$Parameter <- c(
  "Intercept",
  "Confidence Self (High)",
  "Confidence Other (High)",
  "Interaction"
)

## Create table and save into word file 
ft <- flextable::flextable(beast_regression_analysis_table)

# Format to resemble APA
ft <- fontsize(ft, size = 7, part = "all")
ft <- align(ft, align = "center", part = "all")

# Add table to Word document
doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, ft)

print(doc, target = "tables/beast_regression_analysis_table.docx")

## transform the data into matrix
posterior_beast <- as.matrix(model_interaction_beast)

## make data frame 
posterior_data_beast <- data.frame(self = posterior_beast[,'b_confidence_selfHigh'],
                                   other = posterior_beast[,'b_confidence_otherHigh'],
                                   interaction = posterior_beast[,'b_confidence_selfHigh:confidence_otherHigh'],
                                   experiment = factor(rep('Experiment 1', 8000)), labels = '1')

# transform it into long version for ggplot
posterior_data_beast_long <- gather(posterior_data_beast, key = 'confidence', value = 'estimate', 1:3)

## hypothesis testing
hypMatbeast<-matrix(0, nrow=0, ncol=3)
hypMatbeast<-rbind(hypMatbeast,hypothesis(model_interaction_beast, "confidence_selfHigh < 0 ")$hypothesis[1:8])
hypMatbeast<-rbind(hypMatbeast,hypothesis(model_interaction_beast, "confidence_otherHigh > 0 ")$hypothesis[1:8])
hypMatbeast<-rbind(hypMatbeast,hypothesis(model_interaction_beast, "confidence_selfHigh:confidence_otherHigh < 0 ")$hypothesis[1:8])

hypMatbeast_clean <- hypMatbeast %>% 
dplyr::mutate(
  dplyr::across(
    where(is.numeric),
    ~ ifelse(is.infinite(.x), ">100", sprintf("%.2f", round(.x, 2)))
  )
)

write.csv(hypMatbeast_clean, 'tables/hypotheses_beast.csv')

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

## sample priors
model_interaction_election_prior <-
  brms::brm(
    s ~ confidence_self * confidence_other + expertise + favorability_fact + same_majority_fact + population + (1|ID),
    prior = priors,
    sample_prior = 'only',
    family = gaussian,
    data = data_elections,
    cores = 4,
    chains = 4,
    iter = 4000, 
    seed = 88
  )

##prior predictive check
pp_check(model_interaction_election_prior)

## Regression model - experiment 2
model_interaction_elections <-
  brms::brm(
    s ~ confidence_self * confidence_other + expertise + favorability_fact + same_majority_fact + population + (1|ID),
    family = gaussian,
    data = data_elections,
    prior = priors,
    sample_prior = "yes",
    cores = 4,
    chains = 4,
    iter = 4000,
    seed = 88
  )

conditional_effects(model_interaction_elections)

## save modelfit
save(list = c("model_interaction_elections"), 
     file = "model_fits/experiment2/model_interaction_elections.RData")

}

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


elections_regression_analysis_table <- bayestestR::describe_posterior(model_interaction_elections)  %>% 
  dplyr::select( -c("CI","ROPE_CI", "ROPE_Percentage", "ROPE_high", "ROPE_low")) %>% 
  dplyr::mutate(across(
    .cols = setdiff(names(elections_regression_analysis_table)[sapply(elections_regression_analysis_table, is.numeric)], "Rhat"),
    .fns = ~ round(.x, 2)
  ))

elections_regression_analysis_table$Parameter <- c(
    "Intercept",
    "Confidence Self (High)",
    "Confidence Other (High)",
    "Self-rated expertise",
    "Desirable outcome: yes",
    "Same predicted majority: yes",
    "State population size", 
    "Confidence Self X Confidence Other"
)

## Create table and save into word file 
ft <- flextable::flextable(elections_regression_analysis_table)

# Format to resemble APA
ft <- fontsize(ft, size = 7, part = "all")
ft <- align(ft, align = "center", part = "all")

# Add table to Word document
doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, ft)

print(doc, target = "tables/elections_regression_analysis.docx")

hypMatelections<-matrix(0, nrow=0, ncol=7)
hypMatelections<-rbind(hypMatelections,hypothesis(model_interaction_elections, "confidence_selfHigh < 0 ")$hypothesis[1:8])
hypMatelections<-rbind(hypMatelections,hypothesis(model_interaction_elections, "confidence_otherHigh > 0 ")$hypothesis[1:8])
hypMatelections<-rbind(hypMatelections,hypothesis(model_interaction_elections, "confidence_selfHigh:confidence_otherHigh < 0 ")$hypothesis[1:8])
hypMatelections<-rbind(hypMatelections,hypothesis(model_interaction_elections, "expertise < 0 ")$hypothesis[1:8])
hypMatelections<-rbind(hypMatelections,hypothesis(model_interaction_elections, "favorability_factsame > 0 ")$hypothesis[1:8])
hypMatelections<-rbind(hypMatelections,hypothesis(model_interaction_elections, "same_majority_factsame > 0 ")$hypothesis[1:8])
hypMatelections<-rbind(hypMatelections,hypothesis(model_interaction_elections, "population < 0 ")$hypothesis[1:8])

hypMatelections_clean <- hypMatelections %>% 
  dplyr::mutate(
    dplyr::across(
      where(is.numeric),
      ~ ifelse(is.infinite(.x), ">100", sprintf("%.2f", round(.x, 2)))
    )
  )

write.csv(hypMatelections, 'tables/hypotheses_elections.csv')

## convert output from model into matrix
posterior_elections <- as.matrix(model_interaction_elections)

## make it into a dataframe
posterior_data_elections <- data.frame(self = posterior_elections[,'b_confidence_selfHigh'],
                                       other = posterior_elections[,'b_confidence_otherHigh'],
                                       interaction = posterior_elections[,'b_confidence_selfHigh:confidence_otherHigh'],
                                       experiment = factor(rep('Experiment 2', 8000)), labels = '2')

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
         x = density(estimate, n = 8000)$x,
         y = density(estimate, n = 8000)$y,
         ymax = max(y))

## save posterior data combined
save(posterior_data_combined, file = 'model_fits/posterior_distributions_2025.rda')




