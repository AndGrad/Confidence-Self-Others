###------------------------------------------------------------------------
###------------------------------------------------------------------------
###                                                                     ---
###                         SUPPLEMENTARY ANALYSES                      ---
###                                                                     ---
###------------------------------------------------------------------------
###------------------------------------------------------------------------

## SETUP ------------------------------------------------------------------

## load packages and load data from data preparation script
source("scripts/check_pkgs.R")

## EXPERIMENT 1: analysis with the full confidence scale
data_full <- read_csv("data/beast/clean_data/beast_data_expanded_no_filters.csv") %>% 
  dplyr::filter(!is.infinite(s)) %>% 
  dplyr::filter(s <= 1 & s >= 0) %>% 
  mutate(confidence_other_f = ordered(
    confidence_other,
    levels = c('Low', 'Medium', 'High'),
    labels = c('Low', 'Medium', 'High') # create ordered factor for peer confidence treatment
  )) %>%
  mutate(confidence_self_f = ordered(
    confidence_self,
    levels = c('Low', 'Medium', 'High'),
    labels = c('Low', 'Medium', 'High'))) %>% 
  mutate(
    ordered_confidence = factor(
      x = confidence,
      ordered = TRUE
    ),
    ordered_confidence_other = factor(
      x = social_confidence,
      ordered = TRUE
    )
  )

### Model 1: Confidence Medium

## load regression results if already run
if (file.exists(file = "model_fits/experiment1/model_interaction_beast_full_treatments.RData") == TRUE) {

  load('model_fits/experiment1/model_interaction_beast_full_treatments.RData')
  
} else{

## specify priors
priors <-c(prior("normal(.5, .1)", class = "Intercept"),
           prior("uniform(-.2, .2)", class = "b", lb = -0.2, ub = 0.2),
           prior("normal(0, 1)", class = "sd"))

model_interaction_beast_full_treatments <-
  brms::brm(
    s ~ confidence_self_f * confidence_other_f + (1 | ID),
    prior = priors,
    sample_prior = "yes",
    family = gaussian,
    data = data_full,
    cores = 4,
    chains = 4,
    iter = 6000,
    seed = 88
  )

## save modelfit
save(list = c("model_interaction_beast_full_treatments"), 
     file = "model_fits/experiment1/model_interaction_beast_full_treatments.RData")

}

## conditional effects
#conditional_effects(model_interaction_beast_full_treatments, 'confidence_self_f:confidence_other_f')

## save table
## save html table of regression results
tab_model(
  model_interaction_beast_full_treatments,
  show.se = TRUE, 
  pred.labels = c(
    "Intercept",
    "Confidence Self (Medium)",
    "Confidence Self (High)",
    "Confidence Other (Medium)",
    "Confidence Other (High)",
    "Confidence Self (Medium): Confidence Other (Medium)",
    "Confidence Self (High): Confidence Other (Medium)",
    "Confidence Self (Medium): Confidence Other (High)",
    "Confidence Self (High): Confidence Other (High)"
  ),
  file = "tables/beast_regression_analysis_full_treatments.html"
)


## test that s is larger than in low, and that in high is larger than medium

hypothesis(model_interaction_beast_full_treatments, c(
  # Within condition B: add the interaction terms
  "(confidence_self_fMedium + confidence_self_fMedium:confidence_other_fMedium )  < 0",
  "(confidence_self_fHigh + confidence_self_fHigh:confidence_other_fHigh  < confidence_self_fMedium + confidence_self_fMedium:confidence_other_fMedium) "))

### Model 2: Regression with full confidence scale

## scale confidence variable for interpretation of coefficients
data_scaled<- 
  data_full %>% 
  ungroup %>% 
  mutate(confidence_self_scaled= scale(confidence),
         confidence_other_scaled = scale(social_confidence))

## load regression results if already run
if (file.exists('model_fits/experiment1/model_interaction_beast_full_scale.RData') == TRUE) {
  
  load('model_fits/experiment1/model_interaction_beast_full_scale.RData')
} else {

model_interaction_beast_full_scale <-
  brms::brm(
    s ~ confidence_self_scaled * confidence_other_scaled + (1 | ID),
    #prior = prior,
    #sample_prior = "yes",
    family = gaussian,
    data = data_scaled,
    cores = 4,
    chains = 4,
    iter = 4000,
    seed = 88
  )

## save modelfit
save(list = c("model_interaction_beast_full_scale"), 
     file = "model_fits/experiment1/model_interaction_beast_full_scale.RData")

}

## save table

## save html table of regression results
tab_model(
  model_interaction_beast_full_scale,
  show.se = TRUE,
  pred.labels = c(
    "Intercept",
    "Confidence Self (High)",
    "Confidence Other (High)",
    "Interaction"
  ),
  file = "tables/beast_regression_analysis_full_confidence_scale.html"
)

conditional_effects(model_interaction_beast_full_scale)

### Model 3: analysis of uncertainty trials

# load full dataset 
load('data/full_dataset.rda') 

## select only beast trials
data_beast <- full_dataset %>% 
  dplyr::filter(experiment == "Experiment 1")


## load regression results if already run
if (file.exists('model_fits/experiment1/model_interaction_beast_uncertainty.RData') == TRUE) {
  
  load('model_fits/experiment1/model_interaction_beast_uncertainty.RData')
  
} else {

## fit pre-registered model (equation 3 in main text)
model_interaction_beast_uncertainty <-
  brms::brm(
    s ~  uncertainty * confidence_self + confidence_self * confidence_other  + (1 | ID),
    prior = priors,
    sample_prior = 'yes',
    family = gaussian,
    data = data_beast,
    cores = 4,
    chains = 4,
    iter = 4000, 
    seed = 88
  )

## save modelfit
save(list = c("model_interaction_beast_uncertainty"), 
     file = "model_fits/experiment1/model_interaction_beast_uncertainty.RData")


}
## save html table of regression results
tab_model(
  model_interaction_beast_uncertainty,
  show.se = TRUE,
  pred.labels = c(
    "Intercept",
    "Uncertainty (Low)",
    "Confidence Self (High)",
    "Confidence Other (High)",
    "Uncertainty (Low): Confidence Self (High)",
    "Confidence Other (High): Confidence Self (High)"
  ),
  file = "tables/beast_regression_analysis_uncertainty.html"
)

#conditional_effects(model_interaction_beast_uncertainty)

### ZOIB regression ------------------------------------------------------------

## experiment 1

### zoib option 1
zoib_formula_beast <- bf(
  s ~   confidence_self * confidence_other  + (1 |ID),  
  phi ~ confidence_self * confidence_other + (1 |ID),   # formula for phi (constant)
  zoi ~ confidence_self * confidence_other + (1 |ID),   # formula for zero-one inflation
  coi ~ confidence_self * confidence_other + (1 |ID)    
)

## fit model 


## load regression results if already run
if (file.exists('model_fits/experiment1/zoib_model_beast.RData') == TRUE ) {
  
    load('model_fits/experiment1/zoib_model_beast.RData')
  
} else {

zoib_model_beast <- brm(
  formula = zoib_formula_beast,
  data = data_beast,
  family = zero_one_inflated_beta(),
  # prior = priors,
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4, 
  seed = 77
)

## save modelfit
save(list = c("zoib_model_beast"), 
     file = "model_fits/experiment1/zoib_model_beast.RData")

}

conditional_effects(zoib_model_beast)
pp_check(zoib_model_beast)

## experiment 2
data_elections <- full_dataset %>% 
  dplyr::filter(experiment == "Experiment 2")

zoib_formula_elections <- bf(
  s ~   confidence_self * confidence_other + expertise + favorability_fact + same_majority_fact + population  + (1 |ID),  # Formula for mu
  phi ~ confidence_self * confidence_other + expertise + favorability_fact + same_majority_fact + population  + (1 |ID),  # precision of 0-1 values
  zoi ~ confidence_self * confidence_other + expertise + favorability_fact + same_majority_fact + population  + (1 |ID),  # Formula for zero-one inflation
  coi ~ confidence_self * confidence_other + expertise + favorability_fact + same_majority_fact + population  + (1 |ID)  # Formula for phi (constant)
)

## load regression results if already run
if (file.exists('model_fits/experiment2/zoib_model_elections.RData') == TRUE ) {
  
  load('model_fits/experiment1/zoib_model_elections.RData')
  
} else {

zoib_model_elections<- brm(
  formula = zoib_formula_elections,
  data = data_elections,
  family = zero_one_inflated_beta(),
  # prior = priors,
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4, # Use multiple cores for faster sampling
  seed = 1234 # for reproducibility
)

## save modelfit
save(list = c("zoib_model_elections"), 
     file = "model_fits/experiment2/zoib_model_elections.RData")

}
conditional_effects(zoib_model_elections, ask = FALSE)


  