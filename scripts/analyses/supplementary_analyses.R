###------------------------------------------------------------------------
###------------------------------------------------------------------------
###                                                                     ---
###                         SUPPLEMENTARY ANALYSES                      ---
###                                                                     ---
###------------------------------------------------------------------------
###------------------------------------------------------------------------

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

## specify priors
priors <-c(prior("normal(.5, .1)", class = "Intercept"),
           prior("uniform(-.2, .2)", class = "b", lb = -0.2, ub = 0.2),
           prior("normal(0, 1)", class = "sd"))

model_interaction_beast_full_treatments <-
  brms::brm(
    s ~ confidence_self_f * confidence_other_f + (1 | ID),
    prior = prior,
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

## conditional effects
conditional_effects(model_interaction_beast_full_treatments, 'confidence_self_f:confidence_other_f')

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

conditional_effects(model_interaction_beast_uncertainty)

### ZOIB regression ------------------------------------------------------------


## experiment 1

data_beast %>% 
  count(s == 0) %>% 
  mutate(prop = n / sum(n))

### zoib option 1
zoib_formula_beast <- bf(
  s ~   confidence_self * confidence_other  + (1 |ID),  
  phi ~ confidence_self * confidence_other + (1 |ID),   # formula for phi (constant)
  zoi ~ confidence_self * confidence_other + (1 |ID),   # formula for zero-one inflation
  coi ~ confidence_self * confidence_other + (1 |ID)    
)

## optional: add priors

# prior <- brms::prior_string("normal(0, 0.5)", class = "b") # to be specified depending on assumptions
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

conditional_effects(zoib_model_elections, ask = FALSE)

report(zoib_model_elections)

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

# model 1 

## with interaction
h1 <- c("Other - Self" = "(confidence_otherHigh) > (-confidence_selfHigh)")
hyp <- paste0(parnames(model_interaction_beast_full_scale)[2], " > 0")

hypothesis(model_interaction_beast, h1)


h1 <- c("LH - HL" = "plogis(Intercept + interaction_fLH) > plogis(Intercept)")
hypothesis(zoib_model2, h)

## model 2
h2 <- c("LH - HL" = "plogis(Intercept + interaction_fHL) < plogis(Intercept)")
hypothesis(zoib_model, h2)

## contrasts

library(marginaleffects)

aa <- zoib_model_beast %>%
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


### Vizualization of correct E1 guesses ------------------------------------------

## How often is e1 == to social info?

beast_all_trials <- read_csv("data/beast/clean_data/beast_data_expanded_no_filters.csv")

beast_all_trials %>% 
  ggplot(aes(x = norm1, fill = factor))+
  geom_histogram(binwidth = .02,   aes(fill = (..xmin.. <= 1 & ..xmax.. > 1)),
                 color = "black",
                 show.legend = FALSE
  ) +
  labs( x = "E1 / Correct Value") +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  theme_tidybayes(base_size = 20)

n_perfect_guesses <- beast_all_trials  %>% 
  filter(e1 == nAnimals) %>% 
  count() %>% 
  pull() 

round(n_perfect_guesses/nrow(data_beast) * 100, 2)

## make a vizlualization of the distribution of the excluded rounds

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
  
  