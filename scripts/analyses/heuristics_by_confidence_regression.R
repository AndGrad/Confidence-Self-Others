
###-------------------------------------------------------------------------
###-------------------------------------------------------------------------
###                                                                      ---
###                        ANALYSES OF HEURISTICS                        ---
###                                                                      ---
###-------------------------------------------------------------------------
###-------------------------------------------------------------------------

## regression of S by confidence condition

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
if (file.exists('model_fits/experiment1/heuristics_condition_beast_random.RData') == TRUE &
    file.exists('model_fits/experiment1/heuristics_condition_beast.RData') == TRUE) {
  
  load('model_fits/experiment1/heuristics_condition_beast_random.RData')
  load('model_fits/experiment1/heuristics_condition_beast.RData')
  
} else {

## EXPERIMENT 1

## select data
data_beast <- full_dataset %>% 
  dplyr::filter(experiment == "Experiment 1")

prior <- set_prior("normal(0, 1)",class = "b", dpar = c("muCompromise", "muCopy"))

heuristics_condition_beast_random_prior <-
  brm(
    heuristic_f ~ interaction_f + (1|ID),
    family = categorical (link = 'logit'),
    prior = prior,
    sample_prior = 'only',
    data = data_beast,
    cores = 4,
    chains = 4,
    iter = 3000
  )

## run model with random intercept
heuristics_condition_beast_random <-
  brm(
    heuristic_f ~ interaction_f + (1|ID),
    family = categorical (link = 'logit'),
    prior = prior,
    sample_prior = 'yes',
    data = data_beast,
    cores = 4,
    chains = 4,
    iter = 3000
  )

save(list = c("heuristics_condition_beast_random"), 
     file = "model_fits/experiment1/heuristics_condition_beast_random.RData")

## model without random intercept

# heuristics_condition_beast <-
#   brm(
#     heuristic_f ~ interaction_f,
#     family = categorical (link = 'logit'),
#     data = data_beast,
#     cores = 4,
#     chains = 4,
#     iter = 3000
#   )
# 
# save(list = c("heuristics_condition_beast"), 
#      file = "model_fits/experiment1/heuristics_condition_beast.RData")
# 
 }

# ## save html table of regression results
# tab_model(
#   heuristics_condition_beast_random)
#   # show.se = TRUE,
#   # pred.labels = c(
#   #   "Compromise (Intercept)",
#   #   "Copy (Intercept)",
#   #   "Compromise (LH)",
#   #   "Compromise (HL)",
#   #   "Compromise (HH)",
#   #   "Copy (LH)",
#   #   "Copy (HL)",
#   #   "Copy (HH)"
#   #   ),
#   file = "tables/heuristics_condition_beast_table.html"
# )


heuristics_condition_beast_table <- (conditional_effects(heuristics_condition_beast_random, categorical = T))[[1]]
write.csv(heuristics_condition_beast_table, 'tables/heuristics_condition_beast_table_ce.csv')

hypMat_beast<-matrix(0, nrow=0, ncol=8)

hypMat_beast<-rbind(hypMat_beast, hypothesis(heuristics_condition_beast_random, "muCompromise_Intercept> 0 ")$hypothesis[1:8])
hypMat_beast<-rbind(hypMat_beast, hypothesis(heuristics_condition_beast_random, "muCopy_Intercept < 0 ")$hypothesis[1:8])
hypMat_beast<-rbind(hypMat_beast, hypothesis(heuristics_condition_beast_random, "muCompromise_interaction_fLH > 0 ")$hypothesis[1:8])
hypMat_beast<-rbind(hypMat_beast, hypothesis(heuristics_condition_beast_random, "muCompromise_interaction_fHL < 0 ")$hypothesis[1:8])
hypMat_beast<-rbind(hypMat_beast, hypothesis(heuristics_condition_beast_random, "muCompromise_interaction_fHH > 0 ")$hypothesis[1:8])
hypMat_beast<-rbind(hypMat_beast, hypothesis(heuristics_condition_beast_random, "muCopy_interaction_fLH > 0 ")$hypothesis[1:8])
hypMat_beast<-rbind(hypMat_beast, hypothesis(heuristics_condition_beast_random, "muCopy_interaction_fHL > 0 ")$hypothesis[1:8])
hypMat_beast<-rbind(hypMat_beast, hypothesis(heuristics_condition_beast_random, "muCopy_interaction_fHH > 0 ")$hypothesis[1:8])

hypMat_beast_clean <- hypMat_beast %>% 
  dplyr::mutate(
  dplyr::across(
    where(is.numeric),
    ~ ifelse(is.infinite(.x), ">100", sprintf("%.2f", round(.x, 2)))
  )
)

hypMat_beast_clean$Experiment = "Experiment 1"

conditional_effects(heuristics_condition_beast_random, categorical = TRUE)
write.csv(hypMat_beast_clean, 'tables/hypotheses_beast.csv')


## EXPERIMENT 2

## load regression results if already run
if (file.exists('model_fits/experiment2/heuristics_condition_elections_random.RData') == TRUE &
    file.exists('model_fits/experiment2/heuristics_condition_elections.RData') == TRUE) {
  
  load('model_fits/experiment2/heuristics_condition_elections_random.RData')
  load('model_fits/experiment2/heuristics_condition_elections.RData')
  
  
} else {
  
  ## select data
  data_elections <- full_dataset %>% 
    dplyr::filter(experiment == "Experiment 2")

  ## model withrandom intercept
  heuristics_condition_elections_random <-
    brm(
      heuristic_f ~ interaction_f + (1|ID) ,
      family = categorical(link = 'logit'),
      prior = prior,
      sample_prior = 'yes',
      data = data_elections,
      cores = 4,
      chains = 4,
      iter = 3000
    )
  # 
  #   conditional_effects(heuristics_condition_elections_random, categorical = TRUE)
  # fixef(heuristics_condition_beast_random)
  
  save(list = c("heuristics_condition_elections_random"), 
       file = "model_fits/experiment2/heuristics_condition_elections_random.RData")
    # 
    # ## model without random intercept
    # heuristics_condition_elections <-
    # brm(
    #   heuristic_f ~ interaction_f ,
    #   family = categorical(link = 'logit'),
    #   data = data_elections,
    #   cores = 3,
    #   chains = 4,
    #   iter = 2000
    # )
#   
# save(list = c("heuristics_condition_elections"), 
#      file = "model_fits/experiment2//heuristics_condition_elections.RData")

}

# parnames(heuristics_condition_elections)
# table_model(heuristics_condition_elections_random)

heuristics_condition_elections_table<-conditional_effects(heuristics_condition_elections_random,categorical=T)[[1]]

write.csv(heuristics_condition_elections_table, 'tables/heuristics_condition_elections_table.csv')

hypMat_elections<-matrix(0, nrow=0, ncol=8)

hypMat_elections<-rbind(hypMat_elections, hypothesis(heuristics_condition_elections_random, "muCompromise_Intercept > 0 ")$hypothesis[1:8])
hypMat_elections<-rbind(hypMat_elections, hypothesis(heuristics_condition_elections_random, "muCopy_Intercept < 0 ")$hypothesis[1:8])
hypMat_elections<-rbind(hypMat_elections, hypothesis(heuristics_condition_elections_random, "muCompromise_interaction_fLH > 0 ")$hypothesis[1:8])
hypMat_elections<-rbind(hypMat_elections, hypothesis(heuristics_condition_elections_random, "muCompromise_interaction_fHL < 0 ")$hypothesis[1:8])
hypMat_elections<-rbind(hypMat_elections, hypothesis(heuristics_condition_elections_random, "muCompromise_interaction_fHH > 0 ")$hypothesis[1:8])
hypMat_elections<-rbind(hypMat_elections, hypothesis(heuristics_condition_elections_random, "muCopy_interaction_fLH > 0 ")$hypothesis[1:8])
hypMat_elections<-rbind(hypMat_elections, hypothesis(heuristics_condition_elections_random, "muCopy_interaction_fHL < 0 ")$hypothesis[1:8])
hypMat_elections<-rbind(hypMat_elections, hypothesis(heuristics_condition_elections_random, "muCopy_interaction_fHH > 0 ")$hypothesis[1:8])

hypMat_elections_clean <- hypMat_elections %>% 
dplyr::mutate(
  dplyr::across(
    where(is.numeric),
    ~ ifelse(is.infinite(.x), ">100", sprintf("%.2f", round(.x, 2)))
  )
)

hypMat_elections_clean$Experiment = "Experiment 2"

results2exp <- rbind(hypMat_beast_clean,hypMat_elections_clean) %>% 
arrange(Hypothesis)

write.csv(hypMat_elections, 'tables/hypotheses_elections.csv')
write.csv(results2exp, 'tables/hypotheses_both.csv')
