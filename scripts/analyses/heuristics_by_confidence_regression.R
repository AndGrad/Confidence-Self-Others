
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

## run model with random intercept

heuristics_condition_beast_random <-
  brm(
    heuristic_f ~ interaction_f + (1|ID),
    family = categorical (link = 'logit'),
    data = data_beast,
    cores = 3,
    chains = 4,
    iter = 3000
  )

save(list = c("heuristics_condition_beast_random"), 
     file = "model_fits/experiment1/heuristics_condition_beast_random.RData")

## model without random intercept

heuristics_condition_beast <-
  brm(
    heuristic_f ~ interaction_f,
    family = categorical (link = 'logit'),
    data = data_beast,
    cores = 3,
    chains = 4,
    iter = 3000
  )

save(list = c("heuristics_condition_beast"), 
     file = "model_fits/experiment1/heuristics_condition_beast.RData")

}

### What do I want to plot here?? re-read the paper

variables(heuristics_condition_beast)
report(heuristics_condition_beast)

heuristics_condition_beast_random_table <-
  conditional_effects(heuristics_condition_beast_random, categorical = T)[[1]]

plot(conditional_effects(heuristics_condition_beast, categorical = T))
write.csv(heuristics_condition_beast_table, 'heuristics_condition_beast_table.csv')



random_error_model <- load(paste(path,"data/heuristics_condition_beast_random.RData", sep = ""))

prediction <- predict(heuristics_condition_beast_random, newdata = data.frame(interaction_f = treatments, ID = IDs ))

IDs = NULL

for(n in unique(final_data_no_med$ID)){
  p = rep(n, 4)
  IDs <- c(IDs, p)
}

treatment <- c('LL', 'LH', 'HL', 'HH')
treatments <- rep(treatment, 200)

mean(prediction[,1])
mean(prediction[,2])
mean(prediction[,3])

pred <- posterior_predict(heuristics_condition_beast_random)

newdata <- data.frame(interaction_f = c(1,2,3,4))
predict(fit1, newdata = newdata, re_formula = NA)

data_grid

plot(conditional_effects.brmsfit(heuristics_condition_beast_random,categorical=T))


hypMat<-matrix(0, nrow=0, ncol=8)
hypMat<-rbind(hypMat, hypothesis(heuristics_condition_beast, "muCompromise_Intercept >0")$hypothesis[1:8])
hypMat<-rbind(hypMat, hypothesis(heuristics_condition_beast, "muCopy_Intercept > 0 ")$hypothesis[1:8])
hypMat<-rbind(hypMat, hypothesis(heuristics_condition_beast, "muCompromise_interaction_fLH > 0 ")$hypothesis[1:8])
hypMat<-rbind(hypMat, hypothesis(heuristics_condition_beast, "muCompromise_interaction_fHL > 0 ")$hypothesis[1:8])
hypMat<-rbind(hypMat, hypothesis(heuristics_condition_beast, "muCompromise_interaction_fHH > 0 ")$hypothesis[1:8])
hypMat<-rbind(hypMat, hypothesis(heuristics_condition_beast, "muCopy_interaction_fLH > 0 ")$hypothesis[1:8])
hypMat<-rbind(hypMat, hypothesis(heuristics_condition_beast, "muCopy_interaction_fHL > 0 ")$hypothesis[1:8])
hypMat<-rbind(hypMat, hypothesis(heuristics_condition_beast, "muCopy_interaction_fHH > 0 ")$hypothesis[1:8])

write.csv(hypMat, 'hypotheses_beast.csv')


## EXPERIMENT 2

## load regression results if already run
if (file.exists('model_fits/experiment2/heuristics_condition_elections_random.RData') == TRUE &
    file.exists('model_fits/experiment2/heuristics_condition_elections.RData') == TRUE) {
  
  load('model_fits/experiment2/heuristics_condition_elections_random.RData')
  load('model_fits/experiment1/heuristics_condition_elections.RData')
  
  
} else {
  
  ## select data
  data_elections <- full_dataset %>% 
    dplyr::filter(experiment == "Experiment 2")

  ## model withrandom intercept
  heuristics_condition_elections_random <-
    brm(
      heuristic_f ~ interaction_f + (1|ID) ,
      family = categorical(link = 'logit'),
      data = data_elections,
      cores = 3,
      chains = 4,
      iter = 3000
    )
  
  save(list = c("heuristics_condition_elections_random"), 
       file = "model_fits/experiment2/heuristics_condition_elections_random.RData")
  
    ## model without random intercept
    heuristics_condition_elections <-
    brm(
      heuristic_f ~ interaction_f ,
      family = categorical(link = 'logit'),
      data = data_elections,
      cores = 3,
      chains = 4,
      iter = 2000
    )
  
save(list = c("heuristics_condition_elections"), 
     file = "model_fits/experiment2//heuristics_condition_elections.RData")

}

parnames(heuristics_condition_elections)
report(heuristics_condition_elections)

heuristics_condition_elections_table<-conditional_effects(heuristics_condition_elections,categorical=T)[[1]]

write.csv(heuristics_condition_elections_table, 'heuristics_condition_elections_table.csv')


hypMat<-matrix(0, nrow=0, ncol=8)
hypMat<-rbind(hypMat, hypothesis(heuristics_condition_elections, "muCompromise_Intercept > 0 ")$hypothesis[1:8])
hypMat<-rbind(hypMat, hypothesis(heuristics_condition_elections, "muCopy_Intercept > 0 ")$hypothesis[1:8])
hypMat<-rbind(hypMat, hypothesis(heuristics_condition_elections, "muCompromise_interaction_fLH > 0 ")$hypothesis[1:8])
hypMat<-rbind(hypMat, hypothesis(heuristics_condition_elections, "muCompromise_interaction_fHL > 0 ")$hypothesis[1:8])
hypMat<-rbind(hypMat, hypothesis(heuristics_condition_elections, "muCompromise_interaction_fHH > 0 ")$hypothesis[1:8])
hypMat<-rbind(hypMat, hypothesis(heuristics_condition_elections, "muCopy_interaction_fLH > 0 ")$hypothesis[1:8])
hypMat<-rbind(hypMat, hypothesis(heuristics_condition_elections, "muCopy_interaction_fHL > 0 ")$hypothesis[1:8])
hypMat<-rbind(hypMat, hypothesis(heuristics_condition_elections, "muCopy_interaction_fHH > 0 ")$hypothesis[1:8])

write.csv(hypMat, 'hypotheses_elections.csv')

#######################

heuristics_condition_elections_ID <- brm(heuristic_f ~ interaction_f + (1|ID),family= categorical (link='logit'), data=data_filter, cores=3, chains=4, iter=2000) 

heuristics_condition_elections_table_ID<-conditional_effects(heuristics_condition_elections_ID,categorical=T)[[1]]
write.csv(heuristics_condition_elections_table_ID, 'heuristics_condition_elections_table_ID.csv')


##### WHO DRIVES THE RANDOM EFFECTS?

## explore dataset withouth people that copy all the time

data_beast <- data_beast %>% 
  filter(ID != c(45 | 71 | 118 | 123 | 171| 175| 165 | 11 | 21| 14 | 48| 111 | 95))

heuristics_condition_beast_random_no_copy<- brm(heuristic_f ~ interaction_f + (1| ID),family= categorical (link='logit'), data=final_data_no_med_no_copiers, cores=3, chains=4, iter=2000) 
plot(conditional_effects(heuristics_condition_beast_random_no_copy,categorical=T))

heuristics_condition_beast_no_copy<- brm(heuristic_f ~ interaction_f,family= categorical (link='logit'), data=final_data_no_med_no_copiers, cores=3, chains=4, iter=2000) 
plot(conditional_effects(heuristics_condition_beast_no_copy,categorical=T))

