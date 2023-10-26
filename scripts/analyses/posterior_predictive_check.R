##################################################################
##               Draw from posterior distribution               ##
##################################################################

#@andrea: if we have this in the end, add experiment 1 as well

## Experiment 2 -----------------------------------------------------------------------------------

## read model fit data



post_sample_elections <- as_draws(model_interaction_elections, pars = c('b_Intercept',
                                                                        'b_confidence_selfHigh','b_confidence_otherHigh', 'b_confidence_selfHigh:confidence_otherHigh'))

## rebuild treatments from coefficient estimates
post_data_elections <- data.frame( LL = post_sample_elections[[1]]$b_Intercept,
                                   HL = post_sample_elections[[1]]$b_Intercept + post_sample_elections[[1]]$b_confidence_selfHigh,
                                   LH =  post_sample_elections[[1]]$b_Intercept + post_sample_elections[[1]]$b_confidence_otherHigh,
                                   HH =  post_sample_elections[[1]]$b_Intercept + post_sample_elections[[1]]$`b_confidence_selfHigh:confidence_otherHigh`, # @andrea not so sure about this one
                                   experiment = factor(rep('Experiment 2', 1500)), labels = '1')

# transform it into long version for ggplot
post_data_elections_long <- gather(post_data_elections, key = 'confidence', value = 'estimate', 1:4) %>% 
  group_by(confidence, experiment) %>% 
  mutate(q5 = quantile(estimate, .05),
         q95 = quantile(estimate, .95),
         median = median(estimate),
         mean = mean(estimate),
         x = density(estimate, n = 1500)$x,
         y = density(estimate, n = 1500)$y,
         ymax = max(y))
