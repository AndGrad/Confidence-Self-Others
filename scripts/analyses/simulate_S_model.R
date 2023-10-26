# model fitting exercis
library(rethinking)
library(tidyverse)
# generate e2

treatment = c("LL", "LH", "HH", "HL")
e1 <- expand.grid(e1 = runif(100, 40, 120), treatment = treatment)

generate_e2 <-
  function(data,
           confidence_self_low = 1.1,
           confidence_self_high = .95,
           confidence_other_low = 1.1,
           confidence_other_high = 0.8,
           w1 = 1.3,
           w2 = 2-w1) {
    
    e1 <- data
    e1$social_info <-  e1$e1 * 1.2 + rnorm(length(e1$e1),0,1)
    
    low_self <- .9
    low_other <- .8
    
    e1 <- e1 %>% 
      mutate( t =  ifelse(substr(treatment, 1, 1) == "L", confidence_self_low, confidence_self_high),
              q =  ifelse(substr(treatment, 2, 2) == "L", confidence_other_low, confidence_other_high),
              w = ((2-t) * (2 - q)),
              e2 = (((e1  * (2-w)) + (social_info * w)/2)) ,
              s =   (e2 - e1)/(social_info - e1)) 
    
      return(e1)
  }

e2 <- generate_e2(e1)

ggplot(e2)+
  geom_density(aes(x=s, color = treatment))





beast_data <- read.csv("data/beast/clean_data/beast_data_expanded.csv") %>% 
  select(e1, e2, confidence_self, confidence_other, social_info) %>% 
  mutate(confidence_self = ifelse(confidence_self == "Low", 1, 2),
         confidence_other = ifelse(confidence_other == "Low", 1, 2))


# estimate model
model <- quap(
  alist(e2 ~ dnorm(mu, sigma),
        mu <- ( (e1 * b1[confidence_self])*1.35 + (social_info * b2[confidence_other])*0.65 )/2,
       # w1 <- dunif(1,2),
        b1[confidence_self] <- dnorm(0,2), # non-negative slope
        b2[confidence_other] <- dnorm(0,2), # non-negative slope
        sigma <- exp(1)
  ), data = beast_data
)

precis(model, depth = 2)

post <- extract.samples(model)



hist(generate_e2(rnorm(100,60,10), 75, 1, 1))
