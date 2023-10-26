###-------------------------------------------------------------------------
###-------------------------------------------------------------------------
###                                                                      ---
###                            DATA WRANGLING                            ---
###                                                                      ---
###-------------------------------------------------------------------------
###-------------------------------------------------------------------------


### This scripts imports the cleaned experimental data and prepares it for the analyses
# What is calculated here:
# - accuracy
# - errors
# - heuristics use is classified
# - variables are renamed and types are transformed for purposes of plotting and model fitting

# Load data and packages ------------------------------------------------------
source("scripts/load_experimental_data.R")
options(scipen = 1000)

### Experiment 1 data ---------------------------------------------------------

### Create new variables
beast_data_expanded <-
  beast_data %>% 
  mutate(uncertainty = ordered(
    treatment,
    levels = c(0, 1),
    labels = c("High", "Low") # create ordered factor for the uncertainty treatment
  )) %>%   
  mutate(confidence_other = ordered(
    confidenceTreatment,
    levels = c("1", "2", "3"),
    labels = c('Low', 'Medium', 'High') # create ordered factor for peer confidence treatment
  )) %>%
  mutate(confidence_self_category = ifelse(confidence <= 4, 1, ifelse(confidence > 4 & confidence < 7 ,2,ifelse(confidence > 6 ,3, 0 )))) %>% # split  confidence self in 3 bins (for comparison with election experiment)
  mutate(confidence_self = ordered(
    confidence_self_category,
    levels = c("1", "2", "3"),
    labels = c('Low', 'Medium', 'High') # create ordered factor for reported confidence
  )) %>%
  mutate(social_confidence_factor = as.factor(social_confidence)) %>% 
  mutate(norm1 = e1/nAnimals) %>% # normalized first estimate: 1 = correct, < 1 = underestimate, > 1 overestimate
  mutate(std_error = norm1 - mean(norm1, na.rm = TRUE)) %>% # calculate standard error
  mutate(under = ifelse(norm1 < 1, -1, ifelse(norm1 > 1,1, 0 ))) %>%  
  mutate(under_factor = ordered(under, levels = c("-1", "0", "1"),
                                labels = c("underestimate", "correct", "overestimate"))) %>% # create factor for over, under and correct estimates
  mutate(copy = ifelse(e2 == social_info, 1, ifelse(e2 != social_info, 0, NA ))) %>%  # 
  mutate(stay = ifelse(e2 == e1, 1, ifelse(e2 != e1, 0, NA ))) %>% 
  mutate(diff_conf = social_confidence - confidence) %>% 
  mutate(heuristic = ifelse(e2 == e1, 1, ifelse(e2 != e1 & e2 != social_info, 2, ifelse(e2 == social_info, 3, NA) )))  %>% 
  mutate(heuristic_f = ordered(
    heuristic,
    levels = c("1", "2", "3"),
    labels = c('Stay', 'Compromise', 'Copy'))) %>%   # create factor for heuristics (stay, copy, compromise)
  mutate(interaction = ifelse(confidence_self == "Low" & confidence_other == "Low", 1,
                              ifelse(confidence_self == "Low" & confidence_other == "High", 2,
                                     ifelse(confidence_self == "High" & confidence_other == "Low", 3,
                                            ifelse(confidence_self == "High" & confidence_other == "High", 4, NA))))) %>% # label combinations of High (H) and Low (L) self and other confidence (4 treatments, LL, LH, HL ,HH)
  mutate(interaction_f = ordered(
    interaction,
    levels = c("1", "2", "3", "4"),
    labels = c('LL', 'LH', 'HL', 'HH'))) %>%  
  mutate(linetype = ifelse(interaction_f == 'LL', '11', ifelse(interaction_f =='LH', '88', ifelse(interaction_f == 'HL', '3', '1')))) %>%  # add linetype code for plotting
  mutate(experiment = "Experiment 1") 

## These are all the variables without any filtering/exclusions.
## If you want to conduct your own exploratory analyses, you can start from the dataset saved in the next line of code:

write.csv(beast_data_expanded, "data/beast/clean_data/beast_data_expanded_no_filters.csv")

## From here on, the authors analyses choices will start to determine the results.

## Clean dataset based on pre-registerd criteria:
# 1) remove treatments where social information use (s) is 0>s>1
# 2) remove filler trials (confidence of other is medium). this is also done to facilitate comparison with exp 2

beast_data_expanded <- beast_data_expanded %>%
  dplyr::filter(!is.infinite(s)) %>% 
  dplyr::filter(s <= 1 & s >= 0) %>% 
  dplyr::filter(confidence_other != "Medium") %>% 
  dplyr::filter(confidence_self != "Medium") 
  
## structure confidence so it is compatible with elections data 

beast_data_expanded$confidenceTreatment[beast_data_expanded$confidenceTreatment == 3] <- 2
beast_data_expanded$confidence_self_category[beast_data_expanded$confidence_self_category == 3] <- 2

  
beast_data_expanded <- beast_data_expanded %>% 
  mutate(confidence_other = ordered(
    confidenceTreatment,
    levels = c("1", "2"),
    labels = c('Low', 'High') # create ordered factor for peer confidence treatment
  )) %>% 
  mutate(confidence_self = ordered(
    confidence_self_category,
    levels = c("1", "2"),
    labels = c('Low', 'High') # create ordered factor for reported confidence
  ))
  
## aggregate data 2 by 2 (confidence_self and other)
beast_data_2by2 <- beast_data_expanded %>% 
  group_by(ID, interaction_f) %>% 
  summarise(S = mean(s)) %>% 
  mutate(experiment = "Experiment 1")

## heuristics dataset
heuristics_beast_data <- beast_data_expanded %>%
  mutate(bin = ifelse(s == 0, 1, ifelse(s < 0.5, 2, ifelse(s == 0.5, 3, ifelse(s > 0.5 & s < 1, 4, 5))))) %>% 
  group_by(interaction_f, bin) %>%
  summarise(count = n()) %>% 
  mutate(heuristic_total = sum(count)) %>% 
  mutate(freq = count / heuristic_total) %>% 
  mutate(perc = round(freq * 100,0))


## write dataset to .csv file
write.csv(beast_data_expanded, "data/beast/clean_data/beast_data_filtered.csv")

## write 2x2 dataset to .rda file
save(file = "data/beast/clean_data/meanS_data_beast.rda", data = beast_data_2by2)

## write heuristics dataset to .rda file
save(file = "data/beast/clean_data/heuristics_data_beast.rda", data = heuristics_beast_data)




### Experiment 2 data ----------------------------------------------------------

## Load txt file with results of elections state by state
elections_results <- read.csv('data/elections/clean_data/electionOutcome.txt', header = TRUE)
states_short <- read.csv('data/elections/clean_data/states_short.txt', header = TRUE)
population <- read.csv('data/elections/clean_data/states_pop.txt', header = TRUE)

elections_data_expanded <- elections_data %>%
  mutate(confidence_self_number = ifelse(confidence == "btnHigh", 2, ifelse(confidence == "btnLow", 1, NA))) %>%
  mutate(confidence_self = ordered(
    confidence_self_number,
    levels = c("1", "2"),
    labels = c("Low", "High")
  )) %>%
  mutate(confidence_other_number = ifelse(
    confidence_other == "high",
    2,
    ifelse(confidence_other == "low", 1, NA)
  )) %>%
  mutate(confidence_other = ordered(
    confidence_other_number,
    levels = c("1", "2"),
    labels = c("Low", "High")
  )) %>%
  fill(gender, .direction = "up") %>%
  mutate(
    state_type = ifelse(
      state == "D.C." |
        state == "Hawaii" |
        state == "Massachusetts" |
        state == "California" |
        state == "New York",
      "1",
      ifelse(
        state == "Wyoming" |
          state == "West Virginia" |
          state == "Oklahoma" |
          state == "Kentucky" |
          state == "Alabama",
        "4",
        ifelse(
          state == "Ohio" |
            state == "Georgia" |
            state == "Iowa" |
            state == "Texas" | state == "South Carolina",
          "3",
          "2"
        )
      )
    )
  ) %>%
  mutate(state_type_factor = ordered(
    state_type,
    levels = c("1", "2", "3", "4"),
    labels = c("Sure Dem", "Dem Swing", "Rep Swing", "Sure Rep")
  )) %>%   mutate(favorability = ifelse(
    ownParty == "Democratic" &
      social_info > 50,
    1,
    ifelse(ownParty == "Republican" & social_info < 49, 1, 0)
  )) %>%
  mutate(favorability_fact = ordered(
    favorability,
    levels = c("0", "1"),
    labels = c("opp", "same")
  )) %>%
  mutate(same_majority = ifelse(e1 > 50 &
                                  social_info > 50, 1, ifelse(e1 < 49 &
                                                                social_info < 49, 1, 0))) %>%
  mutate(same_majority_fact = ordered(
    same_majority,
    levels = c("0", "1"),
    labels = c("opp", "same"))) %>% 
  mutate(conditions = ifelse(
    confidence_self_number == 1 & confidence_other_number == 1,
    "LL",
    ifelse(
      confidence_self_number == 1 & confidence_other_number == 2,
      "LH",
      ifelse(confidence_self_number == 2 &
               confidence_other_number == 1, "HL", "HH")
    )
  )) %>% 
  left_join(elections_results, by = 'state') %>% 
  mutate(winner = ifelse(percDem >= 50, 1, 0)) %>%
  
  left_join(states_short, by = 'state') %>% 
  left_join(population, by = 'state') %>% 
  mutate(population = population/10) %>% 
  
  mutate(diff1 = abs((e1 - percDem))) %>% 
  mutate(diff2 = abs((e2 - percDem))) %>% 
  mutate(improvement = diff1 - diff2) %>% 
  mutate(heuristic = ifelse(e2 == e1, 1, ifelse(e2 != e1 & e2 != social_info, 2, ifelse(e2 == social_info, 3, NA) )))  %>% 
  mutate(heuristic_f = ordered(
    heuristic,
    levels = c("1", "2", "3"),
    labels = c('Stay', 'Compromise', 'Copy'))) %>% 
  mutate(interaction = ifelse(confidence_self == "Low" & confidence_other == "Low", 1,
                              ifelse(confidence_self == "Low" & confidence_other == "High", 2,
                                     ifelse(confidence_self == "High" & confidence_other == "Low", 3,
                                            ifelse(confidence_self == "High" & confidence_other == "High", 4, NA))))) %>% 
  mutate(interaction_f = ordered(
    interaction,
    levels = c("1", "2", "3", "4"),
    labels = c('LL', 'LH', 'HL', 'HH'))) %>% 
  mutate(copy = ifelse(e2 == social_info, 1, ifelse(e2 != social_info, 0, NA ))) %>% 
  mutate(stay = ifelse(e2 == e1, 1, ifelse(e2 != e1, 0, NA ))) %>% 
  mutate(linetype = ifelse(interaction_f == 'LL', '11', ifelse(interaction_f =='LH', '88', ifelse(interaction_f == 'HL', '3', '1')))) %>% 
  mutate(experiment = "Experiment 2")

## These are all the variables without any filtering/exclusions.
## If you want to conduct your own exploratory analyses, you can start from the dataset saved in the next line of code:

write.csv(elections_data_expanded, "data/elections/clean_data/elections_data_expanded_no_filters.csv")

## From here on, the authors analyses choices will start to determine the results.


### Clean dataset based on pre-registerd criteria:
## 1) remove treatments where social information use (s) is 0>s>1
## 2) remove trials where a target for social information was not found
## 3) @andrea check what treatment 3 & 4 are to make sure of this


elections_data_expanded <- elections_data_expanded %>% 
  dplyr::filter(treatment == 1 | treatment == 2) %>% 
  dplyr::filter(target_found == 1) %>%  
  dplyr::filter(s >= -0 & s <= 1)

elections_data_2by2 <- elections_data_expanded %>%
  group_by(ID,interaction_f) %>% 
  summarise(S = mean(s)) %>%  
  dplyr::filter(S >= -0 & S <= 1) %>% 
  mutate(experiment = "Experiment 2")

## heuristics dataset
heuristics_elections_data <- elections_data_expanded %>%
  mutate(bin = ifelse(s == 0, 1, ifelse(s < 0.5, 2, ifelse(s == 0.5, 3, ifelse(s > 0.5 & s < 1, 4, 5))))) %>% 
  group_by(interaction_f, bin) %>%
  summarise(count = n()) %>% 
  mutate(heuristic_total = sum(count)) %>% 
  mutate(freq = count / heuristic_total) %>% 
  mutate(perc = round(freq * 100,0))


## write filtered dataset to .csv file
write.csv(elections_data_expanded, "data/elections/clean_data/elections_data_filtered.csv")

## write 2x2 dataset to .rda file
save(file = "data/elections/clean_data/meanS_data_elections.rda", data = elections_data_2by2)

## write heuristics dataset to .rda file
save(file = "data/elections/clean_data/heuristics_elections_beast.rda", data = heuristics_elections_data)




### Merge datasets ------------------------------------------------------------

## remove redundant columns 
experiment1 <- beast_data_expanded %>% 
  dplyr::select(-c(confidenceTreatment, gender)) %>%
  dplyr::rename(gender = gender_factor)
  
experiment2 <- elections_data_expanded %>% 
  dplyr::select(-c(confidence))

## merge the 2 datasets
full_dataset <- bind_rows(experiment1, experiment2) 

## write merged dataset to file .csv file
## this is what will be used for all the main scripts

write.csv(full_dataset, "data/full_dataset.csv", row.names = FALSE)
save(full_dataset, file = "data/full_dataset.rda")

