## Simulating a SPSS dataset
##
## This is an example of how you can simulate a .sav file with {surveytoolbox}


#### Load packages ####
library(tidyverse)
library(surveytoolbox)
library(haven)

set.seed(100) # Enable reproducibility

#### Create individual vectors ####
## Record Identifier
v_id <-
  1:1000 %>%
  set_varl("Record Identifier")

## Gender
v_gender <-
  sample(1:3, 1000, replace = TRUE, prob = c(.48, .48, .4)) %>%
  set_vall(value_labels = c("Male" = 1,
                            "Female" = 2,
                            "Other" = 3)) %>%
  set_varl("Q1. Gender")

## KPI
v_kpi <-
  v_gender %>%
  map_dbl(function(x){
    if(x == 1){
      sample(1:5,
             size = 1,
             prob = c(2, 3, 3, 5, 5))
    } else if(x == 2){
      sample(1:5,
             size = 1,
             prob = c(2, 4, 5, 4, 3))

    } else {
      sample(1:5,
             size = 1,
             prob = c(2, 3, 3, 4, 3))
    }
  }) %>%
  set_vall(value_labels = c("Extremely dissatisfied" = 1,
                            "Somewhat dissatisfied" = 2,
                            "Neither" = 3,
                            "Satisfied" = 4,
                            "Extremely satisfied" = 5)) %>%
  set_varl("Q2. KPI")

## Outcome KPI - NPS
v_nps <-
  v_gender %>%
  map_dbl(function(x){
    if(x == 1){
      sample(0:10,
             size = 1,
             prob = c(1, 2, 2, 5, 4, 3, 5, 8, 9, 8, 9))

    } else if(x == 2){

      sample(0:10,
             size = 1,
             prob = c(1, 3, 4, 3, 2, 2, 2, 7, 6, 5, 6))

    } else {
      sample(0:10,
             size = 1,
             prob = c(1, 2, 2, 3, 2, 4, 3, 7, 8, 9, 8))
    }
  }) %>%
  as_nps() %>%
  set_varl("Q3. NPS")

#### Combine individual vectors ####
combined_df <-
  tibble(id = v_id,
         gender = v_gender,
         kpi = v_kpi,
         nps = v_nps)


#### Test the data frame #####

## Variable table
combined_df %>% varl_tb()

## Data dictionary
combined_df %>%
  select(-id) %>%
  data_dict()

## View results
combined_df %>%
  group_by(gender) %>%
  summarise(kpi = mean(kpi),
            nps = mean(nps))

## Clear code frame
combined_df %>% sjPlot::view_df()



