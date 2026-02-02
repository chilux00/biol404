# packages
library(tidyverse)

# read 
fertilized_block <- read.csv(file = 'data_raw/AnovaFertGrazeBlock2.csv')
is.factor(fertilized_block$block)

# convert cat to factor
fertilized_block <- fertilized_block %>% 
  mutate (fertexpt1 = as.factor(fertexpt1),
          fertexpt2=as.factor(fertexpt2),
          grazing=as.factor(grazing),
          block=as.factor(block))

# q2
  ggplot(data = fertilized_block,
        aes(x = fertexpt1,
             y = growthexpt1)) +
    geom_boxplot() +
    labs(x = "Fertilizer Treatment", 
         y = "Plant Growth") # matches boxplot B
# q3 + 4
  experiment1_model <- lm(growthexpt1 ~ fertexpt1, 
                          data = fertilized_block)
  experiment2_model <- lm(growthexpt2 ~ fertexpt2,
                          data = fertilized_block)
  anova(experiment2.model)  
###      b)     Df Sum Sq Mean Sq F value   Pr(>F)    
###  fertexpt2  1 403.28  403.28     231 5.12e-06 ***
###  Residuals  6  10.47    1.75    
  
# q5
  experiment1_grazing_model <- lm(growthexpt1 ~ fertexpt1 + grazing, 
                                  data = fertilized_block)
  
  experiment2_grazing_model <- lm(growthexpt2 ~ fertexpt2*grazing,
                                  data = fertilized_block)
  
  
  