# packages
library(tidyverse)

# read
fertilized_block <- read_csv(file = 'data_raw/AnovaFertGrazeBlock2.csv')
  head(fertilized_block)
  fertilized_block$block
  fertilized_block$fertexpt1
  str(fertilized_block)
  
  # convert to factor
  fertilized_block$fertexpt1<-as.factor(fertilized_block$fertexpt1)
  fertilized_block$fertexpt2<-as.factor(fertilized_block$fertexpt2)
  fertilized_block$grazing<-as.factor(fertilized_block$grazing)
  fertilized_block$block<-as.factor(fertilized_block$block)

# analysis
  # q1
  porcupine <- c(108, 111, 114, 115, 116, 118, 123)
  mean (porcupine) > median (porcupine)
  median(porcupine) > mean (porcupine)
  mean (porcupine) == median (porcupine) # 115
  
  # q2
  var(porcupine) # 23.33333
  sd(porcupine) # 4.830459
  sqrt(var(porcupine)) # 4.830459
  
  # q3
  purple <- 120:160
  (sd(purple))/mean(purple)*100 # cv = 8.556535
  
  # q4
  quantile(purple) # median 140, interquartile 130 to 150
  150 - 130 # 20
  
  # q5
  
  # q6
  summary(fertilized_block$fertexpt1)
  summary(fertilized_block$fertexpt2)
  summary(fertilized_block$growthexpt1) # mean 7.037
  summary(fertilized_block$growthexpt2) # mean 11.675
  summary (fertilized_block)
  
  # q7
  fertilized_block$total_growth <- 
    fertilized_block$growthexpt1 + fertilized_block$growthexpt2
  fertilized_block$total_fert <-
    fertilized_block$fertexpt1 + fertilized_block$fertexpt2
  # ‘+’ not meaningful for factors
  
  # q8
  ggplot(data = fertilized_block, 
         aes(x = fertexpt2, 
             y = growthexpt2)) + 
  geom_boxplot()
  
  # q9
  bumpus <- read_csv(file = 'data_raw/bumpus2-1.csv')
  str(bumpus)
  head(bumpus)
  ggplot(data = bumpus,
         aes(x = Total_length_mm,
             y = weight_g)) +
    geom_point() +
    geom_smooth(method = lm, 
                se = FALSE)
  
  # q10
  ??log()
  
# write output
