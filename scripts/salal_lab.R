# libraries
library(tidyverse)
library(pwr)
library(simr)
library(effectsize)
library(lme4)
library(lmerTest)


# read
patch_data <- read_csv("data_raw/patch.csv")
plant_data <- read_csv("data_raw/plant.csv")

# organize
patch_data <- patch_data %>%
  rename( cover = built_to_forest_ratio)

data <- plant_data %>%
  left_join(patch_data)

tapply(plant_data$high, plant_data$habitat, mean)
tapply(plant_data$mid, plant_data$habitat, mean)
tapply(plant_data$low, plant_data$habitat, mean)

# avg herbivory for mid and high
data <- plant_data %>%
  left_join(patch_data) %>%
  mutate(mean_herb = (mid+high)/2)

long_data <- plant_data %>%
  pivot_longer(low:high, names_to = "height", values_to = "score")

# avg mean herb across sampled plants in a patch
plant_sample <- data %>%
  group_by(patch) %>%
  summarize(habitat = first(habitat),
            cover = mean(cover),
            herb10 = mean(mean_herb)) %>%
  ungroup()

# calc mean herb score for 1,3,5,7 plants
set.seed(3456) # seeds: 1234, 2345, 3456

plant_sample <- data %>%
  select(habitat, patch, plant, cover, mean_herb) %>%
  group_by(patch) %>%
  summarize(habitat = first(habitat),
            cover = mean(cover),
            herb1 = mean(sample(mean_herb,1, replace=FALSE)),
            herb3 = mean(sample(mean_herb,3, replace=FALSE)),
            herb5 = mean(sample(mean_herb,5, replace=FALSE)),
            herb7 = mean(sample(mean_herb,7, replace=FALSE)),
            herb10 = mean(mean_herb))%>%
  ungroup()

# anova 
m1<-lm(herb1~habitat, data = plant_sample)
anova(m1)
m3<-lm(herb3~habitat, data = plant_sample)
anova(m3)
m5<-lm(herb5~habitat, data = plant_sample)
anova(m5)
m7<-lm(herb7~habitat, data = plant_sample)
anova(m7)
m10<-lm(herb10~habitat, data = plant_sample)
anova(m10)
