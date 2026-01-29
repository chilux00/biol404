# libraries
library(tidyverse)
library(pwr)
library(simr)
library(effectsize)
library(lme4)
library(lmerTest)


# read
patch_data <- read_csv("data_raw/patch_data.csv")
plant_data <- read_csv("data_raw/plant_data.csv")

# organize
patch_data <- patch_data %>%
  rename( cover = built_to_forest_ratio)

data <- plant_data %>%
  left_join(patch_data)

tapply(plant_data$high, plant_data$habitat, mean)
tapply(plant_data$mid, plant_data$habitat, mean)
tapply(plant_data$low, plant_data$habitat, mean)
