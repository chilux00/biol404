#### Cleaning data + Running Analysis

## Load libraries
library(tidyverse)
library(broom)
library(dplyr)

## Read in data
data <- read_csv("data_raw/herbivory_data.csv")

## Mutate herbivory to be numerical

# Define mapping
herb_map <- c(
  "0-10%" = 1,
  "10-20%" = 2,
  "20-30%" = 3,
  "30-40%" = 4,
  "40-50%" = 5,
  "50-60%" = 6,
  "60-70%" = 7,
  "70-80%" = 8,
  "80-90%" = 9,
  "90-100%" = 10)


data <- data %>%
  mutate(
    herbivory_low = herb_map[herbivory_low],
    herbivory_med = herb_map[herbivory_med],
    herbivory_high = herb_map[herbivory_high])

## Structure data

data <- data %>%
  mutate(
    type = factor(type),
    sampler = factor(sampler),
    herbivory_mean = (herbivory_low +
                        herbivory_med +
                        herbivory_high) / 3)

## Compare groups to see mean herbivory

mean_herbivory_table <- data %>%
  group_by(type) %>%
  summarise(
    low_mean  = mean(herbivory_low),
    med_mean  = mean(herbivory_med),
    high_mean = mean(herbivory_high),
    mean_all  = mean(herbivory_mean)
  )

mean_herbivory_table

## Run linear models

levels(data$type) ## Will tell us how to interpert the coefficients

mod_low  <- lm(herbivory_low  ~ type + height + sampler, 
               data = data)
summary(mod_low)

mod_med  <- lm(herbivory_med  ~ type + height + sampler, 
               data = data)
summary(mod_med)

mod_high <- lm(herbivory_high ~ type + height + sampler, 
               data = data)
summary(mod_high)

mod_mean <- lm(herbivory_mean ~ type + height + sampler, 
               data = data)
summary(mod_mean)

## Make a table with results

type_results <- bind_rows(
  tidy(mod_low)  %>% filter(term == "typeNative") %>% mutate(model = "Low"),
  tidy(mod_med)  %>% filter(term == "typeNative") %>% mutate(model = "Medium"),
  tidy(mod_high) %>% filter(term == "typeNative") %>% mutate(model = "High"),
  tidy(mod_mean) %>% filter(term == "typeNative") %>% mutate(model = "Mean")) %>%
  select(model, estimate, std.error, p.value)

type_results

## Save table

write_csv(type_results, "results/model_results")
