## Packages
library(tidyverse)
library(lme4)
library(glmmTMB)
library(visreg)

## Read data
data <- read_csv("data_raw/herbivory_data.csv")

## Map herbivory bins to proportions
## These are midpoints of each damage class
herb_map <- c(
  "0-10"   = 0.05,
  "10-20"  = 0.15,
  "20-30"  = 0.25,
  "30-40"  = 0.35,
  "40-50"  = 0.45,
  "50-60"  = 0.55,
  "60-70"  = 0.65,
  "70-80"  = 0.75,
  "80-90"  = 0.85,
  "90-100" = 0.95
)

## Reshape to long format
## One row = one herbivory score from one leaf position on one shrub
herb_long <- data %>%
  mutate(
    plantID = factor(plantID),
    species = factor(species),
    type = factor(type),
    sampler = factor(sampler)
  ) %>%
  pivot_longer(
    cols = c(herbivory_low, herbivory_med, herbivory_high),
    names_to = "leaf_position",
    values_to = "herbivory_bin"
  ) %>%
  mutate(
    herbivory_bin = as.character(herbivory_bin),
    leaf_position = case_when(
      leaf_position == "herbivory_low" ~ "low",
      leaf_position == "herbivory_med" ~ "mid",
      leaf_position == "herbivory_high" ~ "high"
    ),
    leaf_position = factor(leaf_position, levels = c("low", "mid", "high")),
    herbivory_prop = unname(herb_map[herbivory_bin])
  )


## Basic summaries
summary_by_type <- herb_long %>%
  group_by(type) %>%
  summarise(
    n = n(),
    mean_herb = mean(herbivory_prop, na.rm = TRUE),
    sd_herb = sd(herbivory_prop, na.rm = TRUE),
    .groups = "drop"
  )

summary_by_type

summary_by_type_leaf <- herb_long %>%
  group_by(type, leaf_position) %>%
  summarise(
    n = n(),
    mean_herb = mean(herbivory_prop, na.rm = TRUE),
    sd_herb = sd(herbivory_prop, na.rm = TRUE),
    .groups = "drop"
  )

summary_by_type_leaf

## Main beta GLMM

mod_beta <- glmmTMB(
  herbivory_prop ~ type + leaf_position + (1 | plantID) + (1 | sampler) + (1 | species),
  data = herb_long,
  family = beta_family(link = "logit")
)

summary(mod_beta)

## Wald tests for fixed effects
drop1(mod_beta, test = "Chisq")

## Optional interaction model
mod_beta_int <- glmmTMB(
  herbivory_prop ~ type * leaf_position + (1 | plantID) + (1 | sampler) + (1 | species),
  data = herb_long,
  family = beta_family(link = "logit")
)

summary(mod_beta_int)
drop1(mod_beta_int, test = "Chisq")

## Compare additive vs interaction model
anova(mod_beta, mod_beta_int)

## Residual diagnostics
## Pearson residuals vs fitted
fitted_vals <- predict(mod_beta, type = "response")
residual_vals <- residuals(mod_beta, type = "pearson")

par(mfrow = c(1, 2))

plot(
  fitted_vals,
  residual_vals,
  xlab = "Fitted values",
  ylab = "Pearson residuals",
  main = "Residuals vs Fitted"
)
abline(h = 0, lty = 2)

qqnorm(residual_vals, main = "Normal Q-Q Plot of Pearson Residuals")
qqline(residual_vals)

par(mfrow = c(1, 1))

hist(
  residual_vals,
  main = "Histogram of Pearson residuals",
  xlab = "Pearson residuals"
)

## Raw data plot by shrub type
p_type <- ggplot(herb_long, aes(x = type, y = herbivory_prop)) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15) +
  labs(
    x = "Shrub type",
    y = "Herbivory proportion",
    title = "Herbivory in native and non-native shrubs"
  ) +
  theme_bw()

p_type

## Raw data plot by leaf position and shrub type
p_type_leaf <- ggplot(
  herb_long,
  aes(x = leaf_position, y = herbivory_prop, color = type, group = type)
) +
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.08, dodge.width = 0.2),
    alpha = 0.4
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3,
    position = position_dodge(width = 0.2)
  ) +
  stat_summary(
    fun = mean,
    geom = "line",
    position = position_dodge(width = 0.2)
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.12,
    position = position_dodge(width = 0.2)
  ) +
  labs(
    x = "Leaf position",
    y = "Herbivory proportion",
    color = "Shrub type",
    title = "Herbivory by leaf position and shrub type"
  ) +
  theme_bw()

p_type_leaf

## Optional visreg plots
visreg(mod_beta, "type", scale = "response")
visreg(mod_beta, "leaf_position", scale = "response")

