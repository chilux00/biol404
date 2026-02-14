# libraries
library(tidyverse)
library(pwr)
library(simr)
library(effectsize)
library(lme4)
library(lmerTest)
library(scales)
library(patchwork)


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

# anova bootstrapping, internally randomized
F1<-anova_boot(1,100) #selects 1 plant and runs ANOVA, 100 times
F1
F3<-anova_boot(3,100) #selects 3 plants and runs ANOVA, 100 times
F3
F5<-anova_boot(5,100) #selects 5 plants and runs ANOVA, 100 times
F5
F7<-anova_boot(7,100) #selects 7 plants and runs ANOVA, 100 times
F7
F10<-anova_boot(10,100) #selects 10 plants and runs ANOVA, 100 times
F10

# convert F value to measure of effect
f1 <- F_to_f(F1, df = 1, df_error = 18)[1,"Cohens_f_partial"]
f3 <- F_to_f(F3, df = 1, df_error = 18)[1,"Cohens_f_partial"]
f5 <- F_to_f(F5, df = 1, df_error = 18)[1,"Cohens_f_partial"]
f7 <- F_to_f(F7, df = 1, df_error = 18)[1,"Cohens_f_partial"]
f10 <- F_to_f(F10, df = 1, df_error = 18)[1,"Cohens_f_partial"]

# power tests
pwr.anova.test(k = 2, n = 10, f = f1 , sig.level = 0.05)
pwr.anova.test(k = 2, n = 10, f = f3 , sig.level = 0.05)
pwr.anova.test(k = 2, n = 10, f = f5, sig.level = 0.05)
pwr.anova.test(k = 2, n = 10, f = f7 , sig.level = 0.05)
pwr.anova.test(k = 2, n = 10, f = f10 , sig.level = 0.05)

pwr.anova.test(k = 2, power = 0.80, f = f1 , sig.level = 0.05)
pwr.anova.test(k = 2, power = 0.80, f = f3 , sig.level = 0.05)
pwr.anova.test(k = 2, power = 0.80, f = f5 , sig.level = 0.05)
pwr.anova.test(k = 2, power = 0.80, f = f7 , sig.level = 0.05)
pwr.anova.test(k = 2, power = 0.80, f = f10 , sig.level = 0.05)


# calc avg time to setup and locate patch
cl = mean(patch_data$time_to_patch_min) # 11.122 min
ch = mean(patch_data$time_in_patch_min) # 14.3805 min

# repeat for linear model with cover
c10 <- lm(herb10~cover, data = plant_sample)
anova(c10)

F1<-lm_boot(1,100) #selects 1 plant and runs ANOVA, 100 times
F1
F3<-lm_boot(3,100) #selects 3 plants and runs ANOVA, 100 times
F3
F5<-lm_boot(5,100) #selects 5 plants and runs ANOVA, 100 times
F5
F7<-lm_boot(7,100) #selects 7 plants and runs ANOVA, 100 times
F7
F10<-lm_boot(10,100) #selects 10 plants and runs ANOVA, 100 times
F10

# power tests
pwr.anova.test(k = 2, n = 10, f = F1 , sig.level = 0.05)
pwr.anova.test(k = 2, n = 10, f = F3 , sig.level = 0.05)
pwr.anova.test(k = 2, n = 10, f = F5, sig.level = 0.05)
pwr.anova.test(k = 2, n = 10, f = F7 , sig.level = 0.05)
pwr.anova.test(k = 2, n = 10, f = F10 , sig.level = 0.05)

pwr.anova.test(k = 2, power = 0.80, f = F1 , sig.level = 0.05)
pwr.anova.test(k = 2, power = 0.80, f = F3 , sig.level = 0.05)
pwr.anova.test(k = 2, power = 0.80, f = F5 , sig.level = 0.05)
pwr.anova.test(k = 2, power = 0.80, f = F7 , sig.level = 0.05)
pwr.anova.test(k = 2, power = 0.80, f = F10 , sig.level = 0.05)

# ratio calc cost
cj = mean(patch_data$time_to_calculate_ratio, na.rm = TRUE) # 8.604211 min

# priori power test
s10 <-lm(herb10 ~ cover, data = plant_sample)
powerSim(s10)

s10b <- extend(s10, within = "cover", n=5) # simulates up to 5 reps within cover
powerSim(s10b, nsim = 100) #runs power analyses on all simulations

pc1 <- powerCurve(s10b, within = "cover", breaks = 1:5, nsim = 100)
print(pc1) #table form
plot(pc1) #plot form

summary(s10) 
slope <- coef(s10)["cover"] # -0.002537341 

s10<-lm(herb10 ~ cover, data = plant_sample)
coef(s10)["cover"]<- slope*5 #this is where we increase the slope

S10c <- extend(s10, within = "cover", n=125)
powerSim(S10c, nsim = 100)
pc2 <- powerCurve(S10c, within = "cover", breaks = 1:125, nsim = 100)
print(pc2)
plot(pc2) 

# power analysis on linear mixed effect models
mm1<- lmer(score~ height+(1|patch), data = long_data)
summary(mm1)
anova(mm1)

mm2<- lmer(score~ height*habitat+(1|patch), data = long_data)
summary(mm2)
anova(mm2)


powerSim(mm1, nsim = 100) #this is the posthoc power analysis

high_to_low_diff<- fixef(mm1)["heightlow"]
fixef(mm1)["heightlow"]<-high_to_low_diff/2

mm1b <- extend(mm1, within = "height+patch", n=20)
pc3 <- powerCurve(mm1b, test = fixed("height"), nsim = 100, within =
                    "height+patch")
print(pc3)
plot(pc3)

# figure plotting
habitat_tbl <- tibble(
  number_plants = c(1, 3, 5, 7, 10),
  total_time_min = c(4013.004, 7066.377, 18822, 22736.74, 40653.05),
  analysis = "Habitat (built vs forest)"
)

cover_tbl <- tibble(
  number_plants = c(1, 3, 5, 7, 10),
  total_time_min = c(335.6052613, 946.7018504, 8671.89881, 53212.39673, 152368341.6),
  analysis = "Cover (continuous)"
)

fig_df <- bind_rows(habitat_tbl, cover_tbl) 

ggplot(fig_df, aes(x = number_plants, y = total_time_min)) +
  geom_point(size = 2) +
  geom_line(linewidth = 0.8) +
  facet_wrap(
    ~analysis, nrow = 1, scales = "free_y",
    labeller = labeller(analysis = c(
      "Habitat (built vs forest)" = "B.  Habitat (built vs forest)",
      "Cover (continuous)"        = "A.  Cover (continuous)"
    ))
  ) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1,3,5,7,10)) +
  labs(
    x = "Number of plants sampled per patch",
    y = "Total time to reach 80% power (min)"
  ) +
  theme_classic(base_size = 12) + 
  theme(strip.text = element_text(face = "bold", size = 12)) + 
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
  )
