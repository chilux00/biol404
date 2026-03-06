# libraries
library(tidyverse)
library(car)
library(visreg)

# read & setup
fertilized_plant <- read.csv("data_raw/FertilizerPlantAgeGrowth2.csv")
simple_linear <- lm(growth.response ~ plant.age, 
                    data = fertilized_plant)
quadratic_regression <- lm(growth.response ~ plant.age + I(plant.age^2), 
                           data = fertilized_plant)
log_regression <- lm(growth.response ~ log(plant.age), 
                     data=fertilized_plant)

# q1
  anova(quadratic_regression, log_regression) # p 0.3383

  extractAIC(quadratic_regression) # 3.00000 10.41687
  extractAIC(log_regression) # 2.000000 9.821937

  summary(quadratic_regression) # r2 0.7719 
  summary(log_regression) # r2 0.0005055

  par(mfrow = c(2,2))
  plot(log_regression)
  
  par(mfrow = c(2,2))
  plot(quadratic_regression)

# q2
  plant_growth_nutrient_age <- lm(growth.response ~ nutrient + plant.age, 
                                  data = fertilized_plant)
  
  plant_growth_age_nutrient <- lm(growth.response ~ plant.age + nutrient,
                                  data = fertilized_plant)
  
  anova(plant_growth_nutrient_age)
  anova(plant_growth_age_nutrient) 

# q3
  plant_growth_nutrient <- lm(growth.response ~ nutrient, 
                              data = fertilized_plant)
  plant_growth_age <- lm(growth.response ~ plant.age, 
                         data = fertilized_plant)
  
  anova(plant_growth_age, plant_growth_age_nutrient)
  anova(plant_growth_age_nutrient)

# q4
  plant_growth_nutrient_age <- lm(growth.response ~ nutrient + plant.age, 
                                  data = fertilized_plant)
  anova(plant_growth_nutrient, plant_growth_nutrient_age)
  anova(plant_growth_nutrient_age)
  
# q5
  Anova(plant_growth_age_nutrient, type = 2)
  Anova(plant_growth_nutrient_age, type = 2)
  
# q6
  Anova(plant_growth_nutrient_age, type = 2)
  anova(plant_growth_nutrient_age)
  anova(plant_growth_age_nutrient)
  
# q7
  plant_growth_nutrient_age_interaction <- 
    lm(growth.response ~ nutrient * plant.age, 
       data = fertilized_plant)
  
  Anova(plant_growth_nutrient_age_interaction, type = 3)
  
# q8
  plant_growth_age_nutrient_interaction <- 
    lm(growth.response ~ plant.age * nutrient,
       data = fertilized_plant)
  
  Anova(plant_growth_age_nutrient_interaction, type = 3)

# q9
  anova(plant_growth_age_nutrient_interaction, plant_growth_age_nutrient)
  plant_growth_age_interaction <- 
    lm(growth.response ~ plant.age + plant.age:nutrient, 
       data = fertilized_plant)
  
  anova(plant_growth_age_nutrient_interaction,  plant_growth_age_interaction)
  
  # full model
  plant_growth_nutrient_age_interaction <- 
    lm(growth.response ~ nutrient * plant.age,
       data = fertilized_plant)
  # reduced model                                            
  plant_growth_nutrient_interaction <- 
    lm(growth.response ~ nutrient + nutrient:plant.age,
       data = fertilized_plant)
  
  anova(plant_growth_nutrient_age_interaction, 
        plant_growth_nutrient_interaction) # f 37.801 
  
# q10
  ggplot(data = fertilized_plant, 
         aes(x = plant.age, 
             y = growth.response, 
             colour = fertilizer)) + 
    geom_point()
  
  growth_age_fertilizer <- lm(growth.response ~ plant.age*fertilizer, 
                              data = fertilized_plant)
  
  Anova(growth_age_fertilizer, type = 2)   
  
  visreg(growth_age_fertilizer, "plant.age")
  visreg(growth_age_fertilizer, "fertilizer")
  
# q11
  ?geom_abline
  