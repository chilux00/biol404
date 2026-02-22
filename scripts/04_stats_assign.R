# libraries
library(tidyverse)
library(visreg)

# read
fertilized_plant <- read.csv("data_raw/FertilizerPlantAgeGrowth2.csv")

# plot
ggplot(data = fertilized_plant, 
       aes(x = plant.age,y = growth.response)) + 
  geom_point()

simple_linear <- lm(growth.response ~ plant.age, data = fertilized_plant)
summary(simple_linear)

# q1
  par(mfrow = c(2,2))
  plot(simple_linear)
  
# q2
  ggplot(data = fertilized_plant, 
         aes(x = plant.age , y = growth.response)) + 
    geom_point() + geom_smooth(method ='lm', se = FALSE)

  bumpus <- read.csv("data_raw/bumpus2-1.csv")  

  ggplot(data = bumpus,
         aes(x = length_humerus_in,
             y = length_femur_in)) +
    geom_point()

  linear.bumpus <- lm(length_humerus_in ~ length_femur_in,
                      data = bumpus)
  summary(linear.bumpus)
  par(mfrow = c(2,2))
  plot(linear.bumpus)  
  
# q3
  residuals_df <- data.frame(simple_residuals = residuals(simple_linear))
  fertilized_plant_res <- cbind(fertilized_plant, residuals_df)

  ggplot(data = fertilized_plant_res,
         aes(x = nutrient,
             y = simple_residuals)) +
    geom_point() # plot b
  
# q4
  ggplot(data = fertilized_plant_res,
         aes(x = nutrient,
             y = simple_residuals)) +
    geom_point() +
    geom_smooth(method = "lm",
                se = FALSE)
  
  plant_linear <- lm(nutrient ~ simple_residuals,
                     data = fertilized_plant_res)  
  summary(plant_linear)  

# q5
  multiple_regression <- lm(growth.response ~ plant.age + nutrient, 
                            data = fertilized_plant)
  anova(multiple_regression)  
  summary(multiple_regression)
  
# q6
  par(mfrow=c(1,2))
  visreg(multiple_regression)
  median(fertilized_plant$nutrient) # 0.8
  median(fertilized_plant$plant.age) # 3
  
# q7
  multiple_regression_interaction <-lm(growth.response ~ plant.age + 
                                         nutrient + plant.age:nutrient, 
                                       data = fertilized_plant)
    
  multi_reg_interaction_star <-lm(growth.response ~ plant.age * nutrient, 
                                  data = fertilized_plant)

# q9
  anova(multiple_regression_interaction)
  anova(multi_reg_interaction_star)  

# q10
  visreg2d(multiple_regression_interaction, 
           "nutrient", 
           "plant.age", 
           plot.type="persp")
  
  bumpus.mr1 <- lm(length_femur_in ~ length_humerus_in + 
                     length_tibiotarsus_in,
                   data = bumpus)
  anova(bumpus.mr1) 
  par(mfrow=c(1,2))
  visreg(bumpus.mr1)  

  bumpus.mr2 <- lm(length_femur_in ~ length_humerus_in +
                     length_tibiotarsus_in + 
                     length_humerus_in:length_tibiotarsus_in,
                   data = bumpus)  
  bumpus.mr3 <- lm(length_femur_in ~ length_humerus_in*length_tibiotarsus_in,
                   data = bumpus)  
  
  par(mfrow=c(1,1))
  visreg2d(bumpus.mr2, 
           "length_humerus_in","length_tibiotarsus_in", 
           plot.type="persp")
  
# q12
  quadratic_regression <- lm(growth.response ~ plant.age + 
                               I(plant.age^2), 
                             data = fertilized_plant)
  summary(quadratic_regression)  
  visreg(quadratic_regression)  

# q13
  log_regression <- lm(growth.response ~ log(plant.age),
                       data = fertilized_plant)
  summary(log_regression)  
  visreg(log_regression)  
  