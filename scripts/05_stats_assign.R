# libraries
library(tidyverse)

# read & setup
fertilized_plant <- read.csv("data_raw/FertilizerPlantAgeGrowth2.csv")
simple_linear <- lm(growth.response ~ plant.age, 
                    data = fertilized_plant)
quadratic_regression <- lm(growth.response ~ plant.age + I(plant.age^2), 
                           data = fertilized_plant)
log_regression <- lm(growth.response ~ log(plant.age), 
                     data=fertilized_plant)

# q1
  extractAIC(simple_linear) # 2.000000 8.743069
  extractAIC(quadratic_regression) # 3.00000 10.41687

  par(mfrow = c(2,2))
  plot(simple_linear)
  plot(quadratic_regression)







