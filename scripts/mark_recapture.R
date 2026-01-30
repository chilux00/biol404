# libraries
library(tidyverse)

# read
mon_plates <- read_csv("data_raw/plates_monday.csv")
wed_plates <- read_csv("data_raw/plates_wednesday.csv")


# filter NA plates, calc intersect (79)
plate_mon_noNA <- filter(mon_plates, plate_mon!="NA")
plate_weds_noNA <- filter(wed_plates, plate_weds!="NA")

length(intersect(plate_mon_noNA$plate_mon, 
                 plate_weds_noNA$plate_weds)) # 79

# filter to group a, calc intersect (10)
mon_groupA<-filter(plate_mon_noNA, area=="a")
weds_groupA<-filter(plate_weds_noNA, area=="a")

length(intersect(mon_groupA $plate_mon, 
                 weds_groupA$plate_weds)) # 10


# population size estimate (krebs ch2)
n1 <- nrow(plate_mon_noNA) # num of mon vehicles
n2 <- nrow(plate_weds_noNA) # num of wed vehicles
m2 <- length(intersect(plate_mon_noNA$plate_mon, 
                       plate_weds_noNA$plate_weds)) # num recaptures

n_hat <- (n1 * n2) / m2 # estimate population size
n_hat # 414.6456

# confidence interval calculation (formula from krebs)
var_n <- (n1^2 * n2 * (n2 - m2)) / (m2^3)
se_n  <- sqrt(var_n)

CI_lower <- n_hat - 1.96 * se_n
CI_upper <- n_hat + 1.96 * se_n

CI_lower # 345.7152
CI_upper # 483.576
