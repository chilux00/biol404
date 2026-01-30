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

# area a 
n1a <- nrow(mon_groupA) # num of mon vehicles
n2a <- nrow(weds_groupA) # num of wed vehicles
m2a <- length(intersect(mon_groupA$plate_mon, 
                       weds_groupA$plate_weds)) # num recaptures

n_hata <- (n1a * n2a) / m2a # estimate population size
n_hata # 115.6

# confidence interval calculation (formula from krebs)
var_n <- (n1^2 * n2 * (n2 - m2)) / (m2^3)
se_n  <- sqrt(var_n)

CI_lower <- n_hat - 1.96 * se_n
CI_upper <- n_hat + 1.96 * se_n

CI_lower # 345.7152
CI_upper # 483.576

# area a
var_na <- (n1a^2 * n2a * (n2a - m2a)) / (m2a^3)
se_na  <- sqrt(var_na)

CI_lowera <- n_hata - 1.96 * se_na
CI_uppera <- n_hata + 1.96 * se_na

CI_lowera # 55.40228
CI_uppera # 175.7977

#### jolly seber section

# read files
pop_accuracy <- read_csv("data_raw/pop_inaccuracy.csv")
pop_accuracy$exp <- as.factor(pop_accuracy$exp)
pop_accuracy$exp <- factor(
  pop_accuracy$exp,
  levels = c("first", "second", "third"),
  labels = c("First", "Second", "Third"))

recruit_accuracy <- read_csv("data_raw/recruit_inaccuracy.csv")
recruit_accuracy$exp <- as.factor(recruit_accuracy$exp)
recruit_accuracy$inaccuracy <- as.numeric(recruit_accuracy$inaccuracy)
# coerce to numeric to remove DIV/0 from graph
recruit_accuracy_clean <- subset(
  recruit_accuracy,
  is.finite(inaccuracy) & inaccuracy != 0) # clean na values for div 0 errors
recruit_accuracy_clean$exp <- factor(
  recruit_accuracy_clean$exp,
  levels = c("first", "third"),
  labels = c("First", "Third"))

survival_accuracy <- read_csv("data_raw/survival_inaccuracy.csv")
survival_accuracy$exp <- as.factor(survival_accuracy$exp)
survival_accuracy$exp <- factor(
  survival_accuracy$exp,
  levels = c("first", "second", "third"),
  labels = c("First", "Second", "Third"))

# plots
ggplot(data = pop_accuracy, aes(x = time, 
                                 y = inaccuracy, 
                                 group = exp)) +
  geom_line(aes(linetype = exp)) +
  geom_point() +
  theme_bw() +
  labs(
    x = "Time (t)",
    y = "Inaccuracy (%)",
    linetype = "Experiment"
  ) # pop accuracy

ggplot(data = survival_accuracy, aes(x = time, 
                                y = inaccuracy, 
                                group = exp)) +
  geom_line(aes(linetype = exp)) +
  geom_point() +
  theme_bw() +
  labs(
    x = "Time (t)",
    y = "Inaccuracy (%)",
    linetype = "Experiment"
  ) # survival accuracy

ggplot(data = recruit_accuracy_clean, aes(x = time, 
                                     y = inaccuracy, 
                                     group = exp)) +
  geom_line(aes(linetype = exp)) +
  geom_point() +
  theme_bw() +
  labs(
    x = "Time (t)",
    y = "Inaccuracy (%)",
    linetype = "Experiment"
  ) # recruit accuracy
