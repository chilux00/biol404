library(tidyverse)
bumpus <- read.csv("data_raw/bumpus2-1.csv")

# q1
  mutated_bumpus <- mutate(bumpus, 
                           length_humerus_mm = length_humerus_in*25.4)
  ggplot(data = mutated_bumpus,
         aes(x = Total_length_mm,
             y = length_humerus_mm)) +
    geom_point()

# q2
  mean(mutated_bumpus$Total_length_mm)  # 160.0368 mm
  filter_bumpus <- group_by(bumpus,
                            total_length_mm < mean(bumpus$Total_length_mm))
                              
  