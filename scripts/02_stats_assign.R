library(tidyverse)
library(gapminder)
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
  filter_bumpus <- filter(bumpus,
                            Total_length_mm < mean(bumpus$Total_length_mm))
  filter_bumpus # 65 observations remain
  grouped_bumpus <- group_by(filter_bumpus, survival)
  summarised_bumpus <- summarise(grouped_bumpus, number_inds = n())
  summarised_bumpus # survive 34 die 31
  
# q3
  gdp_gap <- select(gapminder, 
                          country,
                          year,
                          gdpPercap)
  head(gdp_gap) # 6 x 3
  wide_gdp <- pivot_wider(gdp_gap,
                          names_from = year,
                          values_from = gdpPercap)
  head(wide_gdp) # 6 x 13
  filtered_gdp <- filter(wide_gdp,
                         country == "Canada")
  long_gdp <- pivot_longer(filtered_gdp,
                           - country,
                           names_to = "sampling_years",
                           values_to = "sampling_values")
  head(long_gdp) # 6 x 3
  ggplot(long_gdp,
         aes(x = sampling_years,
             y = sampling_values)) +
    geom_point() # increasing trend

# q4
  a == 4
  a = 4
  
  # 
  