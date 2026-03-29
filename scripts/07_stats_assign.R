# libraries
library(tidyverse)
library(vegan)

# data
data("dune")
data("dune.env")
species.data <- dune
site.data <- dune.env

# q1
  head(species.data)
  head(site.data)
  str(species.data)
  str(site.data)

# q2
  diversity(species.data, index = "shannon")
  diversity(species.data, index = "simpson")  
  
# q3
  fisher.alpha(species.data)
  site.data <- mutate(site.data, 
                      shannon = diversity(species.data, 
                                          index = "shannon"))
  use_shannon_model <- lm(shannon ~ Use, 
                          data = site.data)
  anova(use_shannon_model)
  diversity(species.data, 
            index = "shannon")/ log(specnumber(species.data))

  # rarefaction
  min(rowSums(species.data))
  rarefy(species.data, 15)
  plot(fisherfit(colSums(species.data)))

# q4
  plot(rad.lognormal(colSums(species.data)))

# q5
  radlattice(radfit(colSums(species.data)))
  
# q6
  species.data <- mutate(species.data, Use = site.data$Use)
  haypasture.data <- filter(species.data, Use == 'Haypastu')
  haypasture.data <- select(haypasture.data, -Use)

  radlattice(radfit(colSums(haypasture.data)))  
  plot(rad.preempt(colSums(haypasture.data)))  

# q7
  data("BCI")
  data("BCI.env")
  BCI.species.data <- BCI
  BCI.site.data <- BCI.env

  BCI.site.data <- mutate(BCI.site.data,
                          shannon = diversity(BCI.species.data, index = "shannon"))
  BCI.site.data <- mutate(BCI.site.data,
                          fisher.alpha = fisher.alpha(BCI.species.data))  
  BCI.site.data <- mutate(BCI.site.data,
                          pielou = (diversity(BCI.species.data, index = "shannon") /
                                      log(specnumber(BCI.species.data))))
  BCI.site.data <- mutate(BCI.site.data,
                          rarefied = rarefy(BCI.species.data, 340))
  
  
  
  
  
  
  
  