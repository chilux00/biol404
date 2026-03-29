# libraries
library(tidyverse)
library(vegan)

# data
data("dune")
data("dune.env")
species.data <- dune
site.data <- dune.env

species.dissim.mat <- vegdist(species.data, 
                              method = "bray")

# q2
  ?vegdist
  site.dissim.mat <- vegdist(site.data$A1, 
                             method = "manhattan")
  
# q3
  mantel(species.dissim.mat, 
         site.dissim.mat, 
         method = "pearson", 
         permutations=9999)
  cluster_fit <- hclust(species.dissim.mat, 
                        method = "average")
  plot(cluster_fit)  

# q4
  rect.hclust(cluster_fit, 
              k = 4, 
              border = "red")
  rect.hclust(cluster_fit, 
              h = 0.5, 
              border = "red")  

  env_cluster_fit <- hclust(site.dissim.mat, 
                            method = "average")
  plot(env_cluster_fit)
  rect.hclust(env_cluster_fit, 
              k = 3, 
              border = "purple")

# q5
  species.NMDS <- metaMDS(species.data, 
                          k=2)
  species.NMDS  
  plot(species.NMDS)  
  ordiplot(species.NMDS, 
           type = "none")  
  orditorp(species.NMDS, 
           display = "species", 
           col = "red", 
           air = 0.01)
  orditorp(species.NMDS, 
           display = "sites", 
           cex = 0.75, 
           air = 0.01)  
  ordiplot(species.NMDS, 
           type = "n")
  ordihull(species.NMDS, 
           groups = site.data$Use, 
           draw = "polygon", 
           col = "grey90", 
           label = FALSE)
  orditorp(species.NMDS, 
           display = "species", 
           col = "red", 
           air = 0.01)
  orditorp(species.NMDS, 
           display = "sites", 
           cex = 0.75, 
           air = 0.01)  

  ordiplot(species.NMDS, 
           type = "none")  
  ordispider(species.NMDS, 
             groups = site.data$Use, 
             col = "black", 
             label = FALSE)
  orditorp(species.NMDS, 
           display = "species", 
           col = "red", 
           air = 0.01)
  orditorp(species.NMDS, 
           display = "sites", 
           cex = 0.75, 
           air = 0.01)  

# q6
  data(BCI)
  data("BCI.env")
  species.BCI <- BCI
  environment.BCI <- BCI.env

  BCI.species.dissim.mat <- vegdist(species.BCI, 
                                    method = "euclidian")  
  BCI.species.NMDS <- metaMDS(species.BCI, k = 2)
  
  ordiplot(BCI.species.NMDS, type = "n")
  
  ordihull(BCI.species.NMDS, 
           groups = environment.BCI$Habitat, 
           draw = "polygon", 
           col = "grey90", 
           label = FALSE)  

  orditorp(BCI.species.NMDS, 
           display = "sites", 
           cex = 0.75, 
           air = 0.01)
  

  
  