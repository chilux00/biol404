library(tidyverse)

anova_boot<-function(n,j)
{ 
  out<-c(1:j) %>% as.data.frame() %>% rename(out = ".")
  for(i in 1:j) 
  {
    plant_sample <- data %>% 
      select(habitat, patch, plant, cover, mean_herb) %>% 
      group_by(patch) %>% 
      summarize(habitat = first(habitat),
                cover = first(cover),
                herb = mean(sample(mean_herb,n, replace=FALSE)))%>% 
      ungroup() 
    
    
    out$out[i]<-anova(lm(herb~habitat, data = plant_sample))[1,"F value"]
  }
  return(mean(out$out))
}

lm_boot<-function(n,j)
{ 
  out<-c(1:j) %>% as.data.frame() %>% rename(out = ".")
  for(i in 1:j) 
  {
    plant_sample <- data %>% 
      select(habitat, patch, plant, cover, mean_herb) %>% 
      group_by(patch) %>% 
      summarize(habitat = first(habitat),
                cover = first(cover),
                herb = mean(sample(mean_herb,n, replace=FALSE)))%>% 
      ungroup() 
    
    
    out$out[i]<-anova(lm(herb~cover, data = plant_sample))[1,"F value"]
  }
  return(mean(out$out))
}
