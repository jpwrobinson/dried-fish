library(tidyverse)

## Dried nutrient sample metadata
dried<-read.csv('data/clean/dried_nutrient_estimates_long.csv')

tab<-dried %>% group_by(local_name, latin_name, type) %>% 
    summarise(n_samples = n_distinct(sample_id), 
              sample_locations = unique(location))
