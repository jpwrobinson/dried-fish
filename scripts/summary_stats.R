library(tidyverse)

## Dried nutrient sample metadata
dried<-read.csv('data/clean/dried_nutrient_estimates_long.csv')

tab<-dried %>% group_by(local_name, latin_name, type) %>% 
    summarise(n_samples = n_distinct(sample_id), 
              sample_locations = unique(location))

tab %>% group_by(type) %>% summarise(N = sum(n_samples))
dried %>% group_by(location) %>% summarise(N = n_distinct(sample_id))
dried %>% group_by(local_name, latin_name) %>% summarise(N = n_distinct(sample_id))

# cadmium
avgs<-dried %>% group_by(nutrient, type, unit) %>% summarise(mu = mean(value))
avgs %>% filter(nutrient=='cadmium') ## 0.8 mug / 100g for wet weight in Sroy et al. 2023. Corresponds to 2.49 mug in this dataset.


## epa + dha
dried %>% filter(nutrient %in% c('epa', 'dha')) %>% group_by(form, nutrient, unit) %>% 
    reframe(n_samples = n_distinct(sample_id), min=min(value),max=max(value), mean=mean(value)) %>% 
    filter(form %in% c('Powder', 'Smoked', 'Sun-dried')) %>% 
    select(form, nutrient, min:mean, unit, n_samples)
