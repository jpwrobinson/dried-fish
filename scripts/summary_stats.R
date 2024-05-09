library(tidyverse)
source('scripts/functions.R')

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


## country pouplations
library(WDI)

pop.df <- getWDItoSYB(name = "total_population", indicator = "SP.POP.TOTL")$entity %>%
    clean_names() %>%
    filter(country == countries) 

pop.df %>% group_by(country) %>% slice_max(year, n =3)


## model summaries

# marine / inland
mod_dat %>% 
    data_grid(Sproximity_to_water_km = 0,
              Sproximity_to_city_mins = 0,
              Swealth = 0,
              nearest_water=levels(mod_dat$nearest_water),
              Sn_hh = 0) %>%  
    add_epred_draws(m2, ndraws = 100, re_formula = NA) %>% 
    group_by(nearest_water) %>% 
    reframe(m = median(.epred), lo = HPDI(.epred, .95)[1], hi = HPDI(.epred, .95)[2])

# prox to city
mod_dat %>%  
    data_grid(Sproximity_to_water_km = 0,
              Sproximity_to_city_mins = seq_range(Sproximity_to_city_mins, n = 100),
              Swealth = 0,
              nearest_water=levels(mod_dat$nearest_water),
              Sn_hh = 0) %>%  
    mutate(proximity_to_city_mins = rep(seq_range(mod_dat$proximity_to_city_mins, n = 100), each=2)) %>% 
    add_epred_draws(m2, ndraws = 100, re_formula = NA) %>%  
    group_by(proximity_to_city_mins, nearest_water) %>% 
    reframe(m = median(.epred), lo = HPDI(.epred, .95)[1], hi = HPDI(.epred, .95)[2]) %>% 
    slice_min_max(proximity_to_city_mins)

m2 %>% 
    emmeans(~ nearest_water,
            at = list(Sproximity_to_city_mins = 0),
            epred = TRUE)

m2 %>% 
    emmeans(~ Sn_hh,
            epred = TRUE)
