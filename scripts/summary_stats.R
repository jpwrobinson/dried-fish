library(tidyverse)
source('scripts/functions.R')

nuts<-c('calcium', 'iron', 'selenium', 'zinc', 'iodine','epa_dha', 'vitamin_a1', 'vitamin_d3', 'vitamin_b12')
cons<-c('lead', 'mercury', 'cadmium')

## Dried nutrient sample metadata
dried<-read.csv('data/clean/dried_nutrient_estimates_long.csv')

tab<-dried %>% group_by(local_name, latin_name, form) %>% 
    summarise(n_samples = n_distinct(sample_id), 
              sample_locations = unique(location))

tab %>% group_by(form) %>% summarise(N = sum(n_samples))
dried %>% group_by(location) %>% summarise(N = n_distinct(sample_id))
dried %>% group_by(local_name, latin_name) %>% summarise(N = n_distinct(sample_id))

# cadmium
avgs<-dried %>% group_by(nutrient, form, unit) %>% summarise(mu = mean(value))
avgs %>% filter(nutrient=='cadmium') ## 0.8 mug / 100g for wet weight in Sroy et al. 2023. Corresponds to 2.49 mug in this dataset.

# check variation in contaminants

dried %>% 
    filter(nutrient %in% c(nuts,cons)) %>% 
    mutate(nutrient = str_to_title(nutrient)) %>% 
    mutate(form = recode(form, Wet = 'Fresh', 'Fresh, gutted' = 'Fresh')) %>% 
    mutate(id = ifelse(form == 'Fresh', 'fresh', 'dried')) %>% 
    group_by(nutrient, id) %>% 
    summarise(var = cv(value)) %>% 
    filter(!is.na(var)) %>% 
    ggplot(aes(fct_reorder(nutrient, var), var, fill=id)) + 
    geom_col(position = position_dodge(width=0.9)) + 
    coord_flip() +
    labs(y = 'Coefficient of Variation', x = '') +
    scale_y_continuous(expand=c(0,0))

# contam limits %
contam<-dried %>% 
    filter(nutrient %in% cons) %>% 
    mutate(nutrient = str_to_title(nutrient)) %>% 
    mutate(form = recode(form, Wet = 'Fresh', 'Fresh, gutted' = 'Fresh')) %>% 
    group_by(form, latin_name, nutrient) %>% 
    summarise(mu = median(value, na.rm=TRUE)) %>% 
    ungroup() %>% droplevels() %>% 
    ## add RDA and units
    left_join(cont %>% mutate(nutrient = str_to_title(nutrient))) %>% 
    mutate(exposure = mu  / limit_100g * 100 * (portionK/100),
           nportions = limit_100g / (mu * portionK/100),
           lab2 = paste(latin_name, form))

contam %>% 
    group_by(nutrient, form) %>% 
    summarise(exposure = mean(exposure), nportions = mean(nportions))

contam %>% ungroup() %>% slice_max(exposure, n =10) %>% select(nutrient, exposure, lab2)


## epa + dha
dried %>% filter(nutrient %in% c('epa', 'dha')) %>% group_by(form, nutrient, unit) %>% 
    reframe(n_samples = n_distinct(sample_id), min=min(value),max=max(value), mean=mean(value)) %>% 
    filter(form %in% c('Powder', 'Smoked', 'Sun-dried')) %>% 
    select(form, nutrient, min:mean, unit, n_samples)

# moisture content
dried %>% 
    distinct(dry_matter_g_100g, local_name, latin_name, form) %>% 
    group_by(form) %>% 
    reframe(mean = 100-mean (dry_matter_g_100g), min = 100-min(dry_matter_g_100g), max = 100-max(dry_matter_g_100g))

## average water content in fresh samples is 76%

# to estimate diff between fresh + dried 
tar_load(figPortionDat)

figPortionDat %>% 
    filter(rni > 0.15) %>% 
    group_by(nutrient, form2) %>% 
    slice_min(rni) %>% 
    pivot_wider(-rni, names_from = 'form2', values_from = 'portion') %>% 
    mutate(diff = Fresh - Dried, inc = diff / Dried * 100,
           type = ifelse(nutrient %in% c('Calcium', 'Iodine', "Iron", 'Selenium', 'Zinc'), 'mineral', 'other'),
            type = ifelse(str_detect(nutrient, 'Vit*'), 'vitamin', type)) %>% 
    group_by(type) %>% 
    summarise(dried = mean(Dried), fresh = mean(Fresh), diff = mean(diff))


## summary stats - LSMS
lsms_hh<-read.csv(file = 'data/lsms_subset/lsms_all_hh.csv')
lsms_fish<-read.csv(file = 'data/lsms_subset/lsms_fish.csv')
lsms_all<-read.csv(file = 'data/lsms_subset/lsms_for_mod.csv')

lsms_hh %>% group_by(country, tot_hh) %>% 
    filter(!is.na(n_hh)) %>% 
    summarise(n_hh = mean(n_hh), n_adult = mean(n_adult), n_children = mean(n_children))

lsms_fish %>% group_by(country, tot_hh) %>% 
    summarise(n_processed = n_distinct(hh_id[form %in% c('dried', 'smoked', 'dry/smoked')]),
              n_dried = n_distinct(hh_id[form %in% c('dried')]),
              n_smoked = n_distinct(hh_id[form %in% c('smoked')]),
              n_fish = n_distinct(hh_id)) %>% 
    mutate(prop_dried_pop = n_processed / tot_hh,
           prop_fish_pop = n_fish / tot_hh,
           prop_processed_of_fish = n_processed / n_fish,
           prop_smoked_of_processed = n_smoked / n_processed,
           prop_dried_of_processed = n_dried / n_processed
    )

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
