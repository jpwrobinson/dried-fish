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

# NRV values
tar_load(figPortionDat)
figPortionDat %>% mutate(rni = rni*100) %>% filter(portion == portionK)

tar_load(figPortionDat_Women)
figPortionDat_Women %>% mutate(rni = rni*100) %>% filter(portion == portionW)

# portion source values
figPortionDat %>% group_by(nutrient) %>% filter(rni>= 0.15) %>% summarise(min(portion))

# degradation stats
tar_load(figContrast)
figContrast$data

figContrast$data %>% filter(nutrient %in% c('Calcium', 'Selenium', 'Zinc', 'Iodine')) %>% mutate(meaner*100)
figContrast$data %>% filter(nutrient %in% c('Cadmium', 'Iron', 'Lead')) %>% mutate(meaner*100)

## summary stats - LSMS
lsms_hh<-read.csv(file = 'data/lsms_subset/lsms_all_hh.csv')
lsms_fish<-read.csv(file = 'data/lsms_subset/lsms_fish.csv')
lsms_all<-read.csv(file = 'data/lsms_subset/lsms_for_mod.csv')

lsms_all %>% summarise(n_distinct(hh_id))

lsms_hh %>% group_by(country, tot_hh) %>% 
    filter(!is.na(n_hh)) %>% 
    summarise(n_hh = mean(n_hh), n_adult = mean(n_adult), n_children = mean(n_children))


# prop fish 
mod_dat %>% group_by(country) %>% 
    mutate(N = length(hh_id)) %>% 
    summarise(dried = sum(response_dried)/unique(N)*100,
              fresh = sum(response_fresh)/unique(N)*100)

## country pouplations
library(WDI)

pop.df <- getWDI(name = "total_population", indicator = "SP.POP.TOTL") %>% 
    clean_names() %>%
    filter(country %in% countries & !(is.na(total_population)))

adult_prop<-getWDI(indicator = 'SP.POP.0014.TO.ZS') %>% 
    clean_names() %>%
    filter(country %in% countries & year == 2023) %>% 
    group_by(country) %>% 
    ungroup() %>% 
    mutate(country = countrycode(iso2_wb_code, 'iso2c', 'iso3c'))

pops<-pop.df %>% group_by(country) %>% slice_max(year, n =1) %>% 
    mutate(country = countrycode(iso2_wb_code, 'iso2c', 'iso3c')) %>% 
    left_join(adult_prop %>% select(iso2_wb_code, sp_pop_0014_to_zs)) %>% 
    mutate(adult_pop = (100-sp_pop_0014_to_zs)/100 * total_population)


## model summaries
# https://www.andrewheiss.com/blog/2021/11/10/ame-bayes-re-guide/#average-marginal-effects
load(file = 'data/mod/lsms_mod.rds')
load(file = 'data/mod/lsms_mod_fresh.rds')

# DRIED FISH #
## country level probabilities, based on average covariate values per country
pop_prob<-mod_dat %>% 
    group_by(country) %>% 
    summarise(Sproximity_to_marine_km = median(Sproximity_to_marine_km),
              Sproximity_to_inland_km = median(Sproximity_to_inland_km),
              Sproximity_to_city_mins = median(Sproximity_to_city_mins),
              Swealth_ppp = median(Swealth_ppp),
              urban_rural = 'Urban',
              Sn_hh = median(Sn_hh)) %>%  
    add_epred_draws(m2, ndraws = 100, re_formula = ~ (1 | country)) %>% 
    reframe(m = median(.epred), lo = HPDI(.epred, .95)[1], hi = HPDI(.epred, .95)[2]) %>% 
    left_join(pops, by = 'country') %>% 
    mutate(m_pop = m*total_population, lo_pop = lo*total_population, hi_pop = hi*total_population) %>% 
    select(country, m:hi, total_population:hi_pop)

# 144.4 million [132.2, 155.0]
sum(pop_prob$lo_pop)/1e6
sum(pop_prob$m_pop)/1e6
sum(pop_prob$hi_pop)/1e6

# proportion of focal population [34.9%, 32.0 - 37.5%]
sum(pop_prob$lo_pop) / sum(pop_prob$total_population) * 100
sum(pop_prob$m_pop) / sum(pop_prob$total_population) * 100
sum(pop_prob$hi_pop) / sum(pop_prob$total_population) * 100

# FRESH FISH #
## country level probabilities, based on average covariate values per country
pop_prob2<-mod_dat %>% 
    group_by(country) %>% 
    summarise(Sproximity_to_marine_km = median(Sproximity_to_marine_km),
              Sproximity_to_inland_km = median(Sproximity_to_inland_km),
              Sproximity_to_city_mins = median(Sproximity_to_city_mins),
              Swealth_ppp = median(Swealth_ppp),
              urban_rural = 'Urban',
              Sn_hh = median(Sn_hh)) %>%  
    add_epred_draws(m3, ndraws = 100, re_formula = ~ (1 | country)) %>% 
    reframe(m = median(.epred), lo = HPDI(.epred, .95)[1], hi = HPDI(.epred, .95)[2]) %>% 
    left_join(pops, by = 'country') %>% 
    mutate(m_pop = m*total_population, lo_pop = lo*total_population, hi_pop = hi*total_population) %>% 
    select(country, m:hi, total_population:hi_pop)

# 94.0 million [84.8 - 103.4]
sum(pop_prob2$m_pop)/1e6
sum(pop_prob2$hi_pop)/1e6
sum(pop_prob2$lo_pop)/1e6

# proportion of focal population 22.7% [20.5, 25.0]
sum(pop_prob2$m_pop) / sum(pop_prob2$total_population) * 100
sum(pop_prob2$lo_pop) / sum(pop_prob2$total_population) * 100
sum(pop_prob2$hi_pop) / sum(pop_prob2$total_population) * 100

# ratio
144.4 / 94.0
# percent
(144.4 - 94) / 94 * 100

# Within 20km of marine / inland
marine<-mod_dat$Sproximity_to_marine_km[mod_dat$distance_to_marine<20]
inland<-mod_dat$Sproximity_to_inland_km[mod_dat$distance_to_inland<20]

mod_dat %>% 
    data_grid(Sproximity_to_marine_km = seq_range(marine,  n = 100),
              Sproximity_to_inland_km = seq_range(inland, n = 100),
              Sproximity_to_city_mins = 0,
              urban_rural = 'Urban',
              Swealth_ppp = 0,
              Sn_hh = 0) %>%  
    add_epred_draws(m2, ndraws = 100, re_formula = NA) %>% ungroup() %>% 
    summarise(m = median(.epred), lo = HPDI(.epred, .95)[1], hi = HPDI(.epred, .95)[2]) %>% 
    select(m,lo,hi)

mod_dat %>% 
    data_grid(Sproximity_to_marine_km = seq_range(marine,  n = 100),
              Sproximity_to_inland_km = seq_range(inland, n = 100),
              Sproximity_to_city_mins = 0,
              urban_rural = 'Urban',
              Swealth_ppp = 0,
              Sn_hh = 0) %>%  
    add_epred_draws(m3, ndraws = 100, re_formula = NA) %>% ungroup() %>% 
    summarise(m = median(.epred), lo = HPDI(.epred, .95)[1], hi = HPDI(.epred, .95)[2]) %>% 
    select(m,lo,hi)

# Dried =  58% [40-74%]
# Fresh =  31% [8-68%]


## households in interaction hotspots
mod_dat %>% filter(distance_to_marine > 1000 & distance_to_inland < 10) %>% summarise(n_distinct(hh_id)) # n = 111, UGA, some TZA
111/dim(mod_dat)[1]*100

mod_dat %>% filter(distance_to_marine < 10 & distance_to_inland > 280) %>% summarise(n_distinct(hh_id)) # n = 42, NGA, some TZA
42/dim(mod_dat)[1]*100
mod_dat %>% filter(distance_to_marine < 10 & distance_to_inland > 280) %>% data.frame

mod_dat %>% filter(distance_to_marine > 600) %>% data.frame %>% distinct(country)

mod_dat %>% filter(distance_to_marine < 20 | distance_to_inland < 20) %>% summarise(n_distinct(hh_id)) # n = 17577
9088/dim(mod_dat)[1]*100

# prox to city
mod_dat %>%  
    data_grid(Sproximity_to_marine_km = 0,
              Sproximity_to_inland_km = 0,
              Sproximity_to_city_mins = seq_range(Sproximity_to_city_mins, n = 100),
              Swealth_ppp = 0,
              Sn_hh = 0) %>%  
    mutate(proximity_to_city_mins = rep(seq_range(mod_dat$proximity_to_city_mins, n = 100), each=2)) %>% 
    add_epred_draws(m2, ndraws = 100, re_formula = NA) %>%  
    group_by(proximity_to_city_mins, nearest_water) %>% 
    reframe(m = median(.epred), lo = HPDI(.epred, .95)[1], hi = HPDI(.epred, .95)[2]) %>% 
    slice_min_max(proximity_to_city_mins)


# Fresh - near marine only
m2 %>% 
    emmeans(~ 1,
            at = list(Sproximity_to_city_mins = 0, 
                      Swealth_ppp = 0, 
                      Sn_hh = 0, 
                      Sproximity_to_inland_km = 0,
                      Sproximity_to_marine_km = min(mod_dat$Sproximity_to_marine_km)),
            epred = TRUE)

# Fresh - distant from marine, near inland
m3 %>% 
    emmeans(~ 1,
            at = list(Sproximity_to_city_mins = 0, 
                      Swealth_ppp = 0, 
                      Sn_hh = 0, 
                      Sproximity_to_inland_km = min(mod_dat$Sproximity_to_inland_km),
                      Sproximity_to_marine_km = max(mod_dat$Sproximity_to_marine_km)),
            epred = TRUE)

# Fresh - near marine, distant from inland
m3 %>% 
    emmeans(~ 1,
            at = list(Sproximity_to_city_mins = 0, 
                      Swealth_ppp = 0, 
                      Sn_hh = 0, 
                      Sproximity_to_inland_km = max(mod_dat$Sproximity_to_inland_km),
                      Sproximity_to_marine_km = min(mod_dat$Sproximity_to_marine_km)),
            epred = TRUE)

## wealth effects
m2 %>% emmeans(~ Swealth_ppp, var = 'Swealth_ppp', 
                at = list(Swealth_ppp = c(min(mod_dat$Swealth_ppp), max(mod_dat$Swealth_ppp))), epred =TRUE) 

m3 %>% emmeans(~ Swealth_ppp, var = 'Swealth_ppp', 
               at = list(Swealth_ppp = c(min(mod_dat$Swealth_ppp), max(mod_dat$Swealth_ppp))), epred =TRUE) 


## distance water effects in the band
m2 %>% emmeans(~ Swealth_ppp, var = 'Swealth_ppp', 
               at = list(Swealth_ppp = c(min(mod_dat$Swealth_ppp), max(mod_dat$Swealth_ppp))), epred =TRUE) 

m3 %>% emmeans(~ Swealth_ppp, var = 'Swealth_ppp', 
               at = list(Swealth_ppp = c(min(mod_dat$Swealth_ppp), max(mod_dat$Swealth_ppp))), epred =TRUE) 

