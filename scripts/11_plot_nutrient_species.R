
## packages
library(tidyverse)
library(ggradar)

## plotting args
source('scripts/theme_sleek.R')
theme_set(theme_sleek())
pcols<-c(RColorBrewer::brewer.pal(9, 'Set1'), 'black') ## 10 colors
pcols_order<-c('Fresh', 'Sun-dried', 'Smoked','Fried', 'Powder')

## get RDA reference vals
source('scripts/rda_reader.R')
rda$nutrient<-str_to_title(rda$nutrient)
rda$nutrient[rda$nutrient=='Vitamin_a']<-'Vitamin A'
rda$nutrient[rda$nutrient=='Vitamin_d']<-'Vitamin D'
rda$nutrient[rda$nutrient=='Vitamin_b12']<-'Vitamin B12'
# rda$nutrient[rda$nutrient=='Omega_3']<-'Omega3'

## get nutrient units
units<-data.frame(nutrient = c('Protein', 'Calcium', 'Iron', 'Selenium', 'Zinc','Iodine', 'Omega3', 'Vitamin A', 'Vitamin D', 'Vitamin B12', 'Folate'),
                  unit = c('percent', 'mg', 'mg', 'mcg', 'mg','mcg', 'g', 'mcg', 'mcg', 'mcg', 'mcg'))

## load data
nut_dry_whole<-read.csv('data/clean/dried_nutrient_estimates_long.csv') 
nuts<-c('calcium', 'iron', 'selenium', 'zinc', 'iodine', 'vitamin_a1', 'vitamin_d3','folate', 'vitamin_b12')
## tidy names
nutl<-nut_dry_whole %>% 
    filter(nutrient %in% nuts) %>%
    mutate(nutrient = str_to_title(nutrient)) %>% 
    rename(species = latin_name, fbname = local_name, form = type, mu = value) %>% 
    mutate(nutrient = fct_relevel(nutrient, c('Calcium', 'Iron', 'Selenium', 'Zinc','Iodine', 
                                              #'Omega3', 
                                              'Vitamin_a1', 'Vitamin_b12', 'Vitamin_d3', 'Folate'))) %>%
    mutate(nutrient = recode(nutrient, #Omega3 = 'Omega-3\nfatty acids', 
                             Vitamin_a1 = 'Vitamin A', Vitamin_b12 = 'Vitamin B12', Vitamin_d3 = 'Vitamin D')) %>% 
    mutate(form = recode(form, Wet = 'Fresh', 'Fresh, gutted' = 'Fresh')) %>% 
    mutate(fbname = ifelse(species == 'Encrasicholina punctifer', 'Omena (marine)', fbname),
           fbname = ifelse(species == 'Rastrineobola argenteus', 'Omena (freshwater)', fbname))



## units in labels
nutl$lab<-nutl$nutrient
levels(nutl$lab)<-c("'Calcium, mg'", "'Iron, mg'", expression('Selenium, '*mu*'g'),
                    "'Zinc, mg'",expression('Iodine, '*mu*'g'),# "'Omega-3, g'", 
                    expression('Vitamin A, '*mu*'g'),expression('Vitamin B12, '*mu*'g'),
                    expression('Vitamin D, '*mu*'g'),expression('Folate, '*mu*'g'))

## correct for wet weight
## but don't have the water content of dried samples
wet<-nutl %>% filter(form == 'Fresh') %>% 
    group_by(fbname, species) %>% 
    summarise(dry_weight = mean(dry_matter_g_100g_ww))

nutl$fresh_wet_ref<-wet$dry_weight[match(nutl$fbname, wet$fbname)]
# use lake victoria omena for wet weight ref
nutl$fresh_wet_ref[nutl$fbname == 'Omena (marine)']<-wet$dry_weight[wet$fbname == 'Omena (freshwater)']
## use tilapia dataset wet weight ref for tilapia


nutl$value_ww<-nutl$mu * (100/nutl$dry_matter_g_100g_ww)

10 dm
5 mu

10*x = 100 ww
x = 100 / 10


nutl_agg<-nutl %>% 
    group_by(species, fbname, nutrient, lab) %>% 
    mutate(n = length(mu)) %>% 
    group_by(form, species, fbname, n, nutrient, lab) %>% 
    summarise(mu = median(mu)) %>% 
    ungroup() %>% droplevels() %>% 
    ## add RDA and units
    left_join(rda) %>% 
    left_join(units) %>% 
    mutate(rni_women = mu/rni_women*100,
           rni_kids = mu/rni_kids*100,
           rni_men = mu/rni_men*100,
           rni_pregnant = mu/rni_pregnant*100)