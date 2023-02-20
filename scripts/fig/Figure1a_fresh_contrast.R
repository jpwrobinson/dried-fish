library(tidyverse)
library(cowplot)
library(ggradar)

## plotting args
source('scripts/theme_sleek.R')
theme_set(theme_sleek())
pcols<-c(RColorBrewer::brewer.pal(9, 'Set1'), 'black') ## 10 colors
pcols_order<-c('Fresh', 'Sun-dried', 'Smoked','Fried', 'Powder')
pcols_named<-c('Fresh' = pcols[1], 'Sun-dried' = pcols[2], 'Smoked' = pcols[3],'Fried' = pcols[4], 'Powder' = pcols[5])

## get nutrient units
units<-data.frame(nutrient = c('Protein', 'Calcium', 'Iron', 'Selenium', 'Zinc','Iodine', 'Omega3', 'Vitamin A', 'Vitamin D', 'Vitamin B12', 'Folate'),
                  unit = c('percent', 'mg', 'mg', 'mcg', 'mg','mcg', 'g', 'mcg', 'mcg', 'mcg', 'mcg'))

## load data
nuts<-c('calcium', 'iron', 'selenium', 'zinc', 'iodine', 'vitamin_a1', 'vitamin_d3','folate', 'vitamin_b12')
nut<-read.csv('data/clean/dried_nutrient_estimates_long.csv') %>% 
    filter(nutrient %in% nuts) %>% 
    mutate(fresh_co = ifelse(type == 'Fresh', 'fresh', 'processed')) %>% 
    select(sample_id, latin_name, nutrient, unit, fresh_co, value) %>%
    pivot_wider(names_from = 'fresh_co', values_from = 'value') %>% 
    mutate(contrast = processed - fresh) %>% 
    filter(!is.na(contrast)) %>% 
    group_by(nutrient, unit) %>% 
    summarise(value = mean(contrast))


## NOT WORKING YET