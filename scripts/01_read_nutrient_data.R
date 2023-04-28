library(tidyverse)
library(janitor)
library(readxl)

## Read dried Ghana + Kenya samples Jan/Feb 2022

## 1. Dried fish size, species, source
rm(metat)
metat<-read.csv('data/sample_metadata/Kenya_Ghana_fish_nutrients - DATA_DRIED.csv') %>% 
		mutate(sample_id = str_replace_all(sample_id, '_A|_B|_C|_D|_E', '')
		       #sample_id = recode(sample_id, 'A_005' = 'A_001')
		       ) %>%
		distinct(sample_id, date, location, type, days_processed, catch_source, local_name, latin_name) %>% 
        rename(form = type) %>% 
        mutate(form = recode(form, Wet = 'Fresh')) %>% 
		filter(sample_id != 'K_005')

## clean norway nutrient estimates
path<-'data/norway_sep22/2022-734 downloaded 06.01.23.xlsx'
filesave<-'dried_nutrient_estimates'
source('scripts/norway_clean.R')


## 2. read tilapia metadata
rm(metat)
metat<-read.csv('data/sample_metadata/Kenya_Ghana_fish_nutrients - DATA_TILAPIA.csv') %>% 
		mutate(local_name = 'tilapia', latin_name = 'Oreochromis niloticus', form='Fresh') %>% 
        rename(location = 'sample_location') %>% 
		select(sample_id, date, location, type, total_length_mm, mass_g, local_name, latin_name) 

## clean tilapia estimates
path<-'data/norway_sep22/2022-528 tilapia downloaded 06.01.23.xlsx'
filesave<-'tilapia_nutrient_estimates'
source('scripts/norway_clean.R')
