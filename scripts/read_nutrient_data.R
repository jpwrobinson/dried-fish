library(tidyverse)
library(janitor)
library(readxl)

## This reads in two files containing sample metadat for Ghana/Kenya (1) and LEC samples (2)
metat_dry<-function(path){
    ## 1. Dried fish size, species, source
    # rm(metat)
    metat<-read.csv(path) %>% 
    		mutate(sample_id = ifelse(location %in% c('Kisumu', 'Accra', 'Lake Volta', 'Mombasa'), 
    		                          str_replace_all(sample_id, '_A|_B|_C|_D|_E', ''), sample_id)
    		       ) %>%
    		distinct(sample_id, date, location, type, days_processed, catch_source, local_name, latin_name) %>% 
            rename(form = type) %>% 
            mutate(form = recode(form, Wet = 'Fresh')) %>% 
    		filter(sample_id != 'K_005')
}
## clean norway nutrient estimates
# path<-'data/norway_sep22/2022-734 downloaded 06.01.23.xlsx'
# filesave<-'dried_nutrient_estimates'
# source('scripts/norway_clean.R')


## 2. read tilapia metadata
# rm(metat)
# metat<-read.csv('data/sample_metadata/Kenya_Ghana_fish_nutrients - DATA_TILAPIA.csv') %>% 
# 		mutate(local_name = 'tilapia', latin_name = 'Oreochromis niloticus', form='Fresh') %>% 
#         rename(location = 'sample_location') %>% 
# 		select(sample_id, date, location, type, total_length_mm, mass_g, local_name, latin_name) 
# 
# ## clean tilapia estimates
# path<-'data/norway_sep22/2022-528 tilapia downloaded 06.01.23.xlsx'
# filesave<-'tilapia_nutrient_estimates'
# source('scripts/norway_clean.R')
