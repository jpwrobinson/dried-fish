pacman::p_load(here, funk, knitr, rethinking, tidyverse, tidybayes, skimr, cowplot, janitor)
theme_set(theme_sleek())


# focs<-c('Salmo salar', 'Gadus morhua', 'Katsuwonus pelamis', 'Sardinella aurita')

dat<-clean_names(read.csv('data/raw/BioFoodComp4.0_fish.csv')) %>% 
			select(country_region, type, food_name_in_english, processing, scientific_name, asfis_english_name, water_g) %>%
			filter(! c(is.na(water_g) & water_g == ''))

## identify species with at least 2 processing types
procs<-aggregate(processing ~ scientific_name, dat, uniques) %>% filter(processing >= 2)

dat<-dat %>% filter(scientific_name %in% procs$scientific_name) %>%
			filter(!grepl('sp.', scientific_name))

