pacman::p_load(funk, tidyverse, cowplot, janitor, readxl)
theme_set(theme_sleek())


# focs<-c('Salmo salar', 'Gadus morhua', 'Katsuwonus pelamis', 'Sardinella aurita')

dat<-clean_names(read.csv('data/raw/BioFoodComp4.0_fish.csv')) %>% 
			select(country_region, type, food_name_in_english, processing, scientific_name, asfis_english_name, water_g) %>%
			filter(! c(is.na(water_g) & water_g == ''))

## identify species with at least 2 processing types
procs<-aggregate(processing ~ scientific_name, dat, uniques) %>% filter(processing >= 2)

dat<-dat %>% filter(scientific_name %in% procs$scientific_name) %>%
			filter(!grepl('sp.', scientific_name))


## read dried fish
dry<-clean_names(read.csv('data/all_nutrients_active.csv')) %>%  
			filter(prep_form == 'dry')

dim(dry) ## 70 samples
dry %>% distinct(x_species) ## 21 species


## focal species
focs<-c('Limnothrissa miodon','Stolothrissa tanganicae','Rastrineobola argentea',
	'Sierrathrissa leonensis',
	'Engraulis encrasicolus','Sardinella aurita','Sardinella maderensis',
	'Sardina pilchardus', 'Oreochromis aureus')
focs_<-focs %>% str_replace_all('\\ ', '_')

focD<-clean_names(read_excel('data/all_nutrients_active.xlsx')) %>%  
			filter(species %in% focs_)

dim(focD)
focD %>% group_by(species, nutrient) %>% summarise(n_cite = n_distinct(citation), n_sample = length(value), val = mean(value))
unique(focD$citation) ## n = 10


focD %>% group_by(species, nutrient, prep_form) %>% summarise(n_cite = n_distinct(citation), n_sample = length(value), val = mean(value)) %>% 
		pivot_wider(names_from = 'nutrient', values_from = 'val') %>% write.csv('data/focal_fish_infoods.csv')


## check predicted values
nut<-read.csv('data/Species_Nutrient_Predictions.csv')
nut %>% filter(species %in% focs) %>% select(species, ends_with('mu')) %>% write.csv('data/focal_fish_predicted.csv')

