library(tidyverse)
library(janitor)
library(readxl)


## Read dried Ghana + Kenya samples Jan/Feb 2022

## Dried fish size, species, source
dried<-read.csv('data/sample_metadata/Kenya_Ghana_fish_nutrients - DATA_DRIED.csv') %>% 
		mutate(sample_id = str_replace_all(sample_id, '_A|_B|_C|_D|_E', '')) %>% 
		distinct(sample_id, date, location, type, days_processed, catch_source, local_name, latin_name) %>% 
		filter(sample_id != 'K_005')

## Norway nutrient estimates
path<-'data/norway_sep22/2022-734 LIMS downloaded 01.09.22 modified by mk 01.09.22.xlsx'

## combine mineral + vitamin sheets
sheets<-c(1,3,4,5,6,7,9,12)
for(i in sheets){
	print(paste('Reading sheet', sheets[i]))
	df<-read_excel(path, sheet = i) %>% clean_names() 
	if(i == 1){dat <- df} 
	if(i != 1){dat<-dat %>% left_join(df)}
	dat<-dat %>% left_join(df)
}

dat<-dat %>% select(-c(project, customer, jnr_analysis_replicate, batch, 
	product, subproduct, project_comments, sample_comments, variation, test_comments, test_status, reviewed_by)) %>% 
		mutate(customer_marking = recode(customer_marking, A_001 = 'A_005')) %>% 
		rename(sample_id = customer_marking)

dried<-dried %>% left_join(dat)

write.csv(dried, file = 'data/clean/dried_nutrient_estimates_wide.csv')

## long version with trace values removed
datl<-dried %>% mutate_if(is.numeric, as.character) %>% 
		pivot_longer(-c(sample_id:latin_name), values_to = 'value', names_to = 'nutrient') %>% 
		mutate(unit = ifelse(str_detect(nutrient, 'dry|protein|torrst'),'g_100g',
			ifelse(str_detect(nutrient, 'folat'), 'mg_100g', 'mg_kg')),
				nutrient = str_replace_all(nutrient, '_mg_kg_dw', ''),
				nutrient = str_replace_all(nutrient, '_mg_kg_ww', ''),
				nutrient = str_replace_all(nutrient, '_g_100g_ww', ''),
				nutrient = str_replace_all(nutrient, '_mg_100_g_ww', ''),
				nutrient = str_replace_all(nutrient, '_mg_kg', ''),
				nutrient = str_replace_all(nutrient, '_percent_g_100g', '')) %>% 
		mutate(value = ifelse(str_detect(value, '<'), NA, as.numeric(value)),
			nutrient = recode(nutrient, v = 'vanadium', cr = 'chromium', mn = 'manganese',
				fe = 'iron', co = 'cobalt', ni = 'nickel', cu = 'copper', zn = 'zinc',
				as = 'arsenic', se = 'selenium', mo = 'molybdenum', ag = 'silver', cd = 'cadmium',
				hg = 'mercury', pb = 'lead', jod = 'iodine', ca = 'calcium', na = 'sodium', k = 'potassium', mg = 'magnesium',
				p = 'phosphorus', folat = 'folate'))

write.csv(datl, file = 'data/clean/dried_nutrient_estimates_long.csv')


## read tilapia
tilap<-read.csv('data/sample_metadata/Kenya_Ghana_fish_nutrients - DATA_TILAPIA.csv') %>% 
		mutate(local_name = 'tilapia', scientific_name = 'Oreochromis niloticus') %>% 
		select(sample_id, date, sample_location, type, total_length_mm, mass_g) 

## Norway nutrient estimates
path<-'data/norway_sep22/2022-528 tilapia LIMS downloaded 01.09.22, modified by mk 01.09.22.xlsx'

## combine mineral + vitamin sheets
sheets<-c(1,3,4,5,6,10)
for(i in sheets){
	print(paste('Reading sheet', sheets[i]))
	df<-read_excel(path, sheet = i) %>% clean_names()  %>% select(-jnr_analysis_replicate)
	if(i == 1){dat <- df} 
	if(i != 1){dat<-dat %>% left_join(df)}
}

dat<-dat %>% select(-c(project, customer, batch, product)) %>% 
		rename(sample_id = customer_marking)

tilap<-tilap %>% left_join(dat)

write.csv(tilap, file = 'data/clean/tilapia_nutrient_estimates_wide.csv')

# ## long version with trace values removed
datl<-tilap %>% mutate_if(is.numeric, as.character) %>% 
		pivot_longer(-c(sample_id:mass_g), values_to = 'value', names_to = 'nutrient') %>% 
		mutate(unit = ifelse(str_detect(nutrient, 'protein'),'g_100g', 'mg_kg'),
				nutrient = str_replace_all(nutrient, '_mg_kg_ww', ''),
				nutrient = str_replace_all(nutrient, '_g_100g_ww', ''),
				nutrient = str_replace_all(nutrient, '_mg_kg', '')) %>% 
		mutate(value = ifelse(str_detect(value, '<'), NA, as.numeric(value)),
			nutrient = recode(nutrient, v = 'vanadium', cr = 'chromium', mn = 'manganese',
				fe = 'iron', co = 'cobalt', ni = 'nickel', cu = 'copper', zn = 'zinc',
				as = 'arsenic', se = 'selenium', mo = 'molybdenum', ag = 'silver', cd = 'cadmium',
				hg = 'mercury', pb = 'lead', jod = 'iodine', ca = 'calcium', na = 'sodium', k = 'potassium', mg = 'magnesium',
				p = 'phosphorus', folat = 'folate'))

write.csv(datl, file = 'data/clean/dried_nutrient_estimates_long.csv')