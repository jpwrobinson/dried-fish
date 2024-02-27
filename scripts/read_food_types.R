pacman::p_load(here, funk, knitr, rethinking, tidyverse, tidybayes, skimr, cowplot, janitor)
theme_set(theme_sleek())

## ufish dataset
dat<-clean_names(read.csv('data/raw/uFish_1.0_products.csv')) %>% 
			select(subgroup,food_name_in_english, processing, asfis_scientific_name, asfis_english_name,
				enerc_kcal, ca_mg, fe_mg, zn_mg, vita_rae_mcg, vita_re_mcg, vitd_mcg, vitb12_mcg, protcnt_g,fapun3_g) 

## identify species with at least 2 processing types
procs<-aggregate(processing ~ asfis_scientific_name, dat, uniques) %>% filter(processing >= 2)


dat<-dat %>% filter(asfis_scientific_name %in% procs$asfis_scientific_name) %>%
			filter(!grepl('sp.', asfis_scientific_name)) %>%
			# mutate(vitb12_mcg = case_when(is.na(vitb12_mcg) ~ '')) %>%
				mutate(across(enerc_kcal:fapun3_g, as.numeric)) %>%
			pivot_longer(enerc_kcal:fapun3_g, names_to = 'nutrient', values_to = 'value') 


pdf(file = 'figures/explore/uFish_processing_types.pdf', height=20, width=14)
ggplot(dat, aes(asfis_scientific_name, value, col = processing)) + 
			geom_point() + 
			# geom_point(position = position_dodge(width=0.5)) + 
			facet_wrap(~nutrient, scales='free_x', nrow=1) + 
			coord_flip()
dev.off()


## first focal list

sp<-data.frame(scientific = c('Gadus morhua', 'Thunnus albacares', 'Salmo salar', 'Rastrelliger kanagurta', 
	'Engraulis encrasicolus', 'Cyprinus carpio', 'Katsuwonus pelamis', 'Oreochromis niloticus'),
common = c('Atlantic cod',  'Tuna (albacore)', 'Atlantic salmon', 'Indian mackerel', 
	'European anchovy', 'Common carp', 'Tuna (skipjack)', 'Tilapia (Nile)'))

dat<-dat %>% filter(asfis_scientific_name %in% sp$scientific)

pdf(file = 'figures/explore/uFish_processing_types_focal.pdf', height=8, width=12)
ggplot(dat, aes(asfis_scientific_name, value, col = processing)) + 
			geom_point(position = position_dodge(width=0.4)) + 
			# geom_point(position = position_dodge(width=0.5)) + 
			facet_wrap(~nutrient, scales='free_x', nrow=1) + 
			coord_flip()

dat %>% group_by(asfis_scientific_name, processing, nutrient) %>% summarise(se = funk::se(value), value = mean(value)) %>%
			mutate(lower  = value - 2*se, upper = value + 2*se) %>% 
			ggplot(aes(asfis_scientific_name, value, col = processing, ymin = lower, ymax = upper)) + 
			geom_pointrange(position = position_dodge(width=0.4)) + 
			# geom_point(position = position_dodge(width=0.5)) + 
			facet_wrap(~nutrient, scales='free_x', nrow=1) + 
			coord_flip()
dev.off()




# ## biocomp dataset
# dat<-clean_names(read.csv('data/raw/BioFoodComp4.0_fish.csv')) %>% 
# 			select(subgroup,food_name_in_english, processing, asfis_scientific_name, asfis_english_name,
# 				enera_kcal, ca_mg, fe_mg, zn_mg,  protcnt_g) 

# ## identify species with at least 2 processing types
# procs<-aggregate(processing ~ asfis_scientific_name, dat, uniques) %>% filter(processing >= 2)


# dat<-dat %>% filter(asfis_scientific_name %in% procs$asfis_scientific_name) %>%
# 			filter(!grepl('sp.', asfis_scientific_name)) %>%
# 			# mutate(vitb12_mcg = case_when(is.na(vitb12_mcg) ~ '')) %>%
# 				mutate(across(enera_kcal:protcnt_g, as.numeric)) %>%
# 			pivot_longer(enera_kcal:protcnt_g, names_to = 'nutrient', values_to = 'value') 


# pdf(file = 'figures/explore/biocomp_processing_types.pdf', height=20, width=14)
# ggplot(dat, aes(asfis_scientific_name, value, col = processing)) + 
# 			geom_point() + 
# 			# geom_point(position = position_dodge(width=0.5)) + 
# 			facet_wrap(~nutrient, scales='free_x', nrow=1) + 
# 			coord_flip()
# dev.off()


# ## first focal list

# sp<-data.frame(scientific = c('Gadus morhua', 'Thunnus albacares', 'Salmo salar', 'Rastrelliger kanagurta', 
# 	'Engraulis encrasicolus', 'Cyprinus carpio', 'Katsuwonus pelamis', 'Oreochromis niloticus'),
# common = c('Atlantic cod',  'Tuna (albacore)', 'Atlantic salmon', 'Indian mackerel', 
# 	'European anchovy', 'Common carp', 'Tuna (skipjack)', 'Tilapia (Nile)'))

# dat<-dat %>% filter(asfis_scientific_name %in% sp$scientific)

# pdf(file = 'figures/explore/biocomp_processing_types_focal.pdf', height=8, width=12)
# ggplot(dat, aes(asfis_scientific_name, value, col = processing)) + 
# 			geom_point(position = position_dodge(width=0.4)) + 
# 			# geom_point(position = position_dodge(width=0.5)) + 
# 			facet_wrap(~nutrient, scales='free_x', nrow=1) + 
# 			coord_flip()

# dat %>% group_by(asfis_scientific_name, processing, nutrient) %>% summarise(se = funk::se(value), value = mean(value)) %>%
# 			mutate(lower  = value - 2*se, upper = value + 2*se) %>% 
# 			ggplot(aes(asfis_scientific_name, value, col = processing, ymin = lower, ymax = upper)) + 
# 			geom_pointrange(position = position_dodge(width=0.4)) + 
# 			# geom_point(position = position_dodge(width=0.5)) + 
# 			facet_wrap(~nutrient, scales='free_x', nrow=1) + 
# 			coord_flip()
# dev.off()
