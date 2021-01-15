pacman::p_load(here, funk, knitr, rethinking, tidyverse, tidybayes, skimr, cowplot, janitor)
theme_set(theme_sleek())


# focs<-c('Salmo salar', 'Gadus morhua', 'Katsuwonus pelamis', 'Sardinella aurita')

dat<-clean_names(read.csv('data/raw/uFish_1.0_products.csv')) %>% 
			select(subgroup,food_name_in_english, processing, asfis_scientific_name, asfis_english_name,
				enerc_kcal, water_g, ca_mg, fe_mg, zn_mg, vita_rae_mcg, vita_re_mcg, vitd_mcg, vitb12_mcg, protcnt_g,fapun3_g) 

## identify species with at least 2 processing types
procs<-aggregate(processing ~ asfis_scientific_name, dat, uniques) %>% filter(processing >= 2)


dat<-dat %>% filter(asfis_scientific_name %in% procs$asfis_scientific_name) %>%
			filter(!grepl('sp.', asfis_scientific_name)) %>%
			# mutate(vitb12_mcg = case_when(is.na(vitb12_mcg) ~ '')) %>%
				mutate(across(enerc_kcal:fapun3_g, as.numeric)) %>%
			pivot_longer(enerc_kcal:fapun3_g, names_to = 'nutrient', values_to = 'value') %>%
			mutate(value = case_when(is.na(value) ~ '')) 


pdf(file = 'figures/explore/uFish_processing_types.pdf', height=7, width=14)
ggplot(dat, aes(asfis_scientific_name, value, col = processing)) + 
			geom_point() + 
			# geom_point(position = position_dodge(width=0.5)) + 
			facet_wrap(~nutrient, scales='free_x', nrow=1) + 
			coord_flip()
dev.off()