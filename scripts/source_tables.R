library(tidyverse)

nuts<-c('calcium', 'iron', 'selenium', 'zinc', 'iodine','epa_dha', 
        'vitamin_a1', 'vitamin_d3','folate', 'vitamin_b12')

nut<-read.csv('data/clean/dried_nutrient_estimates_long.csv') %>% 
    # filter(nutrient %in% nuts) %>% 
    mutate(
           form = recode(form, Wet = 'Fresh', 'Fresh, gutted' = 'Fresh'),
           nutrient = str_to_title(nutrient),
           nutrient = recode(nutrient,  Epa_dha = 'Omega-3 (DHA + EPA)', 
                             Vitamin_a1 = 'Vitamin A', Vitamin_b12 = 'Vitamin B12', Vitamin_d3 = 'Vitamin D')) %>% 
    group_by(local_name, latin_name, catch_source,form) %>% 
    mutate(n_samples = n_distinct(sample_id)) %>% 
    group_by(local_name, latin_name, catch_source, form, n_samples, nutrient) %>% 
    summarise(value = mean(value)) %>% 
    pivot_wider(names_from = nutrient, values_from = value) %>% 
    write.csv('tables/TableS1.csv')

