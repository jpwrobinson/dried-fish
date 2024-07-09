library(tidyverse)

nuts<-c('calcium', 'iron', 'selenium', 'zinc', 'iodine','epa_dha', 
        'vitamin_a1', 'vitamin_d3','folate', 'vitamin_b12')

read.csv('data/clean/dried_nutrient_estimates_long.csv') %>% 
    # filter(nutrient %in% nuts) %>% 
    mutate(
           form = recode(form, Wet = 'Fresh', 'Fresh, gutted' = 'Fresh'),
           unit = str_replace_all(unit, '_', '\\ '),
           unit = str_replace_all(unit, '100g', '100 g-1'),
           nutrient = str_to_title(nutrient),
           nutrient = recode(nutrient,  Epa_dha = 'Omega-3 (DHA + EPA)', 
                             Vitamin_a1 = 'Vitamin A1',Vitamin_a2 = 'Vitamin A2', Vitamin_b12 = 'Vitamin B12', Vitamin_d3 = 'Vitamin D'),
           nutrient = paste(nutrient, unit)) %>% 
    group_by(local_name, latin_name, catch_source,form) %>% 
    mutate(n_samples = n_distinct(sample_id), dry_matter = mean(dry_matter_g_100g)) %>% 
    group_by(local_name, latin_name, catch_source, form, n_samples, dry_matter, nutrient) %>% 
    summarise(value = mean(value)) %>% 
    pivot_wider(names_from = nutrient, values_from = value) %>% 
    arrange(latin_name) %>% 
    write.csv('tables/TableS1.csv')

