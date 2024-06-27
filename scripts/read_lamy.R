

read_lamy<-function(filepath = 'data/lamy/data_sdf_Mada.RData'){
    
    load(filepath)
    
    metat<-dat<-data_taxo_sdf_Mada %>% 
        clean_names() %>% 
        mutate(latin_name = f_hypo) %>% 
        select(sample, latin_name)
    
    dat<-data_nutri_sdf_Mada %>% 
        clean_names() %>% 
        left_join(metat, by = 'sample', relationship='many-to-many') %>% 
        mutate(value = mean, form = transformation,
               sample_id = sample,
               catch_source = type,
               location = 'Madagascar') %>% 
        select(sample_id, latin_name, element, unit, form, value, location, catch_source) %>% 
        # convert element units
        mutate(value = ifelse(element %in% c('As', 'Cd', 'Fe', 'Hg','Pb'),
                              value*1000, value),
               unit = ifelse(element %in% c('Vitamin A', 'Se'), 'mug_100g', 'mg_100g'),
               element = recode(element, 
                                'As' = 'arsenic', 'Ca' = 'calcium','Cd' = 'cadmium','Fe' = 'iron','Hg' = 'mercury', 
                                'Pb' = 'lead', 'Se' = 'selenium', 'Vitamin A' = 'vitamin_a1'),
               form = recode(form, 'Dried' = 'Sun-dried'))
    
    return(dat)
}

dat %>% distinct(unit, element)


