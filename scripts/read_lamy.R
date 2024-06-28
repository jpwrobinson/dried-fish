# 
# I have deposited the data needed on Zenodo following IRD practices. You will find 3 files:
#     - one csv containing the concentration of our focal elements across 10 samples. I have provided mean, sd and se across 3 replicates. 
# - one csv containing the preliminary (visual) taxonomic ID of each sample. 
# - one RData object that you can load directly into R with both files.
# 
# We have 10 samples:
#     E1-3 composed of small pelagic fishes (Engraulidae)
# C1-3 composed of small pelagic fishes (Clupeidae)
# M1-2 which are a mixed of small coastal fishes (for instance many Gobidae harvested using mosquito trawl nets)
# F1-2 are composed of small freshwater fishes
# 
# F2 is smoked, all other samples are dried. We will provide more details on each sample and I will update the taxonomy based on DNA results very soon.

read_lamy<-function(filepath = 'data/lamy/data_sdf_Mada.RData'){
    
    load(filepath)
    
    metat<-data_taxo_sdf_Mada %>% 
        clean_names() %>% 
        mutate(latin_name = g_hypo) %>% 
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
                              value/10/1000, value),
               value = ifelse(element %in% c('Ca','Se'),
                              value/10, value),
               unit = ifelse(element %in% c('Vitamin A', 'Se'), 'mug_100g', 'mg_100g'),
               element = recode(element, 
                                'As' = 'arsenic', 'Ca' = 'calcium','Cd' = 'cadmium','Fe' = 'iron','Hg' = 'mercury', 
                                'Pb' = 'lead', 'Se' = 'selenium', 'Vitamin A' = 'vitamin_a1'),
               form = ifelse(sample_id=='F2', 'Smoked', 'Sun-dried'))
    
    return(dat)
}

# dat %>% group_by(unit, element) %>% summarise(mean(value))


