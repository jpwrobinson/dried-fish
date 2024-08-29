# FAO STAT extracted from FishStatJ on 29/8/24

# Query is 
# Region: Eastern Central Atlantic & Western Indian Ocean & Africa - Inland Waters
# ISSCAAP: Herrings, Sardines, Anchovies & 
# Rastrineobola argentea & 
# Lake Malawi sardine & Lake Tanganiyka sardine & Lake Tanganiyka sprat

fao<-read.csv('data/faostat_small_pelagic_catch.csv') %>% 
    clean_names() %>% 
    filter(!country_name %in% c('Totals - Tonnes - live weight', 'Totals - Number')) %>% 
    select(-starts_with('s')) %>% 
    pivot_longer(-c(country_name:unit), names_to = 'year', values_to = 'catch_t') %>% 
    mutate(year = as.numeric(str_replace_all(year, 'x_', ''))) %>% 
    filter(!is.na(catch_t)) 


fao %>% group_by(asfis_species_name, fao_major_fishing_area_name, year) %>% 
    summarise(t = sum(catch_t)) %>% 
    ggplot() + geom_area(aes(year, t, fill=asfis_species_name)) +
    facet_wrap(~fao_major_fishing_area_name)
