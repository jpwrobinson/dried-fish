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
    filter(!is.na(catch_t)) #%>% 
    # mutate(man_group = recode(asfis_species_name,
    #                           'Anchovies nei' = 'Anchovy sp.',
    #                           'European anchovy' = 'Anchovy sp.',
    #                           'Buccaneer anchovy' = 'Anchovy sp.',
    #                           'Stolephorus anchovies nei' = 'Anchovy sp.',
    #                           'Anchovies, etc. nei' = 'Anchovy sp.',
    #                           'Buccaneer anchovy' = 'Sardinenel sp.',
    #                           ))


plotter<-fao %>% group_by(asfis_species_name, fao_major_fishing_area_name, year) %>% 
    summarise(t = sum(catch_t)) %>% 
    group_by(asfis_species_name, fao_major_fishing_area_name) %>% 
    mutate(meaner = mean(t), resid = t - meaner)

ggplot(plotter) + geom_area(aes(year, t, fill=asfis_species_name)) +
    facet_wrap(~fao_major_fishing_area_name)

ggplot(plotter) + 
    geom_line(aes(year, t, col=asfis_species_name)) +
    facet_wrap(~fao_major_fishing_area_name)

ggplot(plotter %>% filter(meaner > 10000)) + 
    geom_line(aes(year, resid, col=asfis_species_name)) +
    facet_wrap(~fao_major_fishing_area_name)
