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
    mutate(meaner = mean(t), 
           resid = t - meaner, 
           Scatch = scales::rescale(t, to = c(0,1)),
           Scatch_avg = slider::slide_dbl(Scatch, mean, .before = 1, .after = 1)) 

g1<-ggplot(plotter %>% filter(meaner > 10000)) + 
    geom_line(aes(year, Scatch, col=Scatch, group=asfis_species_name)) +
    geom_text(data = plotter %>% filter(meaner > 10000 & year==2022),
              size=2, hjust=0,
              aes(x = 2023, y = Scatch, label = asfis_species_name)) +
    facet_wrap(~fao_major_fishing_area_name, ncol=1) +
    scale_colour_gradientn(colors = viridis::turbo(n=6), breaks=seq(0, 1, length.out = 6), labels=c(0,'', '', '','', 1)) +
    labs(y = 'Relative catch', x = '', color='Relative Catch') +
    theme(strip.text.x = element_text(hjust=0),
          legend.position='top') +
    coord_cartesian(clip='off')
    # scale_discrete_manual("linewidth", values = seq(0.1, 2, length.out = 5))
    
pdf(file = 'fig/fao_landing.pdf', height=7, width=12)
print(g1)
dev.off()