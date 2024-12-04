library(tidytext)
cs<-c('NGA', 'CIV', 'SEN', 'UGA', 'MWI', 'TZA')

ssf<-read.csv('data/ihh/ihh_nutrition_lowest.csv') %>% 
    filter(country_iso_alpha3 %in% cs) %>% 
    group_by(country, country_iso_alpha3, subregion, marine_inland_char, species_main, func_grp, grp_pooled) %>% 
    summarise_at(vars(catch_2017:catch_2013), ~sum(.x, na.rm=TRUE)) %>%
    rowwise() %>%
    mutate_at(vars(catch_2017:catch_2013), ~ifelse(.x == 0, NA, .x)) %>%
    mutate(average_catch = mean(c_across(starts_with('catch')), na.rm=TRUE)) %>% 
    filter(!is.na(average_catch)) %>% 
    filter(average_catch > 0) %>% 
    ungroup() %>% 
    select(country, species_main, func_grp, average_catch) %>% mutate(sector = 'Small-scale')

lsf<-read.csv('data/ihh/ihh_fg_lsf_catch_with_nutrients.csv') %>% 
    filter(country_iso_alpha3 %in% cs) %>% 
    mutate(sector = 'Large-scale') %>% 
    select(colnames(ssf))

tot<-rbind(ssf, lsf) %>% group_by(country, species_main, func_grp) %>% 
    summarise(catch = sum(average_catch))

pdf(file = 'fig/ihh_species_catch.pdf', height = 10, width=12)
ggplot(tot %>% filter(catch > 1000), 
       aes(reorder_within(species_main, catch, country), catch)) +
    geom_col() +
    facet_wrap(~country, scales='free') +
    scale_x_reordered() +
    coord_flip() + labs(x= '', y = 'Catch, t')
dev.off()
