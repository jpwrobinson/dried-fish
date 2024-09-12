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
           Scatch_avg = slider::slide_dbl(Scatch, mean, .before = 1, .after = 1),
           id = paste(asfis_species_name, fao_major_fishing_area_name),
           nyears = n_distinct(year[t > 0])) %>% 
    # filter(meaner > 10000) %>% 
    filter(nyears > 20)

plotter %>% filter(year > 2018 & Scatch > 0.9) %>% ungroup() %>% summarise(n_distinct(id), meaner = sum(meaner))
length(unique(plotter$id))
8/17*100
sum(plotter$meaner)

years<-plotter %>% group_by(id, asfis_species_name, fao_major_fishing_area_name) %>% reframe(year = year[which(Scatch==1)]) %>% mutate(Scatch=1)

dep<-years %>% filter(year < 2010) 
plotter %>% ungroup() %>% filter(year %in% c(2017:2022) & id %in% dep$id) %>% summarise(mean(Scatch))

dep<-years %>% filter(year > 2017 & asfis_species_name %in% c('Sardinellas nei', 'Round sardinella')) 
plotter %>% ungroup() %>% filter(id %in% dep$id) %>% summarise(mean(Scatch))

g1<-ggplot(plotter) + 
    geom_line(aes(year, Scatch, col=Scatch, group=asfis_species_name), linewidth=.3) +
    geom_text(data = plotter %>% filter(year==2022) %>% 
                  mutate(lab = paste0(asfis_species_name, ' (', round(meaner/1000, 0), ' kt)')),
              size=2.5, hjust=0, aes(x = 2023, y = Scatch, label = lab)) +
    geom_point(data = years, pch=21, col='black', fill='#cb181d', aes(year, Scatch, group=asfis_species_name)) +
    facet_wrap(~fao_major_fishing_area_name, ncol=1) +
    scale_colour_gradientn(colors = viridis::turbo(n=6), breaks=seq(0, 1, length.out = 6), labels=c(0,'', '', '','', 1), 
                           guide = guide_colourbar(direction = 'horizontal', title.position = 'bottom', label.position = 'top')) +
    labs(y = 'Relative catch', x = '', color='Relative catch') +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0), breaks=seq(0, 1, length.out= 3), labels=c(0, .5, 1)) +
    theme(strip.text.x = element_text(hjust=.5, vjust=1.5),
          plot.margin=unit(c(.1,3,.1,.1), 'cm'),
          legend.position=c(1.05, 0.78)) +
    coord_cartesian(clip='off')
    # scale_discrete_manual("linewidth", values = seq(0.1, 2, length.out = 5))

g2<-ggplot(years, aes(year)) + geom_histogram(binwidth=1) +
    labs(x = 'Year of maximum catch', y = 'N species') +
    scale_x_continuous(breaks=seq(1980, 2020, by = 10), expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    theme(axis.text = element_text(size=8),
          axis.title = element_text(size=8))
    
pdf(file = 'fig/fao_landing.pdf', height=5, width=9)

ggdraw(g1) +
    draw_plot(g2, .1, .75, .25, .25) +
    draw_plot_label(
        c("a", "b"),
        c(0, 0.08),
        c(1, 1),
        size = 12
    )

dev.off()

