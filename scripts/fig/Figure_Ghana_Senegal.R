source('scripts/00_plot.R')

## SAU from https://www.seaaroundus.org/data/#/eez/686?chart=catch-chart&dimension=taxon&measure=tonnage&limit=10

sp<-c('Small pelagics (<30 cm)')

## Ghana + Senegal catches
catch<-rbind(
    read.csv('data/SAU EEZ 288 v50-1/SAU EEZ 288 v50-1.csv'),
    read.csv('data/SAU EEZ 686 v50-1/SAU EEZ 686 v50-1.csv')) %>% 
    filter(catch_type == 'Landings' & functional_group %in% sp & fishing_sector != 'Subsistence') %>% 
    filter(end_use_type=='Direct human consumption') %>% 
    group_by(area_name, area_type, year, fishing_sector, end_use_type) %>% 
    summarise(tonnes = sum(tonnes), landed_value = sum(landed_value))

catch_all<-catch %>% group_by(area_name, area_type, year, end_use_type) %>% 
            summarise(tonnes = sum(tonnes), landed_value = sum(landed_value))


gcat<-ggplot(catch, aes(year, tonnes, colour=fishing_sector)) +
    geom_line() + 
    geom_line(data=catch_all, col='black') + 
    geom_label(data = catch %>% filter(year ==2019 & area_name =='Senegal') %>% mutate(year=2028), 
               aes(label=fishing_sector), size=3) +
    geom_label(data = catch_all %>% filter(year ==2019 & area_name =='Senegal') %>% mutate(year=2028, fishing_sector='Total'), 
               aes(label=fishing_sector), size=3, colour='black') +
    facet_wrap(~area_name) +
    coord_cartesian(clip='off') +
    scale_y_continuous(labels=scales::comma) +
    scale_x_continuous(limits=c(1950, 2030)) +
    labs(x = '', title = 'Reconstructed landings of small pelagic fishes') +
    theme(legend.position ='none',  legend.title=element_blank())

# ggplot(catch %>% filter(end_use_type=='Fishmeal and fish oil'), aes(year, tonnes)) +
#     geom_line() + 
#     facet_wrap(~area_name) +
#     scale_y_continuous(labels=scales::comma) +
#     labs(x = '')


# trade
cts<-c('GHA', 'SEN')
trade<-read.csv('data/trade/20221011_james_robinson_ARTIS_snet.csv') %>% 
    filter(source_country_iso3c %in% cts) %>% 
    filter(nceas_group=='small pelagics') %>% 
    mutate(country = ifelse(source_country_iso3c == 'GHA', 'Ghana', 'Senegal'))

export<-trade %>% filter(dom_source == 'foreign export') %>% 
    group_by(source_country_iso3c, country, year) %>% 
    summarise(live_weight_t = sum(live_weight_t))

gex<-ggplot(export, aes(year, live_weight_t, col=source_country_iso3c)) + geom_line() +
    geom_label(data = export %>% filter(year ==2019) %>% mutate(year=2022), 
               aes(label=country), size=3) +
    scale_y_continuous(labels=scales::comma) +
    scale_x_continuous(limits=c(1995, 2023)) +
    labs(x = '', title = 'Exports of small pelagic fishes', y = 'tonnes') +
    theme(legend.position ='none',  legend.title=element_blank())


pdf(file = 'fig/FigX_small_pelagics_ghana_senegal.pdf', height=3, width=12)
plot_grid(gcat, gex, nrow = 1, labels=c('a', 'b'), rel_widths=c(1, 0.5))
dev.off()
